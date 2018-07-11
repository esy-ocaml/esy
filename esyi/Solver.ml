module Source = Package.Source
module SourceSpec = Package.SourceSpec
module Version = Package.Version
module VersionSpec = Package.VersionSpec
module Dependencies = Package.Dependencies
module NpmDependencies = Package.NpmDependencies
module Req = Package.Req
module Resolutions = Package.Resolutions
module DepFormula = Package.DepFormula

module Strategy = struct
  let trendy = "-removed,-notuptodate,-new"
  (* let minimalAddition = "-removed,-changed,-notuptodate" *)
end

type t = {
  cfg : Config.t;
  resolver : Resolver.t;
  universe : Universe.t;
  resolutions : Resolutions.t;
}

module Explanation = struct

  module Reason = struct

    type t =
      | Conflict of {left : chain; right : chain}
      | Missing of {name : string; path : chain; available : Resolver.Resolution.t list}
      [@@deriving ord]

    and chain =
      Package.t list

    let ppChain fmt path =
      let ppPkgName fmt pkg = Fmt.string fmt pkg.Package.name in
      let sep = Fmt.unit " -> " in
      Fmt.pf fmt "%a" Fmt.(list ~sep ppPkgName) (List.rev path)

    let pp fmt = function
      | Missing {name; path; available;} ->
        Fmt.pf fmt
          "No packages matching:@;@[<v 2>@;%s (required by %a)@;@;Versions available:@;@[<v 2>@;%a@]@]"
          name
          ppChain path
          (Fmt.list Resolver.Resolution.pp) available
      | Conflict {left; right;} ->
        Fmt.pf fmt
          "@[<v 2>Conflicting dependencies:@;@%a@;%a@]"
          ppChain left ppChain right

    module Set = Set.Make(struct
      type nonrec t = t
      let compare = compare
    end)
  end

  type t = Reason.t list

  let empty = []

  let pp fmt reasons =
    let sep = Fmt.unit "@;@;" in
    Fmt.pf fmt "@[<v>%a@;@]" (Fmt.list ~sep Reason.pp) reasons

  let collectReasons ~resolver ~cudfMapping ~root reasons =
    let open RunAsync.Syntax in

    (* Find a pair of requestor, path for the current package.
    * Note that there can be multiple paths in the dependency graph but we only
    * consider one of them.
    *)
    let resolveDepChain =

      let map =
        let f map = function
          | Algo.Diagnostic.Dependency (pkg, _, _) when pkg.Cudf.package = "dose-dummy-request" -> map
          | Algo.Diagnostic.Dependency (pkg, _, deplist) ->
            let pkg = Universe.CudfMapping.decodePkgExn pkg cudfMapping in
            let f map dep =
              let dep = Universe.CudfMapping.decodePkgExn dep cudfMapping in
              Package.Map.add dep pkg map
            in
            List.fold_left ~f ~init:map deplist
          | _ -> map
        in
        let map = Package.Map.empty in
        List.fold_left ~f ~init:map reasons
      in

      let resolve pkg =
        if pkg.Package.name = root.Package.name
        then failwith "inconsistent state: root package was not expected"
        else
          let rec aux path pkg =
            match Package.Map.find_opt pkg map with
            | None -> pkg::path
            | Some npkg -> aux (pkg::path) npkg
          in
          match List.rev (aux [] pkg) with
          | []
          | _::[] -> failwith "inconsistent state: empty dep path"
          | _::requestor::path -> (requestor, path)
      in

      resolve
    in

    let resolveReqViaDepChain pkg =
      let requestor, path = resolveDepChain pkg in
      (requestor, path)
    in

    let%bind reasons =
      let f reasons = function
        | Algo.Diagnostic.Conflict (pkga, pkgb, _) ->
          let pkga = Universe.CudfMapping.decodePkgExn pkga cudfMapping in
          let pkgb = Universe.CudfMapping.decodePkgExn pkgb cudfMapping in
          let requestora, patha = resolveReqViaDepChain pkga in
          let requestorb, pathb = resolveReqViaDepChain pkgb in
          let conflict = Reason.Conflict {left = requestora::patha; right = requestorb::pathb} in
          if not (Reason.Set.mem conflict reasons)
          then return (Reason.Set.add conflict reasons)
          else return reasons
        | Algo.Diagnostic.Missing (pkg, vpkglist) ->
          let pkg = Universe.CudfMapping.decodePkgExn pkg cudfMapping in
          let path =
            if pkg.Package.name = root.Package.name
            then []
            else
              let requestor, path = resolveDepChain pkg in
              requestor::path
          in
          let f reasons (name, _) =
            let name = Universe.CudfMapping.decodePkgName name in
            let%lwt available =
              match%lwt Resolver.resolve ~name resolver with
              | Ok (available, _) -> Lwt.return available
              | Error _ -> Lwt.return []
            in
            let missing = Reason.Missing {name; path = pkg::path; available} in
            if not (Reason.Set.mem missing reasons)
            then return (Reason.Set.add missing reasons)
            else return reasons
          in
          RunAsync.List.foldLeft ~f ~init:reasons vpkglist
        | _ -> return reasons
      in
      RunAsync.List.foldLeft ~f ~init:Reason.Set.empty reasons
    in

    return (Reason.Set.elements reasons)

  let explain ~resolver ~cudfMapping ~root cudf =
    let open RunAsync.Syntax in
    begin match Algo.Depsolver.check_request ~explain:true cudf with
    | Algo.Depsolver.Sat  _
    | Algo.Depsolver.Unsat None
    | Algo.Depsolver.Unsat (Some { result = Algo.Diagnostic.Success _; _ }) ->
      return None
    | Algo.Depsolver.Unsat (Some { result = Algo.Diagnostic.Failure reasons; _ }) ->
      let reasons = reasons () in
      let%bind reasons = collectReasons ~resolver ~cudfMapping ~root reasons in
      return (Some reasons)
    | Algo.Depsolver.Error err -> error err
    end

end

let rec findResolutionForRequest ~req = function
  | [] -> None
  | res::rest ->
    if
      Req.matches
        ~name:res.Resolver.Resolution.name
        ~version:res.Resolver.Resolution.version
        req
    then Some res
    else findResolutionForRequest ~req rest

let solutionRecordOfPkg ~solver (pkg : Package.t) =
  let open RunAsync.Syntax in
  let%bind source =
    match pkg.source with
    | Package.Source source -> return source
    | Package.SourceSpec sourceSpec ->
      Resolver.resolveSource ~name:pkg.name ~sourceSpec solver.resolver
  in

  let%bind files =
    match pkg.opam with
    | Some opam -> opam.files ()
    | None -> return []
  in

  let opam =
    match pkg.opam with
    | Some opam -> Some {
        Solution.Record.Opam.
        name = opam.name;
        version = opam.version;
        opam = opam.opam;
        override =
          if Package.OpamOverride.equal opam.override Package.OpamOverride.empty
          then None
          else Some opam.override;
      }
    | None -> None
  in

  return {
    Solution.Record.
    name = pkg.name;
    version = pkg.version;
    source;
    files;
    opam;
  }

let make ~cfg ?resolver ~resolutions () =
  let open RunAsync.Syntax in
  let%bind resolver =
    match resolver with
    | None -> Resolver.make ~cfg ()
    | Some resolver -> return resolver
  in
  let universe = ref Universe.empty in
  return {cfg; resolver; universe = !universe; resolutions}

let add ~(dependencies : Dependencies.t) solver =
  let open RunAsync.Syntax in

  let universe = ref solver.universe in
  let report, finish = solver.cfg.Config.createProgressReporter ~name:"resolving" () in

  let rec addPackage (pkg : Package.t) =
    if not (Universe.mem ~pkg !universe)
    then
      match pkg.kind with
      | Package.Esy ->
        universe := Universe.add ~pkg !universe;
        let%bind dependencies = addDependencies pkg.dependencies in
        universe := (
          let pkg = {pkg with dependencies} in
          Universe.add ~pkg !universe
        );
        return ()
      | Package.Npm -> return ()
    else return ()

  and addDependencies (dependencies : Dependencies.t) =
    let dependencies =
      Dependencies.applyResolutions solver.resolutions dependencies
    in

    match dependencies with
    | Dependencies.NpmFormula reqs ->
      let%bind reqs = RunAsync.List.joinAll (
        let f (req : Req.t) = addDependency req in
        List.map ~f reqs
      ) in
      return (Dependencies.NpmFormula reqs)

    | Dependencies.OpamFormula formula ->
      let%bind rewrites =
        let f rewrites (req : Req.t) =
          let%bind nextReq = addDependency req in
          match req.spec, nextReq.Req.spec with
          | VersionSpec.Source prev, VersionSpec.Source next ->
            return (SourceSpec.Map.add prev next rewrites)
          | _ -> return rewrites
        in
        let reqs = Dependencies.toApproximateRequests dependencies in
        RunAsync.List.foldLeft ~f ~init:SourceSpec.Map.empty reqs
      in

      let formula =
        let f (dep : Package.Dep.t) =
          match dep.req with
          | Package.Dep.Source src -> begin
            match SourceSpec.Map.find_opt src rewrites with
            | Some nextSrc -> {dep with req = Package.Dep.Source nextSrc}
            | None -> dep
            end
          | _ -> dep
        in
        List.map ~f:(List.map ~f) formula
      in

      return (Dependencies.OpamFormula formula)

  and addDependency (req : Req.t) =
    let%lwt () =
      let status = Format.asprintf "%s" req.name in
      report status
    in
    let%bind resolutions, spec =
      Resolver.resolve ~name:req.name ~spec:req.spec solver.resolver
      |> RunAsync.withContext (Format.asprintf "resolving %a" Req.pp req)
    in

    let%bind packages =
      let fetchPackage resolution =
        Resolver.package ~resolution solver.resolver
        |> RunAsync.withContext (
            Format.asprintf "resolving metadata %a" Resolver.Resolution.pp resolution)
      in
      resolutions
      |> List.map ~f:fetchPackage
      |> RunAsync.List.joinAll
    in

    let%bind () =
      packages
      |> List.map ~f:addPackage
      |> RunAsync.List.waitAll
    in

    return (
      match spec with
      | Some spec -> Req.ofSpec ~name:req.name ~spec
      | None -> req
    )
  in

  let%bind dependencies = addDependencies dependencies in

  let%lwt () = finish () in

  (* TODO: return rewritten deps *)
  return ({solver with universe = !universe}, dependencies)

let printCudfDoc doc =
  let o = IO.output_string () in
  Cudf_printer.pp_io_doc o doc;
  IO.close_out o

let parseCudfSolution ~cudfUniverse data =
  let i = IO.input_string data in
  let p = Cudf_parser.from_IO_in_channel i in
  let solution = Cudf_parser.load_solution p cudfUniverse in
  IO.close_in i;
  solution

let solveDependencies ~installed ~strategy dependencies solver =
  let open RunAsync.Syntax in

  let dummyRoot = {
    Package.
    name = "ROOT";
    version = Version.parseExn "0.0.0";
    source = Package.Source Source.NoSource;
    opam = None;
    dependencies;
    devDependencies = Dependencies.NpmFormula [];
    kind = Esy;
  } in

  let universe = Universe.add ~pkg:dummyRoot solver.universe in
  let cudfUniverse, cudfMapping = Universe.toCudf ~installed universe in
  let cudfRoot = Universe.CudfMapping.encodePkgExn dummyRoot cudfMapping in

  let request = {
    Cudf.default_request with
    install = [cudfRoot.Cudf.package, Some (`Eq, cudfRoot.Cudf.version)]
  } in
  let preamble = Cudf.default_preamble in

  let solution =
    let cudf =
      Some preamble, Cudf.get_packages cudfUniverse, request
    in
    let dataIn = printCudfDoc cudf in
    let%bind dataOut = Fs.withTempFile ~data:dataIn (fun filename ->
      let cmd = Cmd.(
        solver.cfg.Config.esySolveCmd
        % ("--strategy=" ^ strategy)
        % ("--timeout=" ^ string_of_float(solver.cfg.solveTimeout))
        % p filename) in
      ChildProcess.runOut cmd
    ) in
    return (parseCudfSolution ~cudfUniverse dataOut)
  in

  match%lwt solution with

  | Error _ ->
    let cudf = preamble, cudfUniverse, request in
    begin match%bind
      Explanation.explain
        ~resolver:solver.resolver
        ~cudfMapping
        ~root:dummyRoot
        cudf
    with
    | Some reasons -> return (Error reasons)
    | None -> return (Error Explanation.empty)
    end

  | Ok (_preamble, cudfUniv) ->

    let packages =
      cudfUniv
      |> Cudf.get_packages ~filter:(fun p -> p.Cudf.installed)
      |> List.map ~f:(fun p -> Universe.CudfMapping.decodePkgExn p cudfMapping)
      |> List.filter ~f:(fun p -> p.Package.name <> dummyRoot.Package.name)
      |> Package.Set.of_list
    in

    return (Ok packages)

let solveDependenciesNaively
  ~(installed : Package.Set.t)
  (dependencies : Dependencies.t)
  (solver : t) =
  let open RunAsync.Syntax in

  let report, finish = solver.cfg.Config.createProgressReporter ~name:"resolving" () in

  let installed =
    let tbl = Hashtbl.create 100 in
    Package.Set.iter (fun pkg -> Hashtbl.add tbl pkg.name pkg) installed;
    tbl
  in

  let addToInstalled pkg =
    Hashtbl.add installed pkg.Package.name pkg
  in

  let resolveOfInstalled req =

    let rec findFirstMatching = function
      | [] -> None
      | pkg::pkgs ->
        if Req.matches
          ~name:pkg.Package.name
          ~version:pkg.Package.version
          req
        then Some pkg
        else findFirstMatching pkgs
    in

    findFirstMatching (Hashtbl.find_all installed req.name)
  in

  let resolveOfOutside req =
    let%lwt () =
      let status = Format.asprintf "%a" Req.pp req in
      report status
    in
    let%bind resolutions, _ = Resolver.resolve ~name:req.name ~spec:req.spec solver.resolver in
    match findResolutionForRequest ~req resolutions with
    | Some resolution ->
      let%bind pkg = Resolver.package ~resolution solver.resolver in
      return (Some pkg)
    | None -> return None
  in

  let resolve (req : Req.t) =
    let%bind pkg =
      match resolveOfInstalled req with
      | None -> begin match%bind resolveOfOutside req with
        | None ->
          let msg = Format.asprintf "unable to find a match for %a" Req.pp req in
          error msg
        | Some pkg -> return pkg
        end
      | Some pkg -> return pkg
    in
    return pkg
  in

  let rec solveDependencies ~seen dependencies =

    let reqs = match dependencies with
    | Dependencies.NpmFormula reqs -> reqs
    | Dependencies.OpamFormula _ ->
      (* TODO: cause opam formulas should be solved by the proper dependency
       * solver we skip solving them, but we need some sanity check here *)
      Dependencies.toApproximateRequests dependencies
    in

    (** This prefetches resolutions which can result in an overfetch but makes
     * things happen much faster. *)
    let%bind _ =
      let f (req : Req.t) = Resolver.resolve ~name:req.name ~spec:req.spec solver.resolver in
      reqs
      |> List.map ~f
      |> RunAsync.List.joinAll
    in

    let f roots (req : Req.t) =
      if StringSet.mem req.name seen
      then return roots
      else begin
        let seen = StringSet.add req.name seen in
        let%bind pkg = resolve req in
        addToInstalled pkg;
        let%bind dependencies = solveDependencies ~seen pkg.Package.dependencies in
        let%bind record = solutionRecordOfPkg ~solver pkg in
        let root = Solution.make record dependencies in
        return (root::roots)
      end
    in
    RunAsync.List.foldLeft ~f ~init:[] reqs
  in

  let%bind roots = solveDependencies ~seen:StringSet.empty dependencies in
  finish ();%lwt
  return roots

let solveOCamlReq ~cfg ~opamRegistry req =
  let open RunAsync.Syntax in
  let%bind resolver = Resolver.make ~opamRegistry ~cfg () in
  let%bind resolutions, _ = Resolver.resolve ~name:req.name ~spec:req.spec resolver in
  begin match findResolutionForRequest ~req resolutions with
  | Some res ->
    Logs_lwt.app (fun m -> m "using %a" Resolver.Resolution.pp res);%lwt
    return (Some res.version)
  | None ->
    Logs_lwt.warn (fun m -> m "no version found for %a" Req.pp req);%lwt
    return None
  end

let solve ~cfg ~resolutions (root : Package.t) =
  let open RunAsync.Syntax in

  let getResultOrExplain = function
    | Ok dependencies -> return dependencies
    | Error explanation ->
      let msg = Format.asprintf
        "@[<v>No solution found:@;@;%a@]"
        Explanation.pp explanation
      in
      error msg
  in

  let reqs, ocamlReq =
    match root.dependencies, root.devDependencies with
    | Dependencies.NpmFormula reqs, Dependencies.NpmFormula devReqs ->
      (* we override dependencies with devDependencies for the root project *)
      let reqs = NpmDependencies.override reqs devReqs in
      let ocamlReq = NpmDependencies.find ~name:"ocaml" reqs in
      reqs, ocamlReq
    | _ -> failwith "only npm formulas are supported for the root manifest"
  in

  let%bind opamRegistry = OpamRegistry.init ~cfg () in

  let%bind ocamlVersion, reqs =
    match ocamlReq with
    | Some req ->
      let%bind version = solveOCamlReq ~cfg ~opamRegistry req in
      let reqs =
        match version with
        | Some version ->
          let ocamlReq = Req.ofSpec ~name:req.name ~spec:(VersionSpec.ofVersion version) in
          NpmDependencies.override reqs [ocamlReq]
        | None -> reqs
      in
      return (version, reqs)
    | None ->
      Logs_lwt.app (fun m -> m "no ocaml constraint defined");%lwt
      return (None, reqs)
  in

  let dependencies = Dependencies.NpmFormula reqs in

  let%bind solver, dependencies =
    let%bind resolver  = Resolver.make ?ocamlVersion ~opamRegistry ~cfg () in
    let%bind solver = make ~resolver ~cfg ~resolutions () in
    let%bind solver, dependencies = add ~dependencies solver in
    return (solver, dependencies)
  in

  (* Solve runtime dependencies first *)
  let%bind installed =
    let%bind res =
      solveDependencies
        ~installed:Package.Set.empty
        ~strategy:Strategy.trendy
        dependencies
        solver
    in getResultOrExplain res
  in

  let%bind dependencies =
    solveDependenciesNaively
      ~installed
      dependencies
      solver
  in

  let%bind solution =
    let%bind record = solutionRecordOfPkg ~solver root in
    return (Solution.make record dependencies)
  in

  return solution
