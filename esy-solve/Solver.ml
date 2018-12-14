module Dependencies = Package.Dependencies
module Resolutions = EsyInstall.PackageConfig.Resolutions
module Resolution = EsyInstall.PackageConfig.Resolution

let computeOverrideDigest sandbox override =
  let open RunAsync.Syntax in
  match override with
  | EsyInstall.Override.OfJson {json;} -> return (Digestv.ofJson json)
  | OfDist {dist; json = _;} -> return (Digestv.ofString (EsyInstall.Dist.show dist))
  | OfOpamOverride info ->
    let%bind files =
      EsyInstall.Override.files
        sandbox.Sandbox.cfg.installCfg
        sandbox.spec override
    in
    let%bind digests = RunAsync.List.mapAndJoin ~f:EsyInstall.File.digest files in
    let digest = Digestv.ofJson info.json in
    let digests = digest::digests in
    let digests = List.sort ~cmp:Digestv.compare digests in
    return (List.fold_left ~init:Digestv.empty ~f:Digestv.combine digests)

let computeOverridesDigest sandbox overrides =
  let open RunAsync.Syntax in
  let%bind digests = RunAsync.List.mapAndJoin ~f:(computeOverrideDigest sandbox) overrides in
  return (List.fold_left ~init:Digestv.empty ~f:Digestv.combine digests)

let lock sandbox (pkg : Package.t) =
  let open RunAsync.Syntax in
  match pkg.source with
  | Install { source = _; opam = Some opam; } ->
    let%bind id =
      let%bind opamDigest = OpamResolution.digest opam in
      let%bind overridesDigest = computeOverridesDigest sandbox pkg.overrides in
      let digest = Digestv.(opamDigest + overridesDigest) in
      return (EsyInstall.PackageId.make pkg.name pkg.version (Some digest))
    in
    return (id, pkg)
  | Install { source = _; opam = None; } ->
    let%bind id =
      let%bind digest = computeOverridesDigest sandbox pkg.overrides in
      return (EsyInstall.PackageId.make pkg.name pkg.version (Some digest))
    in
    return (id, pkg)
  | Link _ ->
    let id = EsyInstall.PackageId.make pkg.name pkg.version None in
    return (id, pkg)

module Strategy = struct
  let trendy = "-count[staleness,solution]"
  (* let minimalAddition = "-removed,-changed,-notuptodate" *)
end

type t = {
  cfg : Config.t;
  resolver : Resolver.t;
  universe : Universe.t;
  resolutions : Resolutions.t;
}

module Reason : sig

  type t

  and chain = {constr : Dependencies.t; trace : trace;}

  and trace = Package.t list

  val pp : t Fmt.t

  val conflict : chain -> chain -> t
  val missing : ?available:Resolution.t list -> chain -> t

  module Set : Set.S with type elt := t

end = struct

  type t =
    | Conflict of chain * chain
    | Missing of {chain : chain; available : Resolution.t list}
    [@@deriving ord]

  and chain = {constr : Dependencies.t; trace : trace;}

  and trace = Package.t list

  let conflict left right =
    if compare_chain left right <= 0
    then Conflict (left, right)
    else Conflict (right, left)

  let missing ?(available=[]) chain =
    Missing {chain; available;}

  let ppTrace fmt path =
    let ppPkgName fmt pkg =
      let name = Option.orDefault ~default:pkg.Package.name pkg.originalName in
      Fmt.string fmt name
    in
    let sep = Fmt.unit " -> " in
    Fmt.(hbox (list ~sep ppPkgName)) fmt (List.rev path)

  let ppChain fmt {constr; trace} =
    match trace with
    | [] -> Fmt.pf fmt "%a" Dependencies.pp constr
    | trace -> Fmt.pf fmt "%a -> %a" ppTrace trace Dependencies.pp constr

  let pp fmt = function
    | Missing {chain; available = [];} ->
      Fmt.pf fmt
        "No package matching:@;@[<v 2>@;%a@;@]"
        ppChain chain
    | Missing {chain; available;} ->
      Fmt.pf fmt
        "No package matching:@;@[<v 2>@;%a@;@;Versions available:@;@[<v 2>@;%a@]@]"
        ppChain chain
        (Fmt.list Resolution.pp) available
    | Conflict (left, right) ->
      Fmt.pf fmt
        "@[<v 2>Conflicting constraints:@;%a@;%a@]"
        ppChain left ppChain right

  module Set = Set.Make(struct
    type nonrec t = t
    let compare = compare
  end)
end

module Explanation = struct

  type t = Reason.t list

  let empty = []

  let pp fmt reasons =
    let ppReasons fmt reasons =
      let sep = Fmt.unit "@;@;" in
      Fmt.pf fmt "@[<v>%a@;@]" (Fmt.list ~sep Reason.pp) reasons
    in
    Fmt.pf fmt "@[<v>No solution found:@;@;%a@]" ppReasons reasons

  let collectReasons ~resolver ~cudfMapping ~root reasons =
    let open RunAsync.Syntax in

    (* Find a pair of requestor, path for the current package.
    * Note that there can be multiple paths in the dependency graph but we only
    * consider one of them.
    *)
    let resolveDepChain pkg =

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
        then pkg, []
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

      resolve pkg
    in

    let resolveReqViaDepChain pkg =
      let requestor, path = resolveDepChain pkg in
      (requestor, path)
    in

    let%bind reasons =
      let f reasons = function
        | Algo.Diagnostic.Conflict (left, right, _) ->
          let left =
            let pkg = Universe.CudfMapping.decodePkgExn left cudfMapping in
            let requestor, path = resolveReqViaDepChain pkg in
            let constr = Package.Dependencies.filterDependenciesByName
              ~name:pkg.name
              requestor.dependencies
            in
            {Reason. constr; trace = requestor::path}
          in
          let right =
            let pkg = Universe.CudfMapping.decodePkgExn right cudfMapping in
            let requestor, path = resolveReqViaDepChain pkg in
            let constr = Package.Dependencies.filterDependenciesByName
              ~name:pkg.name
              requestor.dependencies
            in
            {Reason. constr; trace = requestor::path}
          in
          let conflict = Reason.conflict left right in
          if not (Reason.Set.mem conflict reasons)
          then return (Reason.Set.add conflict reasons)
          else return reasons
        | Algo.Diagnostic.Missing (pkg, vpkglist) ->
          let pkg = Universe.CudfMapping.decodePkgExn pkg cudfMapping in
          let requestor, path = resolveDepChain pkg in
          let trace =
            if pkg.Package.name = root.Package.name
            then []
            else pkg::requestor::path
          in
          let f reasons (name, _) =
            let name = Universe.CudfMapping.decodePkgName (Universe.CudfName.make name) in
            let%lwt available =
              match%lwt Resolver.resolve ~name resolver with
              | Ok available -> Lwt.return available
              | Error _ -> Lwt.return []
            in
            let constr = Dependencies.filterDependenciesByName ~name pkg.dependencies in
            let missing = Reason.missing ~available {constr; trace} in
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

let rec findResolutionForRequest resolver req = function
  | [] -> None
  | res::rest ->
    let version =
      match res.Resolution.resolution with
      | Version version -> version
      | SourceOverride {source;_} -> EsyInstall.Version.Source source
    in
    if
      Resolver.versionMatchesReq
        resolver
        req
        res.Resolution.name
        version
    then Some res
    else findResolutionForRequest resolver req rest

let lockPackage
  resolver
  (id : EsyInstall.PackageId.t)
  (pkg : Package.t)
  (dependenciesMap : EsyInstall.PackageId.t StringMap.t)
  (allDependenciesMap : EsyInstall.PackageId.t EsyInstall.Version.Map.t StringMap.t)
  =
  let open RunAsync.Syntax in

  let {
    Package.
    name;
    version;
    originalVersion = _;
    originalName = _;
    source;
    overrides;
    dependencies;
    devDependencies;
    peerDependencies;
    optDependencies;
    resolutions = _;
    kind = _;
  } = pkg in

  let idsOfDependencies dependencies =
    dependencies
    |> Dependencies.toApproximateRequests
    |> List.map ~f:(fun req -> StringMap.find req.EsyInstall.Req.name dependenciesMap)
    |> List.filterNone
    |> EsyInstall.PackageId.Set.of_list
  in

  let optDependencies =
    let f name =
      match StringMap.find name dependenciesMap with
      | Some dep -> Some dep
      | None ->
        begin match StringMap.find name allDependenciesMap with
        | Some versions ->
          let _version, id = EsyInstall.Version.Map.find_first (fun _ -> true) versions in
          Some id
        | None -> None
        end
    in
    optDependencies
    |> StringSet.elements
    |> List.map ~f
    |> List.filterNone
    |> EsyInstall.PackageId.Set.of_list
  in

  let peerDependencies =
    let f req =
      let versions =
        match StringMap.find req.EsyInstall.Req.name allDependenciesMap with
        | Some versions -> versions
        | None -> EsyInstall.Version.Map.empty
      in
      let versions = List.rev (EsyInstall.Version.Map.bindings versions) in
      let f (version, _id) =
        Resolver.versionMatchesReq resolver req req.EsyInstall.Req.name version
      in
      match List.find_opt ~f versions with
      | Some (_version, id) -> Some id
      | None -> None
    in
    peerDependencies
    |> List.map ~f
    |> List.filterNone
    |> EsyInstall.PackageId.Set.of_list
  in

  let dependencies =
    let dependencies = idsOfDependencies dependencies in
    dependencies
    |> EsyInstall.PackageId.Set.union optDependencies
    |> EsyInstall.PackageId.Set.union peerDependencies
  in
  let devDependencies =
    let devDependencies = idsOfDependencies devDependencies in
    EsyInstall.PackageId.Set.diff devDependencies dependencies
  in
  let source =
    match source with
    | EsyInstall.PackageSource.Link link -> EsyInstall.PackageSource.Link link
    | Install {source; opam = None;} ->
      EsyInstall.PackageSource.Install {source;opam = None;}
    | Install {source; opam = Some opam;} ->
      EsyInstall.PackageSource.Install {source; opam = Some opam;}
  in
  return {
    EsyInstall.Package.
    id;
    name = name;
    version = version;
    source = source;
    overrides = overrides;
    dependencies;
    devDependencies;
  }

let make ~cfg ~resolver ~resolutions () =
  let open RunAsync.Syntax in
  let universe = ref (Universe.empty resolver) in
  return {cfg; resolver; universe = !universe; resolutions}

let add ~(dependencies : Dependencies.t) solver =
  let open RunAsync.Syntax in

  let universe = ref solver.universe in
  let report, finish = Cli.createProgressReporter ~name:"resolving esy packages" () in

  let rec addPackage (pkg : Package.t) =
    if not (Universe.mem ~pkg !universe)
    then
      match pkg.kind with
      | Package.Esy ->
        universe := Universe.add ~pkg !universe;
        let%bind () =
          RunAsync.contextf
            (addDependencies pkg.dependencies)
            "resolving %a" Package.pp pkg
        in
        universe := Universe.add ~pkg !universe;
        return ()
      | Package.Npm -> return ()
    else return ()

  and addDependencies (dependencies : Dependencies.t) =
    match dependencies with
    | Dependencies.NpmFormula reqs ->
      let f (req : EsyInstall.Req.t) = addDependency req in
      RunAsync.List.mapAndWait ~f reqs

    | Dependencies.OpamFormula _ ->
      let f (req : EsyInstall.Req.t) = addDependency req in
      let reqs = Dependencies.toApproximateRequests dependencies in
      RunAsync.List.mapAndWait ~f reqs

  and addDependency (req : EsyInstall.Req.t) =
    report "%s" req.name;%lwt
    let%bind resolutions =
      RunAsync.contextf (
        Resolver.resolve ~fullMetadata:true ~name:req.name ~spec:req.spec solver.resolver
      ) "resolving %a" EsyInstall.Req.pp req
    in

    let%bind packages =
      let fetchPackage resolution =
        let%bind pkg =
          RunAsync.contextf
            (Resolver.package ~resolution solver.resolver)
            "resolving metadata %a" Resolution.pp resolution
        in
        match pkg with
        | Ok pkg -> return (Some pkg)
        | Error reason ->
          Logs_lwt.info (fun m ->
            m "skipping package %a: %s" Resolution.pp resolution reason);%lwt
          return None
      in
      resolutions
      |> List.map ~f:fetchPackage
      |> RunAsync.List.joinAll
    in

    let%bind () =
      let f tasks pkg =
        match pkg with
        | Some pkg -> (addPackage pkg)::tasks
        | None -> tasks
      in
      packages
      |> List.fold_left ~f ~init:[]
      |> RunAsync.List.waitAll
    in

    return ()
  in

  let%bind () = addDependencies dependencies in

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

let solveDependencies ~root ~installed ~strategy dependencies solver =
  let open RunAsync.Syntax in

  let runSolver filenameIn filenameOut =
    let cmd = Cmd.(
      solver.cfg.Config.esySolveCmd
      % ("--strategy=" ^ strategy)
      % ("--timeout=" ^ string_of_float(solver.cfg.solveTimeout))
      % p filenameIn
      % p filenameOut
    ) in

    try%lwt
      let env = ChildProcess.CustomEnv EsyBash.currentEnvWithMingwInPath in
      ChildProcess.run ~env cmd
    with
    | Unix.Unix_error (err, _, _) ->
      let msg = Unix.error_message err in
      RunAsync.error msg
    | _ ->
      RunAsync.error "error running cudf solver"
  in

  let dummyRoot = {
    Package.
    name = root.Package.name;
    version = EsyInstall.Version.parseExn "0.0.0";
    originalVersion = None;
    originalName = root.originalName;
    source = EsyInstall.PackageSource.Link {
      path = EsyInstall.DistPath.v ".";
      manifest = None;
    };
    overrides = EsyInstall.Overrides.empty;
    dependencies;
    devDependencies = Dependencies.NpmFormula [];
    peerDependencies = EsyInstall.PackageConfig.NpmFormula.empty;
    optDependencies = StringSet.empty;
    resolutions = Resolutions.empty;
    kind = Esy;
  } in

  let universe = Universe.add ~pkg:dummyRoot solver.universe in
  let cudfUniverse, cudfMapping = Universe.toCudf ~installed universe in
  let cudfRoot = Universe.CudfMapping.encodePkgExn dummyRoot cudfMapping in

  let request = {
    Cudf.default_request with
    install = [cudfRoot.Cudf.package, Some (`Eq, cudfRoot.Cudf.version)]
  } in

  let preamble =
    {
      Cudf.default_preamble with
      property =
        ("staleness", `Int None)
        ::("original-version", `String None)
        ::Cudf.default_preamble.property
    }
  in

  (* The solution has CRLF on Windows, which breaks the parser *)
  let normalizeSolutionData s = 
      Str.global_replace (Str.regexp_string ("\r\n")) "\n" s 
  in

  let solution =
    let cudf =
      Some preamble, Cudf.get_packages cudfUniverse, request
    in
    Fs.withTempDir (fun path ->
      let%bind filenameIn =
        let filename = Path.(path / "in.cudf") in
        let cudfData = printCudfDoc cudf in
        let%bind () = Fs.writeFile ~data:cudfData filename in
        return filename
      in
      let filenameOut = Path.(path / "out.cudf") in
      let report, finish = Cli.createProgressReporter ~name:"solving esy constraints" () in
      let%lwt () = report "running solver" in
      let%bind () = runSolver filenameIn filenameOut in
      let%lwt () = finish () in
      let%bind result =
        let%bind dataOut = Fs.readFile filenameOut in
        let dataOut = String.trim dataOut in
        if String.length dataOut = 0
        then return None
        else (
          let dataOut = normalizeSolutionData dataOut in
          let solution = parseCudfSolution ~cudfUniverse (dataOut ^ "\n") in
          return (Some solution)
        )
      in
      return result
    )
  in

  match%bind solution with

  | Some (_preamble, cudfUniv) ->

    let packages =
      cudfUniv
      |> Cudf.get_packages ~filter:(fun p -> p.Cudf.installed)
      |> List.map ~f:(fun p -> Universe.CudfMapping.decodePkgExn p cudfMapping)
      |> List.filter ~f:(fun p -> p.Package.name <> dummyRoot.Package.name)
      |> Package.Set.of_list
    in

    return (Ok packages)

  | None ->
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

let solveDependenciesNaively
  ~(installed : Package.Set.t)
  ~(root : Package.t)
  (dependencies : Dependencies.t)
  (solver : t) =
  let open RunAsync.Syntax in

  let report, finish = Cli.createProgressReporter ~name:"resolving npm packages" () in

  let installed =
    let tbl = Hashtbl.create 100 in
    Package.Set.iter (fun pkg -> Hashtbl.add tbl pkg.name pkg) installed;
    tbl
  in

  let addToInstalled pkg =
    Hashtbl.replace installed pkg.Package.name pkg
  in

  let resolveOfInstalled req =

    let rec findFirstMatching = function
      | [] -> None
      | pkg::pkgs ->
        if Resolver.versionMatchesReq
            solver.resolver
            req
            pkg.Package.name
            pkg.Package.version
        then Some pkg
        else findFirstMatching pkgs
    in

    findFirstMatching (Hashtbl.find_all installed req.name)
  in

  let resolveOfOutside req =
    report "%a" EsyInstall.Req.pp req;%lwt
    let%bind resolutions = Resolver.resolve ~name:req.name ~spec:req.spec solver.resolver in
    match findResolutionForRequest solver.resolver req resolutions with
    | Some resolution ->
      begin match%bind Resolver.package ~resolution solver.resolver with
      | Ok pkg -> return (Some pkg)
      | Error reason ->
        errorf "invalid package %a: %s" Resolution.pp resolution reason
      end
    | None -> return None
  in

  let resolve trace (req : EsyInstall.Req.t) =
    let%bind pkg =
      match resolveOfInstalled req with
      | None -> begin match%bind resolveOfOutside req with
        | None ->
          let explanation = [
            Reason.missing {constr = Dependencies.NpmFormula [req]; trace;}
          ] in
          errorf "%a" Explanation.pp explanation
        | Some pkg -> return pkg
        end
      | Some pkg -> return pkg
    in
    return pkg
  in

  let sealDependencies, addDependencies =
    let solved = Hashtbl.create 100 in
    let key pkg = pkg.Package.name ^ "." ^ (EsyInstall.Version.show pkg.Package.version) in
    let sealDependencies () =
      let f _key (pkg, dependencies) map = Package.Map.add pkg dependencies map in
      Hashtbl.fold f solved Package.Map.empty
      (* Hashtbl.find_opt solved (key pkg) *)
    in
    let register pkg dependencies =
      Hashtbl.add solved (key pkg) (pkg, dependencies)
    in
    sealDependencies, register
  in

  let solveDependencies trace dependencies =
    let reqs =
      match dependencies with
      | Dependencies.NpmFormula reqs -> reqs
      | Dependencies.OpamFormula _ ->
        (* only use already installed dependencies here
         * TODO: refactor solution * construction so we don't need to do that *)
        let reqs = Dependencies.toApproximateRequests dependencies in
        let reqs =
          let f req = Hashtbl.mem installed req.EsyInstall.Req.name in
          List.filter ~f reqs
        in
        reqs
    in

    let%bind pkgs =
      let f req =
        let%bind pkg =
          RunAsync.contextf
            (resolve trace req)
            "resolving request %a" EsyInstall.Req.pp req
        in
        addToInstalled pkg;
        return pkg
      in
      reqs
      |> List.map ~f
      |> RunAsync.List.joinAll
    in
    return (Package.Set.elements (Package.Set.of_list pkgs))
  in

  let rec loop trace seen = function
    | pkg::rest ->
      begin match Package.Set.mem pkg seen with
      | true ->
        loop trace seen rest
      | false ->
        let seen = Package.Set.add pkg seen in
        let%bind dependencies =
          RunAsync.contextf
            (solveDependencies (pkg::trace) pkg.dependencies)
            "solving dependencies of %a" Package.pp pkg
        in
        addDependencies pkg dependencies;
        loop trace seen (rest @ dependencies)
      end
    | [] -> return ()
  in

  let%bind () =
    let%bind dependencies = solveDependencies [root] dependencies in
    let%bind () = loop [root] Package.Set.empty dependencies in
    addDependencies root dependencies;
    return ()
  in

  finish ();%lwt
  return (sealDependencies ())

let solveOCamlReq (req : EsyInstall.Req.t) resolver =
  let open RunAsync.Syntax in

  let make resolution =
    Logs_lwt.info (fun m -> m "using %a" Resolution.pp resolution);%lwt
    let%bind pkg = Resolver.package ~resolution resolver in
    let%bind pkg = RunAsync.ofStringError pkg in
    return (pkg.Package.originalVersion, Some pkg.version)
  in

  match req.spec with
  | EsyInstall.VersionSpec.Npm _
  | EsyInstall.VersionSpec.NpmDistTag _ ->
    let%bind resolutions = Resolver.resolve ~name:req.name ~spec:req.spec resolver in
    begin match findResolutionForRequest resolver req resolutions with
    | Some resolution -> make resolution
    | None ->
      Logs_lwt.warn (fun m -> m "no version found for %a" EsyInstall.Req.pp req);%lwt
      return (None, None)
    end
  | EsyInstall.VersionSpec.Opam _ -> error "ocaml version should be either an npm version or source"
  | EsyInstall.VersionSpec.Source _ ->
    begin match%bind Resolver.resolve ~name:req.name ~spec:req.spec resolver with
    | [resolution] -> make resolution
    | _ -> errorf "multiple resolutions for %a, expected one" EsyInstall.Req.pp req
    end

let solve (sandbox : Sandbox.t) =
  let open RunAsync.Syntax in

  let getResultOrExplain = function
    | Ok dependencies -> return dependencies
    | Error explanation ->
      errorf "%a" Explanation.pp explanation
  in

  let%bind dependencies, ocamlVersion =
    match sandbox.ocamlReq with
    | None -> return (sandbox.dependencies, None)
    | Some ocamlReq ->
      let%bind (ocamlVersionOrig, ocamlVersion) =
        RunAsync.contextf
          (solveOCamlReq ocamlReq sandbox.resolver)
          "resolving %a" EsyInstall.Req.pp ocamlReq
      in

      let dependencies =
        match ocamlVersion, sandbox.dependencies with
        | Some ocamlVersion, Package.Dependencies.NpmFormula reqs ->
          let ocamlSpec = EsyInstall.VersionSpec.ofVersion ocamlVersion in
          let ocamlReq = EsyInstall.Req.make ~name:"ocaml" ~spec:ocamlSpec in
          let reqs = EsyInstall.PackageConfig.NpmFormula.override reqs [ocamlReq] in
          Package.Dependencies.NpmFormula reqs
        | Some ocamlVersion, Package.Dependencies.OpamFormula deps ->
          let req =
            match ocamlVersion with
            | EsyInstall.Version.Npm v -> Package.Dep.Npm (EsyInstall.SemverVersion.Constraint.EQ v);
            | EsyInstall.Version.Source src -> Package.Dep.Source (EsyInstall.SourceSpec.ofSource src)
            | EsyInstall.Version.Opam v -> Package.Dep.Opam (EsyInstall.OpamPackageVersion.Constraint.EQ v)
          in
          let ocamlDep = {Package.Dep. name = "ocaml"; req;} in
          Package.Dependencies.OpamFormula (deps @ [[ocamlDep]])
        | None, deps -> deps
      in

      return (dependencies, ocamlVersionOrig)
  in

  let () =
    match ocamlVersion with
    | Some version -> Resolver.setOCamlVersion version sandbox.resolver
    | None -> ()
  in

  let%bind solver, dependencies =
    let%bind solver = make
      ~resolver:sandbox.resolver
      ~cfg:sandbox.cfg
      ~resolutions:sandbox.resolutions
      ()
    in
    let%bind solver, dependencies = add ~dependencies solver in
    return (solver, dependencies)
  in

  (* Solve esy dependencies first. *)
  let%bind installed =
    let%bind res =
      solveDependencies
        ~root:sandbox.root
        ~installed:Package.Set.empty
        ~strategy:Strategy.trendy
        dependencies
        solver
    in
    getResultOrExplain res
  in

  (* Solve npm dependencies now. *)
  let%bind dependenciesMap =
    solveDependenciesNaively
      ~installed
      ~root:sandbox.root
      dependencies
      solver
  in

  let%bind packageById, idByPackage, dependenciesById =
    let%bind packageById, idByPackage =
      let rec aux (packageById, idByPackage as acc) = function
        | pkg::rest ->
          let%bind id, pkg = lock sandbox pkg in
          begin match EsyInstall.PackageId.Map.find_opt id packageById with
          | Some _ -> aux acc rest
          | None ->
            let deps =
              match Package.Map.find_opt pkg dependenciesMap with
              | Some deps -> deps
              | None -> Exn.failf "no dependencies solved found for %a" Package.pp pkg
            in
            let acc =
              let packageById = EsyInstall.PackageId.Map.add id pkg packageById in
              let idByPackage = Package.Map.add pkg id idByPackage in
              (packageById, idByPackage)
            in
            aux acc (rest @ deps)
          end
        | [] -> return (packageById, idByPackage)
      in

      let packageById = EsyInstall.PackageId.Map.empty in
      let idByPackage = Package.Map.empty in

      aux (packageById, idByPackage) [sandbox.root]
    in

    let dependencies =
      let f pkg id map =
        let dependencies =
          match Package.Map.find_opt pkg dependenciesMap with
          | Some deps -> deps
          | None -> Exn.failf "no dependencies solved found for %a" Package.pp pkg
        in
        let dependencies =
          let f deps pkg =
            let id = Package.Map.find pkg idByPackage in
            StringMap.add pkg.Package.name id deps
          in
          List.fold_left ~f ~init:StringMap.empty dependencies
        in
        EsyInstall.PackageId.Map.add id dependencies map
      in
      Package.Map.fold f idByPackage EsyInstall.PackageId.Map.empty
    in

    return (packageById, idByPackage, dependencies)
  in

  let%bind solution =

    let allDependenciesByName =
      let f _id deps map =
        let f _key a b =
          match a, b with
          | None, None -> None
          | Some vs, None -> Some vs
          | None, Some id ->
            let version = EsyInstall.PackageId.version id in
            Some (EsyInstall.Version.Map.add version id EsyInstall.Version.Map.empty)
          | Some vs, Some id ->
            let version = EsyInstall.PackageId.version id in
            Some (EsyInstall.Version.Map.add version id vs)
        in
        StringMap.merge f map deps
      in
      EsyInstall.PackageId.Map.fold f dependenciesById StringMap.empty
    in

    let%bind solution =
      let id = Package.Map.find sandbox.root idByPackage in
      let dependenciesByName =
        EsyInstall.PackageId.Map.find id dependenciesById
      in
      let%bind root =
        lockPackage
          sandbox.resolver
          id
          sandbox.root
          dependenciesByName
          allDependenciesByName
      in
      return (
        EsyInstall.Solution.empty root.EsyInstall.Package.id
        |> EsyInstall.Solution.add root
      )
    in

    let%bind solution =
      let f solution (id, dependencies) =
        let pkg = EsyInstall.PackageId.Map.find id packageById in
        let%bind pkg =
          lockPackage
            sandbox.resolver
            id
            pkg
            dependencies
            allDependenciesByName
        in
        return (EsyInstall.Solution.add pkg solution)
      in
      dependenciesById
      |> EsyInstall.PackageId.Map.bindings
      |> RunAsync.List.foldLeft ~f ~init:solution
    in
    return solution
  in

  return solution
