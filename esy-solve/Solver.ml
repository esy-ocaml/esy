open EsyPackageConfig

module Dependencies = InstallManifest.Dependencies

let computeOverrideDigest sandbox override =
  let open RunAsync.Syntax in
  match override with
  | Override.OfJson {json;} -> return (Digestv.ofJson json)
  | OfDist {dist; json = _;} -> return (Digestv.ofString (Dist.show dist))
  | OfOpamOverride info ->
    let%bind files =
      EsyInstall.Fetch.fetchOverrideFiles
        sandbox.Sandbox.cfg.installCfg
        sandbox.spec override
    in
    let%bind digests = RunAsync.List.mapAndJoin ~f:File.digest files in
    let digest = Digestv.ofJson info.json in
    let digests = digest::digests in
    let digests = List.sort ~cmp:Digestv.compare digests in
    return (List.fold_left ~init:Digestv.empty ~f:Digestv.combine digests)

let computeOverridesDigest sandbox overrides =
  let open RunAsync.Syntax in
  let%bind digests = RunAsync.List.mapAndJoin ~f:(computeOverrideDigest sandbox) overrides in
  return (List.fold_left ~init:Digestv.empty ~f:Digestv.combine digests)

let lock sandbox (pkg : InstallManifest.t) =
  let open RunAsync.Syntax in
  match pkg.source with
  | Install { source = _; opam = Some opam; } ->
    let%bind id =
      let%bind opamDigest = OpamResolution.digest opam in
      let%bind overridesDigest = computeOverridesDigest sandbox pkg.overrides in
      let digest = Digestv.(opamDigest + overridesDigest) in
      return (PackageId.make pkg.name pkg.version (Some digest))
    in
    return (id, pkg)
  | Install { source = _; opam = None; } ->
    let%bind id =
      let%bind digest = computeOverridesDigest sandbox pkg.overrides in
      return (PackageId.make pkg.name pkg.version (Some digest))
    in
    return (id, pkg)
  | Link _ ->
    let id = PackageId.make pkg.name pkg.version None in
    return (id, pkg)

module Strategy = struct
  let trendy = "-count[staleness,solution]"
  (* let minimalAddition = "-removed,-changed,-notuptodate" *)
end

type t = {
  universe : Universe.t;
  solvespec : SolveSpec.t;
  sandbox : Sandbox.t;
}

let evalDependencies solver manifest =
  SolveSpec.eval solver.solvespec solver.sandbox.root manifest

module Reason : sig

  type t

  and chain = {constr : Dependencies.t; trace : trace;}

  and trace = InstallManifest.t list

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

  and trace = InstallManifest.t list

  let conflict left right =
    if compare_chain left right <= 0
    then Conflict (left, right)
    else Conflict (right, left)

  let missing ?(available=[]) chain =
    Missing {chain; available;}

  let ppTrace fmt path =
    let ppPkgName fmt pkg =
      let name = Option.orDefault ~default:pkg.InstallManifest.name pkg.originalName in
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

  let empty : t = []

  let pp fmt reasons =
    let ppReasons fmt reasons =
      let sep = Fmt.unit "@;@;" in
      Fmt.pf fmt "@[<v>%a@;@]" (Fmt.list ~sep Reason.pp) reasons
    in
    Fmt.pf fmt "@[<v>No solution found:@;@;%a@]" ppReasons reasons

  let collectReasons cudfMapping solver reasons =
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
              InstallManifest.Map.add dep pkg map
            in
            List.fold_left ~f ~init:map deplist
          | _ -> map
        in
        let map = InstallManifest.Map.empty in
        List.fold_left ~f ~init:map reasons
      in

      let resolve pkg =
        if pkg.InstallManifest.name = solver.sandbox.root.InstallManifest.name
        then pkg, []
        else
          let rec aux path pkg =
            match InstallManifest.Map.find_opt pkg map with
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

    let maybeEvalDependencies manifest =
      match evalDependencies solver manifest with
      | Ok deps -> deps
      | Error _ -> Dependencies.NpmFormula []
    in

    let%bind reasons =
      let f reasons = function
        | Algo.Diagnostic.Conflict (left, right, _) ->
          let left =
            let pkg = Universe.CudfMapping.decodePkgExn left cudfMapping in
            let requestor, path = resolveReqViaDepChain pkg in
            let constr = Dependencies.filterDependenciesByName
              ~name:pkg.name
              (maybeEvalDependencies requestor)
            in
            {Reason. constr; trace = requestor::path}
          in
          let right =
            let pkg = Universe.CudfMapping.decodePkgExn right cudfMapping in
            let requestor, path = resolveReqViaDepChain pkg in
            let constr = Dependencies.filterDependenciesByName
              ~name:pkg.name
              (maybeEvalDependencies requestor)
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
            if pkg.InstallManifest.name = solver.sandbox.root.InstallManifest.name
            then []
            else pkg::requestor::path
          in
          let f reasons (name, _) =
            let name = Universe.CudfMapping.decodePkgName (Universe.CudfName.make name) in
            let%lwt available =
              match%lwt Resolver.resolve ~name solver.sandbox.resolver with
              | Ok available -> Lwt.return available
              | Error _ -> Lwt.return []
            in
            let constr = Dependencies.filterDependenciesByName ~name (maybeEvalDependencies pkg) in
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

  let explain cudfMapping solver cudf =
    let open RunAsync.Syntax in
    begin match Algo.Depsolver.check_request ~explain:true cudf with
    | Algo.Depsolver.Sat  _
    | Algo.Depsolver.Unsat None
    | Algo.Depsolver.Unsat (Some { result = Algo.Diagnostic.Success _; _ }) ->
      return None
    | Algo.Depsolver.Unsat (Some { result = Algo.Diagnostic.Failure reasons; _ }) ->
      let reasons = reasons () in
      let%bind reasons = collectReasons cudfMapping solver reasons in
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
      | SourceOverride {source;_} -> Version.Source source
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
  (id : PackageId.t)
  (pkg : InstallManifest.t)
  (dependenciesMap : PackageId.t StringMap.t)
  (allDependenciesMap : PackageId.t Version.Map.t StringMap.t)
  =
  let open RunAsync.Syntax in

  let {
    InstallManifest.
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
    |> List.map ~f:(fun req -> StringMap.find req.Req.name dependenciesMap)
    |> List.filterNone
    |> PackageId.Set.of_list
  in

  let optDependencies =
    let f name =
      match StringMap.find name dependenciesMap with
      | Some dep -> Some dep
      | None ->
        begin match StringMap.find name allDependenciesMap with
        | Some versions ->
          let _version, id = Version.Map.find_first (fun _ -> true) versions in
          Some id
        | None -> None
        end
    in
    optDependencies
    |> StringSet.elements
    |> List.map ~f
    |> List.filterNone
    |> PackageId.Set.of_list
  in

  let peerDependencies =
    let f req =
      let versions =
        match StringMap.find req.Req.name allDependenciesMap with
        | Some versions -> versions
        | None -> Version.Map.empty
      in
      let versions = List.rev (Version.Map.bindings versions) in
      let f (version, _id) =
        Resolver.versionMatchesReq resolver req req.Req.name version
      in
      match List.find_opt ~f versions with
      | Some (_version, id) -> Some id
      | None -> None
    in
    peerDependencies
    |> List.map ~f
    |> List.filterNone
    |> PackageId.Set.of_list
  in

  let dependencies =
    let dependencies = idsOfDependencies dependencies in
    dependencies
    |> PackageId.Set.union optDependencies
    |> PackageId.Set.union peerDependencies
  in
  let devDependencies = idsOfDependencies devDependencies in
  let source =
    match source with
    | PackageSource.Link link -> PackageSource.Link link
    | Install {source; opam = None;} ->
      PackageSource.Install {source;opam = None;}
    | Install {source; opam = Some opam;} ->
      PackageSource.Install {source; opam = Some opam;}
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

let make solvespec (sandbox : Sandbox.t) =
  let open RunAsync.Syntax in
  let universe = ref (Universe.empty sandbox.resolver) in
  return {
    solvespec;
    universe = !universe;
    sandbox;
  }

let add ~(dependencies : Dependencies.t) solver =
  let open RunAsync.Syntax in

  let universe = ref solver.universe in
  let report, finish = Cli.createProgressReporter ~name:"resolving esy packages" () in

  let rec addPackage (manifest : InstallManifest.t) =
    if not (Universe.mem ~pkg:manifest !universe)
    then
      match manifest.kind with
      | InstallManifest.Esy ->
        universe := Universe.add ~pkg:manifest !universe;
        let%bind dependencies = RunAsync.ofRun (evalDependencies solver manifest) in
        let%bind () =
          RunAsync.contextf
            (addDependencies dependencies)
            "resolving %a" InstallManifest.pp manifest
        in
        universe := Universe.add ~pkg:manifest !universe;
        return ()
      | InstallManifest.Npm -> return ()
    else return ()

  and addDependencies (dependencies : Dependencies.t) =
    match dependencies with
    | Dependencies.NpmFormula reqs ->
      let f (req : Req.t) = addDependency req in
      RunAsync.List.mapAndWait ~f reqs

    | Dependencies.OpamFormula _ ->
      let f (req : Req.t) = addDependency req in
      let reqs = Dependencies.toApproximateRequests dependencies in
      RunAsync.List.mapAndWait ~f reqs

  and addDependency (req : Req.t) =
    report "%s" req.name;%lwt
    let%bind resolutions =
      RunAsync.contextf (
        Resolver.resolve ~fullMetadata:true ~name:req.name ~spec:req.spec solver.sandbox.resolver
      ) "resolving %a" Req.pp req
    in

    let%bind packages =
      let fetchPackage resolution =
        let%bind pkg =
          RunAsync.contextf
            (Resolver.package ~resolution solver.sandbox.resolver)
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
      let f tasks manifest =
        match manifest with
        | Some manifest -> (addPackage manifest)::tasks
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
      solver.sandbox.cfg.Config.esySolveCmd
      % ("--strategy=" ^ strategy)
      % ("--timeout=" ^ string_of_float(solver.sandbox.cfg.solveTimeout))
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
    InstallManifest.
    name = root.InstallManifest.name;
    version = Version.parseExn "0.0.0";
    originalVersion = None;
    originalName = root.originalName;
    source = PackageSource.Link {
      path = DistPath.v ".";
      manifest = None;
    };
    overrides = Overrides.empty;
    dependencies;
    devDependencies = Dependencies.NpmFormula [];
    peerDependencies = NpmFormula.empty;
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
      |> List.filter ~f:(fun p -> p.InstallManifest.name <> dummyRoot.InstallManifest.name)
      |> InstallManifest.Set.of_list
    in

    return (Ok packages)

  | None ->
    let cudf = preamble, cudfUniverse, request in
    begin match%bind
      Explanation.explain
        cudfMapping
        solver
        cudf
    with
    | Some reasons -> return (Error reasons)
    | None -> return (Error Explanation.empty)
    end

let solveDependenciesNaively
  ~(installed : InstallManifest.Set.t)
  ~(root : InstallManifest.t)
  (dependencies : Dependencies.t)
  (solver : t) =
  let open RunAsync.Syntax in

  let report, finish = Cli.createProgressReporter ~name:"resolving npm packages" () in

  let installed =
    let tbl = Hashtbl.create 100 in
    InstallManifest.Set.iter (fun pkg -> Hashtbl.add tbl pkg.name pkg) installed;
    tbl
  in

  let addToInstalled pkg =
    Hashtbl.replace installed pkg.InstallManifest.name pkg
  in

  let resolveOfInstalled req =

    let rec findFirstMatching = function
      | [] -> None
      | pkg::pkgs ->
        if Resolver.versionMatchesReq
            solver.sandbox.resolver
            req
            pkg.InstallManifest.name
            pkg.InstallManifest.version
        then Some pkg
        else findFirstMatching pkgs
    in

    findFirstMatching (Hashtbl.find_all installed req.name)
  in

  let resolveOfOutside req =
    report "%a" Req.pp req;%lwt
    let%bind resolutions = Resolver.resolve ~name:req.name ~spec:req.spec solver.sandbox.resolver in
    match findResolutionForRequest solver.sandbox.resolver req resolutions with
    | Some resolution ->
      begin match%bind Resolver.package ~resolution solver.sandbox.resolver with
      | Ok pkg -> return (Some pkg)
      | Error reason ->
        errorf "invalid package %a: %s" Resolution.pp resolution reason
      end
    | None -> return None
  in

  let resolve trace (req : Req.t) =
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
    let key pkg = pkg.InstallManifest.name ^ "." ^ (Version.show pkg.InstallManifest.version) in
    let sealDependencies () =
      let f _key (pkg, dependencies) map =
        InstallManifest.Map.add pkg dependencies map
      in
      Hashtbl.fold f solved InstallManifest.Map.empty
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
          let f req = Hashtbl.mem installed req.Req.name in
          List.filter ~f reqs
        in
        reqs
    in

    let%bind pkgs =
      let f req =
        let%bind manifest =
          RunAsync.contextf
            (resolve trace req)
            "resolving request %a" Req.pp req
        in
        addToInstalled manifest;
        return manifest
      in
      reqs
      |> List.map ~f
      |> RunAsync.List.joinAll
    in

    let _, solved =
      let f (seen, solved) manifest =
        if InstallManifest.Set.mem manifest seen
        then seen, solved
        else
          let seen = InstallManifest.Set.add manifest seen in
          let solved = manifest::solved in
          seen, solved
      in
      List.fold_left ~f ~init:(InstallManifest.Set.empty, []) pkgs
    in
    return solved
  in

  let rec loop trace seen = function
    | pkg::rest ->
      begin match InstallManifest.Set.mem pkg seen with
      | true ->
        loop trace seen rest
      | false ->
        let seen = InstallManifest.Set.add pkg seen in
        let%bind dependencies = RunAsync.ofRun (evalDependencies solver pkg) in
        let%bind dependencies =
          RunAsync.contextf
            (solveDependencies (pkg::trace) dependencies)
            "solving dependencies of %a" InstallManifest.pp pkg
        in
        addDependencies pkg dependencies;
        loop trace seen (rest @ dependencies)
      end
    | [] -> return ()
  in

  let%bind () =
    let%bind dependencies = solveDependencies [root] dependencies in
    let%bind () = loop [root] InstallManifest.Set.empty dependencies in
    addDependencies root dependencies;
    return ()
  in

  finish ();%lwt
  return (sealDependencies ())

let solveOCamlReq (req : Req.t) resolver =
  let open RunAsync.Syntax in

  let make resolution =
    Logs_lwt.info (fun m -> m "using %a" Resolution.pp resolution);%lwt
    let%bind pkg = Resolver.package ~resolution resolver in
    let%bind pkg = RunAsync.ofStringError pkg in
    return (pkg.InstallManifest.originalVersion, Some pkg.version)
  in

  match req.spec with
  | VersionSpec.Npm _
  | VersionSpec.NpmDistTag _ ->
    let%bind resolutions = Resolver.resolve ~name:req.name ~spec:req.spec resolver in
    begin match findResolutionForRequest resolver req resolutions with
    | Some resolution -> make resolution
    | None ->
      Logs_lwt.warn (fun m -> m "no version found for %a" Req.pp req);%lwt
      return (None, None)
    end
  | VersionSpec.Opam _ -> error "ocaml version should be either an npm version or source"
  | VersionSpec.Source _ ->
    begin match%bind Resolver.resolve ~name:req.name ~spec:req.spec resolver with
    | [resolution] -> make resolution
    | _ -> errorf "multiple resolutions for %a, expected one" Req.pp req
    end

let solve solvespec (sandbox : Sandbox.t) =
  let open RunAsync.Syntax in

  let getResultOrExplain = function
    | Ok dependencies -> return dependencies
    | Error explanation ->
      errorf "%a" Explanation.pp explanation
  in

  let%bind solver = make solvespec sandbox in

  let%bind dependencies, ocamlVersion =

    let%bind rootDependencies = RunAsync.ofRun (evalDependencies solver sandbox.root) in

    let ocamlReq =
      match rootDependencies with
      | InstallManifest.Dependencies.OpamFormula _ -> None
      | InstallManifest.Dependencies.NpmFormula reqs ->
        NpmFormula.find ~name:"ocaml" reqs
    in

    match ocamlReq with
    | None ->
      return (rootDependencies, None)
    | Some ocamlReq ->
      let%bind (ocamlVersionOrig, ocamlVersion) =
        RunAsync.contextf
          (solveOCamlReq ocamlReq sandbox.resolver)
          "resolving %a" Req.pp ocamlReq
      in

      let%bind dependencies =
        match ocamlVersion, rootDependencies with
        | Some ocamlVersion, InstallManifest.Dependencies.NpmFormula reqs ->
          let ocamlSpec = VersionSpec.ofVersion ocamlVersion in
          let ocamlReq = Req.make ~name:"ocaml" ~spec:ocamlSpec in
          let reqs = NpmFormula.override reqs [ocamlReq] in
          return (InstallManifest.Dependencies.NpmFormula reqs)
        | Some ocamlVersion, InstallManifest.Dependencies.OpamFormula deps ->
          let req =
            match ocamlVersion with
            | Version.Npm v -> InstallManifest.Dep.Npm (SemverVersion.Constraint.EQ v);
            | Version.Source src -> InstallManifest.Dep.Source (SourceSpec.ofSource src)
            | Version.Opam v -> InstallManifest.Dep.Opam (OpamPackageVersion.Constraint.EQ v)
          in
          let ocamlDep = {InstallManifest.Dep. name = "ocaml"; req;} in
          return (InstallManifest.Dependencies.OpamFormula (deps @ [[ocamlDep]]))
        | None, deps -> return deps
      in

      return (dependencies, ocamlVersionOrig)
  in

  let () =
    match ocamlVersion with
    | Some version -> Resolver.setOCamlVersion version sandbox.resolver
    | None -> ()
  in

  let%bind solver, dependencies =
    let%bind solver, dependencies = add ~dependencies solver in
    return (solver, dependencies)
  in

  (* Solve esy dependencies first. *)
  let%bind installed =
    let%bind res =
      solveDependencies
        ~root:sandbox.root
        ~installed:InstallManifest.Set.empty
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
          begin match PackageId.Map.find_opt id packageById with
          | Some _ -> aux acc rest
          | None ->
            let deps =
              match InstallManifest.Map.find_opt pkg dependenciesMap with
              | Some deps -> deps
              | None -> Exn.failf "no dependencies solved found for %a" InstallManifest.pp pkg
            in
            let acc =
              let packageById = PackageId.Map.add id pkg packageById in
              let idByPackage = InstallManifest.Map.add pkg id idByPackage in
              (packageById, idByPackage)
            in
            aux acc (rest @ deps)
          end
        | [] -> return (packageById, idByPackage)
      in

      let packageById = PackageId.Map.empty in
      let idByPackage = InstallManifest.Map.empty in

      aux (packageById, idByPackage) [sandbox.root]
    in

    let dependencies =
      let f pkg id map =
        let dependencies =
          match InstallManifest.Map.find_opt pkg dependenciesMap with
          | Some deps -> deps
          | None -> Exn.failf "no dependencies solved found for %a" InstallManifest.pp pkg
        in
        let dependencies =
          let f deps pkg =
            let id = InstallManifest.Map.find pkg idByPackage in
            StringMap.add pkg.InstallManifest.name id deps
          in
          List.fold_left ~f ~init:StringMap.empty dependencies
        in
        PackageId.Map.add id dependencies map
      in
      InstallManifest.Map.fold f idByPackage PackageId.Map.empty
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
            let version = PackageId.version id in
            Some (Version.Map.add version id Version.Map.empty)
          | Some vs, Some id ->
            let version = PackageId.version id in
            Some (Version.Map.add version id vs)
        in
        StringMap.merge f map deps
      in
      PackageId.Map.fold f dependenciesById StringMap.empty
    in

    let%bind solution =
      let id = InstallManifest.Map.find sandbox.root idByPackage in
      let dependenciesByName =
        PackageId.Map.find id dependenciesById
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
        let pkg = PackageId.Map.find id packageById in
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
      |> PackageId.Map.bindings
      |> RunAsync.List.foldLeft ~f ~init:solution
    in
    return solution
  in

  return solution
