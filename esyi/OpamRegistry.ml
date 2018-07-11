module Source = Package.Source
module SourceSpec = Package.SourceSpec
module String = Astring.String
module Override = Package.OpamOverride

module OpamPathsByVersion = Memoize.Make(struct
  type key = OpamPackage.Name.t
  type value = Path.t OpamPackage.Version.Map.t RunAsync.t
end)

module OpamFiles = Memoize.Make(struct
  type key = OpamPackage.Name.t * OpamPackage.Version.t
  type value = OpamFile.OPAM.t RunAsync.t
end)

type t = {
  repoPath : Path.t;
  overrides : OpamOverrides.t;
  pathsCache : OpamPathsByVersion.t;
  opamCache : OpamFiles.t;
}

type resolution = {
  name: OpamPackage.Name.t;
  version: OpamPackage.Version.t;
  opam: Path.t;
  url: Path.t option;
}

let packagePath ~name ~version registry =
  let name = OpamPackage.Name.to_string name in
  let version = OpamPackage.Version.to_string version in
  Path.(
    registry.repoPath
    / "packages"
    / name
    / (name ^ "." ^ version)
  )

let readOpamFile ~name ~version registry =
  let open RunAsync.Syntax in
  let compute (name, version) =
    let path = Path.(packagePath ~name ~version registry / "opam") in
    let%bind data = Fs.readFile path in
    return (OpamFile.OPAM.read_from_string data)
  in
  OpamFiles.compute registry.opamCache (name, version) compute

let readOpamFiles (path : Path.t) () =
  let open RunAsync.Syntax in
  let filesPath = Path.(path / "files") in
  if%bind Fs.isDir filesPath
  then
    let collect files filePath _fileStats =
      match Path.relativize ~root:filesPath filePath with
      | Some name ->
        let%bind content = Fs.readFile filePath in
        return ({Package.File. name; content}::files)
      | None -> return files
    in
    Fs.fold ~init:[] ~f:collect filesPath
  else return []

module Manifest = struct
  type t = {
    name: OpamPackage.Name.t;
    version: OpamPackage.Version.t;
    path : Path.t;
    opam: OpamFile.OPAM.t;
    url: OpamFile.URL.t option;
    override : Override.t;
  }

  let ofFile ~name ~version ?url registry =
    let open RunAsync.Syntax in
    let path = packagePath ~name ~version registry in
    let%bind opam = readOpamFile ~name ~version registry in
    let%bind url =
      match url with
      | Some url ->
        let%bind data = Fs.readFile url in
        return (Some (OpamFile.URL.read_from_string data))
      | None -> return None
    in
    return {name; version; opam; url; path; override = Override.empty;}

  let toPackage ~name ~version {name = opamName; version = opamVersion; opam; url; path; override} =
    let open RunAsync.Syntax in
    let context = Format.asprintf "processing %a opam package" Path.pp path in
    RunAsync.withContext context (

      let%bind source =
        match override.Override.opam.Override.Opam.source with
        | Some source ->
          return (Package.Source (Package.Source.Archive (source.url, source.checksum)))
        | None -> begin
          match url with
          | Some url ->
            let {OpamUrl. backend; path; hash; _} = OpamFile.URL.url url in
            begin match backend, hash with
            | `http, Some hash ->
              return (Package.Source (Package.Source.Archive (path, hash)))
            | `http, None ->
              (* TODO: what to do here? fail or resolve? *)
              return (Package.SourceSpec (Package.SourceSpec.Archive (path, None)))
            | `rsync, _ -> error "unsupported source for opam: rsync"
            | `hg, _ -> error "unsupported source for opam: hg"
            | `darcs, _ -> error "unsupported source for opam: darcs"
            | `git, ref ->
              return (Package.SourceSpec (Package.SourceSpec.Git {remote = path; ref}))
            end
          | None -> return (Package.Source Package.Source.NoSource)
          end
      in

      let translateFormula f =
        let translateAtom ((name, relop) : OpamFormula.atom) =
          let module C = OpamVersion.Constraint in
          let name = "@opam/" ^ OpamPackage.Name.to_string name in
          let req =
            match relop with
            | None -> C.ANY
            | Some (`Eq, v) -> C.EQ v
            | Some (`Neq, v) -> C.NEQ v
            | Some (`Lt, v) -> C.LT v
            | Some (`Gt, v) -> C.GT v
            | Some (`Leq, v) -> C.LTE v
            | Some (`Geq, v) -> C.GTE v
          in {Package.Dep. name; req = Opam req}
        in
        let cnf = OpamFormula.to_cnf f in
        List.map ~f:(List.map ~f:translateAtom) cnf
      in

      let translateFilteredFormula ~build ~post ~test ~doc ~dev f =
        let%bind f =
          let env var =
            match OpamVariable.Full.to_string var with
            | "test" -> Some (OpamVariable.B test)
            | _ -> None
          in
          let f = OpamFilter.partial_filter_formula env f in
          try return (OpamFilter.filter_deps ~build ~post ~test ~doc ~dev f)
          with Failure msg -> error msg
        in
        return (translateFormula f)
      in

      let%bind dependencies =
        let%bind formula =
          RunAsync.withContext "processing depends field" (
            translateFilteredFormula
              ~build:true ~post:true ~test:false ~doc:false ~dev:false
              (OpamFile.OPAM.depends opam)
          )
        in
        let formula =
          formula
          @ [
              [{
                Package.Dep.
                name = "@esy-ocaml/esy-installer";
                req = Npm SemverVersion.Constraint.ANY;
              }];
              [{
                Package.Dep.
                name = "@esy-ocaml/substs";
                req = Npm SemverVersion.Constraint.ANY;
              }];
            ]
          @ Package.NpmDependencies.toOpamFormula override.Package.OpamOverride.dependencies
          @ Package.NpmDependencies.toOpamFormula override.Package.OpamOverride.peerDependencies
        in return (Package.Dependencies.OpamFormula formula)
      in

      let%bind devDependencies =
        RunAsync.withContext "processing depends field" (
          let%bind formula =
            translateFilteredFormula
              ~build:true ~post:true ~test:false ~doc:false ~dev:false
              (OpamFile.OPAM.depends opam)
          in return (Package.Dependencies.OpamFormula formula)
        )
      in

      let readOpamFilesForPackage path () =
        let%bind files = readOpamFiles path () in
        return (files @ override.Override.opam.files)
      in

      return {
        Package.
        name;
        version;
        kind = Package.Esy;
        source;
        opam = Some {
          Package.Opam.
          name = opamName;
          version = opamVersion;
          files = readOpamFilesForPackage path;
          opam = opam;
          override = {override with opam = Override.Opam.empty};
        };
        dependencies;
        devDependencies;
      }
    )
end

let init ~cfg () =
  let open RunAsync.Syntax in
  let%bind repoPath =
    match cfg.Config.opamRepository with
    | Config.Local local -> return local
    | Config.Remote (remote, local) ->
      Logs_lwt.app (fun m -> m "checking %s for updates..." remote);%lwt
      let%bind () = Git.ShallowClone.update ~branch:"master" ~dst:local remote in
      return local
  in

  let%bind overrides = OpamOverrides.init ~cfg () in

  return {
    repoPath;
    pathsCache = OpamPathsByVersion.make ();
    opamCache = OpamFiles.make ();
    overrides;
  }

let getVersionIndex registry ~(name : OpamPackage.Name.t) =
  let f name =
    let open RunAsync.Syntax in
    let path = Path.(
      registry.repoPath
      / "packages"
      / OpamPackage.Name.to_string name
    ) in
    let%bind entries = Fs.listDir path in
    let f index entry =
      let version = match String.cut ~sep:"." entry with
        | None -> OpamPackage.Version.of_string ""
        | Some (_name, version) -> OpamPackage.Version.of_string version
      in
      OpamPackage.Version.Map.add version Path.(path / entry) index
    in
    return (List.fold_left ~init:OpamPackage.Version.Map.empty ~f entries)
  in
  OpamPathsByVersion.compute registry.pathsCache name f

let getPackage
  ?ocamlVersion
  ~(name : OpamPackage.Name.t)
  ~(version : OpamPackage.Version.t)
  registry
  =
  let open RunAsync.Syntax in
  let%bind index = getVersionIndex registry ~name in
  match OpamPackage.Version.Map.find_opt version index with
  | None -> return None
  | Some packagePath ->
    let opam = Path.(packagePath / "opam") in
    let%bind url =
      let url = Path.(packagePath / "url") in
      if%bind Fs.exists url
      then return (Some url)
      else return None
    in

    let%bind available =
      let env (var : OpamVariable.Full.t) =
        let scope = OpamVariable.Full.scope var in
        let name = OpamVariable.Full.variable var in
        let v =
          let open Option.Syntax in
          let open OpamVariable in
          match scope, OpamVariable.to_string name with
          | OpamVariable.Full.Global, "preinstalled" ->
            return (bool false)
          | OpamVariable.Full.Global, "compiler"
          | OpamVariable.Full.Global, "ocaml-version" ->
            let%bind ocamlVersion = ocamlVersion in
            return (string (OpamPackage.Version.to_string ocamlVersion))
          | OpamVariable.Full.Global, _ -> None
          | OpamVariable.Full.Self, _ -> None
          | OpamVariable.Full.Package _, _ -> None
        in v
      in
      let%bind opam = readOpamFile ~name ~version registry in
      let formula = OpamFile.OPAM.available opam in
      let available = OpamFilter.eval_to_bool ~default:true env formula in
      return available
    in

    if available
    then return (Some { name; opam; url; version })
    else return None

let versions ?ocamlVersion ~(name : OpamPackage.Name.t) registry =
  let open RunAsync.Syntax in
  let%bind index = getVersionIndex registry ~name in
  let queue = LwtTaskQueue.create ~concurrency:2 () in
  let%bind resolutions =
    let getPackageVersion version () =
      getPackage ?ocamlVersion ~name ~version registry
    in
    index
    |> OpamPackage.Version.Map.bindings
    |> List.map ~f:(fun (version, _path) -> LwtTaskQueue.submit queue (getPackageVersion version))
    |> RunAsync.List.joinAll
  in
  return (List.filterNone resolutions)

let version ~(name : OpamPackage.Name.t) ~version registry =
  let open RunAsync.Syntax in
  match%bind getPackage registry ~name ~version with
  | None -> return None
  | Some { opam = _; url; name; version } ->
    let%bind pkg = Manifest.ofFile ~name ~version ?url registry in
    begin match%bind OpamOverrides.find ~name ~version registry.overrides with
    | None -> return (Some pkg)
    | Some override -> return (Some {pkg with Manifest. override})
    end
