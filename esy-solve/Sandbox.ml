open EsyPackageConfig

type t = {
  cfg : Config.t;
  spec : EsyInstall.SandboxSpec.t;
  root : InstallManifest.t;
  resolutions : Resolutions.t;
  resolver : Resolver.t;
}

let makeResolution source = {
  Resolution.
  name = "root";
  resolution = Version (Version.Source source);
}

let ofResolution cfg spec resolver resolution =
  let open RunAsync.Syntax in
  match%bind Resolver.package ~resolution resolver with
  | Ok root ->
    let root =
      let name =
        match root.InstallManifest.originalName with
        | Some name -> name
        | None -> EsyInstall.SandboxSpec.projectName spec
      in
      {root with name;}
    in

    return {
      cfg;
      spec;
      root;
      resolutions = root.resolutions;
      resolver;
    }
  | Error msg -> errorf "unable to construct sandbox: %s" msg

let make ~cfg (spec : EsyInstall.SandboxSpec.t) =
  let open RunAsync.Syntax in
  let path = DistPath.make ~base:spec.path spec.path in
  let makeSource manifest =
    Source.Link {path; manifest = Some manifest;}
  in
  RunAsync.contextf (
    let%bind resolver = Resolver.make ~cfg ~sandbox:spec () in
    match spec.manifest with
    | EsyInstall.SandboxSpec.Manifest manifest ->
      let source = makeSource manifest in
      let resolution = makeResolution source in
      let%bind sandbox = ofResolution cfg spec resolver resolution in
      Resolver.setResolutions sandbox.resolutions sandbox.resolver;
      return sandbox
    | EsyInstall.SandboxSpec.ManifestAggregate manifests ->
      let%bind resolutions, deps, devDeps =
        let f (resolutions, deps, devDeps) manifest  =
          let source = makeSource manifest in
          let resolution = makeResolution source in
          match%bind Resolver.package ~resolution resolver with
          | Error msg -> errorf "unable to read %a: %s" ManifestSpec.pp manifest msg
          | Ok pkg ->
            let name =
              match ManifestSpec.inferPackageName manifest with
              | None -> failwith "TODO"
              | Some name -> name
            in
            let resolutions =
              let resolution = Resolution.Version (Version.Source source) in
              Resolutions.add name resolution resolutions
            in
            let dep = {InstallManifest.Dep.name; req = Opam OpamPackageVersion.Constraint.ANY;} in
            let deps = [dep]::deps in
            let devDeps =
              match pkg.InstallManifest.devDependencies with
              | InstallManifest.Dependencies.OpamFormula deps -> deps @ devDeps
              | InstallManifest.Dependencies.NpmFormula _ -> devDeps
            in
            return (resolutions, deps, devDeps)
        in
        RunAsync.List.foldLeft ~f ~init:(Resolutions.empty, [], []) manifests
      in
      Resolver.setResolutions resolutions resolver;
      let root = {
        InstallManifest.
        name = Path.basename spec.path;
        version = Version.Source (Dist NoSource);
        originalVersion = None;
        originalName = None;
        source = PackageSource.Install {
          source = NoSource, [];
          opam = None;
        };
        overrides = Overrides.empty;
        dependencies = InstallManifest.Dependencies.OpamFormula deps;
        devDependencies = InstallManifest.Dependencies.OpamFormula devDeps;
        peerDependencies = NpmFormula.empty;
        optDependencies = StringSet.empty;
        resolutions;
        kind = Npm;
      } in
      return {
        cfg;
        spec;
        root;
        resolutions = root.resolutions;
        resolver;
      }
  ) "loading root package metadata"

let digest _solvespec sandbox =
  let open RunAsync.Syntax in

  let ppDependencies fmt deps =

    let ppOpamDependencies fmt deps =
      let ppDisj fmt disj =
        match disj with
        | [] -> Fmt.unit "true" fmt ()
        | [dep] -> InstallManifest.Dep.pp fmt dep
        | deps -> Fmt.pf fmt "(%a)" Fmt.(list ~sep:(unit " || ") InstallManifest.Dep.pp) deps
      in
      Fmt.pf fmt "@[<h>[@;%a@;]@]" Fmt.(list ~sep:(unit " && ") ppDisj) deps
    in

    let ppNpmDependencies fmt deps =
      let ppDnf ppConstr fmt f =
        let ppConj = Fmt.(list ~sep:(unit " && ") ppConstr) in
        Fmt.(list ~sep:(unit " || ") ppConj) fmt f
      in
      let ppVersionSpec fmt spec =
        match spec with
        | VersionSpec.Npm f ->
          ppDnf SemverVersion.Constraint.pp fmt f
        | VersionSpec.NpmDistTag tag ->
          Fmt.string fmt tag
        | VersionSpec.Opam f ->
          ppDnf OpamPackageVersion.Constraint.pp fmt f
        | VersionSpec.Source src ->
          Fmt.pf fmt "%a" SourceSpec.pp src
      in
      let ppReq fmt req =
        Fmt.fmt "%s@%a" fmt req.Req.name ppVersionSpec req.spec
      in
      Fmt.pf fmt "@[<hov>[@;%a@;]@]" (Fmt.list ~sep:(Fmt.unit ", ") ppReq) deps
    in

    match deps with
    | InstallManifest.Dependencies.OpamFormula deps -> ppOpamDependencies fmt deps
    | InstallManifest.Dependencies.NpmFormula deps -> ppNpmDependencies fmt deps
  in

  let showDependencies (deps : InstallManifest.Dependencies.t) =
    Format.asprintf "%a" ppDependencies deps
  in

  let digest =
    Resolutions.digest sandbox.root.resolutions
    |> Digestv.(add (string (showDependencies sandbox.root.dependencies)))
    |> Digestv.(add (string (showDependencies sandbox.root.devDependencies)))
  in

  let%bind digest =
    let f digest resolution =
      let resolution =
        match resolution.Resolution.resolution with
        | SourceOverride {source = Source.Link _; override = _;} -> Some resolution
        | SourceOverride _ -> None
        | Version (Version.Source (Source.Link _)) -> Some resolution
        | Version _ -> None
      in
      match resolution with
      | None -> return digest
      | Some resolution ->
        begin match%bind Resolver.package ~resolution sandbox.resolver with
        | Error _ ->
          errorf "unable to read package: %a" Resolution.pp resolution
        | Ok pkg ->
          return Digestv.(add (string (showDependencies pkg.InstallManifest.dependencies)) digest)
        end
    in
    RunAsync.List.foldLeft
      ~f
      ~init:digest
      (Resolutions.entries sandbox.resolutions)
  in

  return digest
