
module EsyPackageJson = struct
  type t = {
    _dependenciesForNewEsyInstaller : Package.NpmFormula.t option [@default None];
  } [@@deriving of_yojson { strict = false }]
end

type t = {
  name : string option [@default None];
  version : SemverVersion.Version.t option [@default None];
  dependencies : Package.NpmFormula.t [@default Package.NpmFormula.empty];
  devDependencies : Package.NpmFormula.t [@default Package.NpmFormula.empty];
  esy : EsyPackageJson.t option [@default None];
} [@@deriving of_yojson { strict = false }]

let findInDir (path : Path.t) =
  let open RunAsync.Syntax in
  let esyJson = Path.(path / "esy.json") in
  let packageJson = Path.(path / "package.json") in
  if%bind Fs.exists esyJson
  then return (Some esyJson)
  else if%bind Fs.exists packageJson
  then return (Some packageJson)
  else return None

let ofFile path =
  let open RunAsync.Syntax in
  let%bind json = Fs.readJsonFile path in
  RunAsync.ofRun (Json.parseJsonWith of_yojson json)

let ofDir path =
  let open RunAsync.Syntax in
  match%bind findInDir path with
  | Some filename ->
    let%bind json = Fs.readJsonFile filename in
    RunAsync.ofRun (Json.parseJsonWith of_yojson json)
  | None -> error "no package.json (or esy.json) found"

let toPackage ~name ~version ~source (pkgJson : t) =
  let originalVersion =
    match pkgJson.version with
    | Some version -> Some (Version.Npm version)
    | None -> None
  in
  let dependencies =
    match pkgJson.esy with
    | None
    | Some {EsyPackageJson. _dependenciesForNewEsyInstaller= None} ->
      pkgJson.dependencies
    | Some {EsyPackageJson. _dependenciesForNewEsyInstaller= Some dependencies} ->
      dependencies
  in
  {
    Package.
    name;
    version;
    originalVersion;
    dependencies = Package.Dependencies.NpmFormula dependencies;
    devDependencies = Package.Dependencies.NpmFormula pkgJson.devDependencies;
    resolutions = Package.Resolutions.empty;
    source = source, [];
    overrides = Package.Overrides.empty;
    opam = None;
    kind = if Option.isSome pkgJson.esy then Esy else Npm;
  }

