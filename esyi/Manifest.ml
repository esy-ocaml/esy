module Version = SemverVersion.Version
module String = Astring.String
module Resolutions = Package.Resolutions
module Source = Package.Source
module Req = Package.Req
module Dep = Package.Dep
module NpmDependencies = Package.NpmDependencies
module Dependencies = Package.Dependencies

let find (path : Path.t) =
  let open RunAsync.Syntax in
  let esyJson = Path.(path / "esy.json") in
  let packageJson = Path.(path / "package.json") in
  if%bind Fs.exists esyJson
  then return esyJson
  else if%bind Fs.exists packageJson
  then return packageJson
  else error "no package.json found"

(* This is used just to read the Json.t *)
module PackageJson = struct
  type t = {
    name : string;
    version : string;
    dependencies : (NpmDependencies.t [@default NpmDependencies.empty]);
    devDependencies : (NpmDependencies.t [@default NpmDependencies.empty]);
    dist : (dist option [@default None]);
    esy : (Json.t option [@default None]);
  } [@@deriving of_yojson { strict = false }]

  and dist = {
    tarball : string;
    shasum : string;
  }

  let ofFile (path : Path.t) =
    let open RunAsync.Syntax in
    let%bind data = Fs.readJsonFile path in
    let%bind pkgJson = RunAsync.ofRun (Json.parseJsonWith of_yojson data) in
    return pkgJson
end

type t = {
  name : string;
  version : string;
  dependencies : NpmDependencies.t;
  devDependencies : NpmDependencies.t;
  source : Source.t;
  hasEsyManifest : bool;
}

type manifest = t

let name manifest = manifest.name
let version manifest = Version.parseExn manifest.version

let ofPackageJson ?(source=Source.NoSource) (pkgJson : PackageJson.t) = {
  name = pkgJson.name;
  version = pkgJson.version;
  dependencies = pkgJson.dependencies;
  devDependencies = pkgJson.devDependencies;
  hasEsyManifest = Option.isSome pkgJson.esy;
  source =
    match pkgJson.dist with
    | Some dist -> Source.Archive (dist.PackageJson.tarball, dist.PackageJson.shasum)
    | None -> source;
}

let of_yojson json =
  let open Result.Syntax in
  let%bind pkgJson = PackageJson.of_yojson json in
  return (ofPackageJson pkgJson)

let ofDir (path : Path.t) =
  let open RunAsync.Syntax in
  let%bind filename = find path in
  let%bind json = Fs.readJsonFile filename in
  let%bind pkgJson = RunAsync.ofRun (Json.parseJsonWith PackageJson.of_yojson json) in
  return (ofPackageJson pkgJson)

module Root = struct
  type t = {
    manifest : manifest;
    resolutions : Resolutions.t;
  }

  module ParseResolutions = struct
    type t = {
      resolutions : (Package.Resolutions.t [@default Package.Resolutions.empty]);
    } [@@deriving of_yojson { strict = false }]
  end

  let ofDir (path : Path.t) =
    let open RunAsync.Syntax in
    let%bind filename = find path in
    let%bind json = Fs.readJsonFile filename in
    let%bind pkgJson = RunAsync.ofRun (Json.parseJsonWith PackageJson.of_yojson json) in
    let%bind resolutions = RunAsync.ofRun (Json.parseJsonWith ParseResolutions.of_yojson json) in
    let manifest = ofPackageJson pkgJson in
    return {manifest; resolutions = resolutions.ParseResolutions.resolutions}
end

let toPackage ?name ?version (manifest : t) =
  let open RunAsync.Syntax in
  let name =
    match name with
    | Some name -> name
    | None -> manifest.name
  in
  let version =
    match version with
    | Some version -> version
    | None -> Package.Version.Npm (SemverVersion.Version.parseExn manifest.version)
  in
  let source =
    match version with
    | Package.Version.Source src -> Package.Source src
    | _ -> Package.Source manifest.source
  in

  let pkg = {
    Package.
    name;
    version;
    dependencies = Dependencies.NpmFormula manifest.dependencies;
    devDependencies = Dependencies.NpmFormula manifest.devDependencies;
    source;
    opam = None;
    kind =
      if manifest.hasEsyManifest
      then Esy
      else Npm
  } in

  return pkg

