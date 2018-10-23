module EsyPackageJson = struct
  type t = {
    _dependenciesForNewEsyInstaller : Package.NpmFormula.t option [@default None];
  } [@@deriving of_yojson { strict = false }]
end

module Manifest = struct
  type t = {
    name : string option [@default None];
    version : SemverVersion.Version.t option [@default None];
    dependencies : Package.NpmFormula.t [@default Package.NpmFormula.empty];
    optDependencies : Json.t StringMap.t [@default StringMap.empty];
    esy : EsyPackageJson.t option [@default None];
    dist : dist option [@default None]
  } [@@deriving of_yojson { strict = false }]

  and dist = {
    tarball : string;
    shasum : string;
  }

end

module ResolutionsOfManifest = struct
  type t = {
    resolutions : (Package.Resolutions.t [@default Package.Resolutions.empty]);
  } [@@deriving of_yojson { strict = false }]
end

module DevDependenciesOfManifest = struct
  type t = {
    devDependencies : Package.NpmFormula.t [@default Package.NpmFormula.empty];
  } [@@deriving of_yojson { strict = false }]
end

let rebaseDependencies source reqs =
  let open Run.Syntax in
  let f req =
    match source, req.Req.spec with
    | (Source.Dist LocalPath {path = basePath; _}
      | Source.Link {path = basePath; _}),
      VersionSpec.Source (SourceSpec.LocalPath {path; manifest;}) ->
      let path = Path.(basePath // path |> normalizeAndRemoveEmptySeg) in
      let spec = VersionSpec.Source (SourceSpec.LocalPath {path; manifest;}) in
      return (Req.make ~name:req.name ~spec)
    | (Source.Dist LocalPath {path = basePath; _}
      | Source.Link {path = basePath; _}),
      VersionSpec.Source (SourceSpec.LocalPathLink {path; manifest;}) ->
      let path = Path.(basePath // path |> normalizeAndRemoveEmptySeg) in
      let spec = VersionSpec.Source (SourceSpec.LocalPathLink {path; manifest;}) in
      return (Req.make ~name:req.name ~spec)
    | _, VersionSpec.Source (SourceSpec.LocalPath _)
    | _, VersionSpec.Source (SourceSpec.LocalPathLink _) ->
      errorf
        "path constraints %a are not allowed from %a"
        VersionSpec.pp req.spec Source.pp source
    | _ -> return req
  in
  Result.List.map ~f reqs

let packageOfJson
  ?(parseResolutions=false)
  ?(parseDevDependencies=false)
  ?source
  ~name
  ~version
  json =
  let open Run.Syntax in
  let%bind pkgJson = Json.parseJsonWith Manifest.of_yojson json in
  let originalVersion =
    match pkgJson.Manifest.version with
    | Some version -> Some (Version.Npm version)
    | None -> None
  in

  let%bind source =
    match source, pkgJson.dist with
    | Some source, _ -> return source
    | None, Some dist ->
      return (Source.Dist (Archive {
        url = dist.tarball;
        checksum = Checksum.Sha1, dist.shasum;
      }))
    | None, None ->
      error "unable to determine package source, missing 'dist' metadata"
  in


  let dependencies =
    match pkgJson.esy with
    | None
    | Some {EsyPackageJson. _dependenciesForNewEsyInstaller= None} ->
      pkgJson.dependencies
    | Some {EsyPackageJson. _dependenciesForNewEsyInstaller= Some dependencies} ->
      dependencies
  in

  let%bind dependencies = rebaseDependencies source dependencies in

  let%bind devDependencies =
    match parseDevDependencies with
    | false -> return Package.NpmFormula.empty
    | true ->
      let%bind {DevDependenciesOfManifest. devDependencies} =
        Json.parseJsonWith DevDependenciesOfManifest.of_yojson json
      in
      let%bind devDependencies = rebaseDependencies source devDependencies in
      return devDependencies
  in

  let%bind resolutions =
    match parseResolutions with
    | false -> return Package.Resolutions.empty
    | true ->
      let%bind {ResolutionsOfManifest. resolutions} =
        Json.parseJsonWith ResolutionsOfManifest.of_yojson json
      in
      return resolutions
  in

  let source =
    match source with
    | Source.Link {path; manifest;} ->
      Package.Link {
        path;
        manifest;
        overrides = Package.Overrides.empty;
      }
    | _ ->
      Package.Install {
        source = source, [];
        overrides = Package.Overrides.empty;
        opam = None;
      }
  in

  return {
    Package.
    name;
    version;
    originalVersion;
    originalName = pkgJson.name;
    dependencies = Package.Dependencies.NpmFormula dependencies;
    devDependencies = Package.Dependencies.NpmFormula devDependencies;
    optDependencies = pkgJson.optDependencies |> StringMap.keys |> StringSet.of_list;
    resolutions;
    source;
    kind = if Option.isSome pkgJson.esy then Esy else Npm;
  }
