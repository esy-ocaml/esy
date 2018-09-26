module Resolutions = Package.Resolutions
module Resolution = Package.Resolution

module PackageCache = Memoize.Make(struct
  type key = (string * Resolution.resolution)
  type value = (Package.t, string) result RunAsync.t
end)

module SourceCache = Memoize.Make(struct
  type key = SourceSpec.t
  type value = Source.t RunAsync.t
end)

module ResolutionCache = Memoize.Make(struct
  type key = string
  type value = Resolution.t list RunAsync.t
end)

module PackageOverride = struct
  type t = {
    source : Source.t;
    override : Package.Override.t;
  } [@@deriving of_yojson]

end

let rebaseSource ~(base : Source.t) (source : Source.t) =
  let open Run.Syntax in
  match source, base with
  | LocalPathLink _, _ -> error "link is not supported at manifest overrides"
  | LocalPath info, LocalPath {path = basePath; _}
  | LocalPath info, LocalPathLink {path = basePath; _} ->
    let path = Path.(basePath // info.path |> normalizeAndRemoveEmptySeg) in
    return (Source.LocalPath {info with path;})
  | LocalPath _, _ -> failwith "not implemented"
  | source, _ -> return source

type resolutionInProgress =
  | Package of Package.t
  | PackageOverride of PackageOverride.t

let opamname name =
  let open RunAsync.Syntax in
  match Astring.String.cut ~sep:"@opam/" name with
  | Some ("", name) -> return (OpamPackage.Name.of_string name)
  | _ -> errorf "invalid opam package name: %s" name

let toOpamOcamlVersion version =
  match version with
  | Some (Version.Npm { major; minor; patch; _ }) ->
    let minor =
      if minor < 10
      then "0" ^ (string_of_int minor)
      else string_of_int minor
    in
    let patch =
      if patch < 1000
      then patch
      else patch / 1000
    in
    let v = Printf.sprintf "%i.%s.%i" major minor patch in
    let v =
      match OpamPackageVersion.Version.parse v with
      | Ok v -> v
      | Error msg -> failwith msg
    in
    Some v
  | Some (Version.Opam v) -> Some v
  | Some (Version.Source _) -> None
  | None -> None

let makeDummyPackage name version source =
  {
    Package.
    name;
    version;
    originalVersion = None;
    source = source, [];
    overrides = Package.Overrides.empty;
    dependencies = Package.Dependencies.NpmFormula [];
    devDependencies = Package.Dependencies.NpmFormula [];
    resolutions = Package.Resolutions.empty;
    opam = None;
    kind = Esy;
  }

let loadPackageOfGithub ?manifest ~allowEmptyPackage ~name ~version ~source ~user ~repo ?(ref="master") () =
  let open RunAsync.Syntax in
  let fetchFile name =
    let url =
      Printf.sprintf
        "https://raw.githubusercontent.com/%s/%s/%s/%s"
        user repo ref name
    in
    Curl.get url
  in

  let rec tryFilename filenames =
    match filenames with
    | [] ->
      if allowEmptyPackage
      then return (Package (makeDummyPackage name version source))
      else errorf "cannot find manifest at github:%s/%s#%s" user repo ref
    | (kind, fname)::rest ->
      begin match%lwt fetchFile fname with
      | Error _ -> tryFilename rest
      | Ok data ->
        begin match kind with
        | ManifestSpec.Filename.Esy ->
          RunAsync.ofRun (
            let open Run.Syntax in
            let%bind json = Json.parse data in
            let%bind pkg =
              PackageJson.packageOfJson
                ~parseResolutions:true
                ~name
                ~version
                ~source
                json
            in
            return (Package pkg)
          )
        | ManifestSpec.Filename.Opam ->
          let opamname =
            match ManifestSpec.Filename.inferPackageName (kind, fname) with
            | None -> repo
            | Some name -> name
          in
          let%bind manifest =
            let version = OpamPackage.Version.of_string "dev" in
            let name = OpamPackage.Name.of_string opamname in
            RunAsync.ofRun (OpamManifest.ofString ~name ~version data)
          in
          begin match%bind OpamManifest.toPackage ~name ~version ~source manifest with
          | Ok pkg -> return (Package pkg)
          | Error err -> error err
          end
        end
      end
  in

  let filenames =
    match manifest with
    | Some manifest -> [manifest]
    | None -> [
      ManifestSpec.Filename.Esy, "esy.json";
      ManifestSpec.Filename.Esy, "package.json"
    ]
  in

  tryFilename filenames

let loadPackageOfPath ?manifest ~allowEmptyPackage ~name ~version ~source (path : Path.t) =
  let open RunAsync.Syntax in

  let rec tryFilename filenames =
    match filenames with
    | [] ->
      if allowEmptyPackage
      then return (Package (makeDummyPackage name version source))
      else errorf "cannot find manifest at %a" Path.pp path
    | (kind, fname)::rest ->
      let path = Path.(path / fname) in
      if%bind Fs.exists path
      then
        match kind with
        | ManifestSpec.Filename.Esy ->
          let%bind json = Fs.readJsonFile path in
          begin match PackageOverride.of_yojson json with
          | Ok override ->
            return (PackageOverride override)
          | Error _ ->
            RunAsync.ofRun (
              let open Run.Syntax in
              let%bind pkg =
                PackageJson.packageOfJson
                  ~parseResolutions:true
                  ~name
                  ~version
                  ~source
                  json
              in
              return (Package pkg)
            )
          end
        | ManifestSpec.Filename.Opam ->
          let opamname =
            match ManifestSpec.Filename.inferPackageName (kind, fname) with
            | None -> Path.(basename (parent path))
            | Some name -> name
          in
          let%bind manifest =
            let version = OpamPackage.Version.of_string "dev" in
            let name = OpamPackage.Name.of_string opamname in
            OpamManifest.ofPath ~name ~version path
          in
          begin match%bind OpamManifest.toPackage ~name ~version ~source manifest with
          | Ok pkg -> return (Package pkg)
          | Error err -> error err
          end
      else
        tryFilename rest
  in
  let filenames =
    match manifest with
    | Some manifest -> [manifest]
    | None -> [
      ManifestSpec.Filename.Esy, "esy.json";
      ManifestSpec.Filename.Esy, "package.json";
      ManifestSpec.Filename.Opam, "opam";
      ManifestSpec.Filename.Opam, (Path.basename path ^ ".opam");
    ]
  in
  tryFilename filenames

type t = {
  cfg: Config.t;
  pkgCache: PackageCache.t;
  srcCache: SourceCache.t;
  opamRegistry : OpamRegistry.t;
  npmRegistry : NpmRegistry.t;
  mutable ocamlVersion : Version.t option;
  mutable resolutions : Package.Resolutions.t;
  resolutionCache : ResolutionCache.t;

  npmDistTags : (string, SemverVersion.Version.t StringMap.t) Hashtbl.t;
  sourceSpecToSource : (SourceSpec.t, Source.t) Hashtbl.t;
  sourceToSource : (Source.t, Source.t) Hashtbl.t;
}

let make ~cfg () =
  RunAsync.return {
    cfg;
    pkgCache = PackageCache.make ();
    srcCache = SourceCache.make ();
    opamRegistry = OpamRegistry.make ~cfg ();
    npmRegistry = NpmRegistry.make ~url:cfg.Config.npmRegistry ();
    ocamlVersion = None;
    resolutions = Package.Resolutions.empty;
    resolutionCache = ResolutionCache.make ();
    npmDistTags = Hashtbl.create 500;
    sourceSpecToSource = Hashtbl.create 500;
    sourceToSource = Hashtbl.create 500;
  }

let setOCamlVersion ocamlVersion resolver =
  resolver.ocamlVersion <- Some ocamlVersion

let setResolutions resolutions resolver =
  resolver.resolutions <- resolutions

let sourceMatchesSpec resolver spec source =
  match Hashtbl.find_opt resolver.sourceSpecToSource spec with
  | Some resolvedSource ->
    if Source.compare resolvedSource source = 0
    then true
    else
      begin match Hashtbl.find_opt resolver.sourceToSource resolvedSource with
      | Some resolvedSource -> Source.compare resolvedSource source = 0
      | None -> false
      end
  | None -> false

let versionMatchesReq (resolver : t) (req : Req.t) name (version : Version.t) =
  let checkVersion () =
    match req.spec, version with

    | (VersionSpec.Npm spec, Version.Npm version) ->
      SemverVersion.Formula.DNF.matches ~version spec

    | (VersionSpec.NpmDistTag tag, Version.Npm version) ->
      begin match Hashtbl.find_opt resolver.npmDistTags req.name with
      | Some tags ->
        begin match StringMap.find_opt tag tags with
        | None -> false
        | Some taggedVersion ->
          SemverVersion.Version.compare version taggedVersion = 0
        end
      | None -> false
      end

    | (VersionSpec.Opam spec, Version.Opam version) ->
      OpamPackageVersion.Formula.DNF.matches ~version spec

    | (VersionSpec.Source spec, Version.Source source) ->
      sourceMatchesSpec resolver spec source

    | (VersionSpec.Npm _, _) -> false
    | (VersionSpec.NpmDistTag _, _) -> false
    | (VersionSpec.Opam _, _) -> false
    | (VersionSpec.Source _, _) -> false
  in
  let checkResolutions () =
    match Resolutions.find resolver.resolutions req.name with
    | Some _ -> true
    | None -> false
  in
  req.name = name && (checkResolutions () || checkVersion ())

let versionMatchesDep (resolver : t) (dep : Package.Dep.t) name (version : Version.t) =
  let checkVersion () =
    match version, dep.Package.Dep.req with

    | Version.Npm version, Npm spec ->
      SemverVersion.Constraint.matches ~version spec

    | Version.Opam version, Opam spec ->
      OpamPackageVersion.Constraint.matches ~version spec

    | Version.Source source, Source spec ->
      sourceMatchesSpec resolver spec source

    | Version.Npm _, _ -> false
    | Version.Opam _, _ -> false
    | Version.Source _, _ -> false
  in
  let checkResolutions () =
    match Resolutions.find resolver.resolutions dep.name with
    | Some _ -> true
    | None -> false
  in
  dep.name = name && (checkResolutions () || checkVersion ())

let packageOfSource ~allowEmptyPackage ~name ~overrides (source : Source.t) resolver =
  let open RunAsync.Syntax in

  let resolve' ~allowEmptyPackage (source : Source.t) =
    Logs_lwt.debug (fun m -> m "fetching metadata %a" Source.pp source);%lwt
    match source with
    | LocalPath {path; manifest}
    | LocalPathLink {path; manifest} ->
      let%bind pkg = loadPackageOfPath
        ?manifest
        ~name
        ~version:(Version.Source source)
        ~allowEmptyPackage
        ~source
        path
      in
      return pkg
    | Git {remote; commit; manifest;} ->
      Fs.withTempDir begin fun repo ->
        let%bind () = Git.clone ~dst:repo ~remote () in
        let%bind () = Git.checkout ~ref:commit ~repo () in
        loadPackageOfPath
          ?manifest
          ~name
          ~version:(Version.Source source)
          ~allowEmptyPackage
          ~source
          repo
      end
    | Github {user; repo; commit; manifest;} ->
      loadPackageOfGithub
        ?manifest
        ~name
        ~version:(Version.Source source)
        ~allowEmptyPackage
        ~source
        ~user
        ~repo
        ~ref:commit
        ()

    | Archive _ ->
      Fs.withTempDir begin fun path ->
        let%bind () =
          SourceStorage.fetchAndUnpack
            ~cfg:resolver.cfg
            ~dst:path
            source
        in
        loadPackageOfPath
          ~name
          ~version:(Version.Source source)
          ~allowEmptyPackage
          ~source
          path
      end

    | NoSource ->
      return (Package (makeDummyPackage name (Version.Source source) source))
  in

  let rec loop' ~allowEmptyPackage ~overrides source =
    match%bind resolve' ~allowEmptyPackage source with
    | Package pkg ->
      let pkg = {
        pkg with
        Package.overrides = Package.Overrides.addMany pkg.overrides overrides
      } in
      return (pkg, source)
    | PackageOverride {source = nextSource; override} ->
      let%bind nextSource = RunAsync.ofRun (rebaseSource ~base:source nextSource) in
      let overrides = Package.Overrides.add override overrides in
      loop' ~allowEmptyPackage:true ~overrides nextSource
  in

  let%bind pkg, finalSource =
    loop'
      ~allowEmptyPackage
      ~overrides
      source
  in

  Hashtbl.replace resolver.sourceToSource source finalSource;
  return pkg

let package ~(resolution : Resolution.t) resolver =
  let open RunAsync.Syntax in
  let key = (resolution.name, resolution.resolution) in

  let ofVersion (version : Version.t) =
    match version with
    | Version.Source source ->
      let%bind pkg =
        packageOfSource
          ~allowEmptyPackage:false
          ~overrides:Package.Overrides.empty
          ~name:resolution.name
          source
          resolver in
      return (Ok pkg)

    | Version.Npm version ->
      let%bind pkg =
        NpmRegistry.package
          ~name:resolution.name
          ~version
          resolver.npmRegistry ()
      in
      return (Ok pkg)
    | Version.Opam version ->
      begin match%bind
        let%bind name = opamname resolution.name in
        OpamRegistry.version
          ~name
          ~version
          resolver.opamRegistry
      with
        | Some manifest ->
          OpamManifest.toPackage
            ~name:resolution.name
            ~version:(Version.Opam version)
            manifest
        | None -> error ("no such opam package: " ^ resolution.name)
      end
  in

  let applyOverride pkg override =
    let pkg =
      match override.Package.Override.dependencies with
      | Some dependencies -> {
          pkg with
          Package.
          dependencies = Package.Dependencies.NpmFormula dependencies
        }
      | None -> pkg
    in
    pkg
  in

  PackageCache.compute resolver.pkgCache key begin fun _ ->
    match resolution.resolution with
    | Version version -> ofVersion version
    | SourceOverride {source; override} ->
      let overrides = Package.Overrides.(empty |> add override) in
      let%bind pkg =
        packageOfSource
          ~allowEmptyPackage:true
          ~name:resolution.name
          ~overrides
          source
          resolver
      in
      let pkg =
        Package.Overrides.apply
          pkg.Package.overrides
          applyOverride
          pkg
        in
      return (Ok pkg)
  end

let resolveSource ~name ~(sourceSpec : SourceSpec.t) (resolver : t) =
  let open RunAsync.Syntax in

  let errorResolvingSource msg =
    errorf
      "unable to resolve %s@%a: %s"
      name SourceSpec.pp sourceSpec msg
  in

  SourceCache.compute resolver.srcCache sourceSpec begin fun _ ->
    let%lwt () = Logs_lwt.debug (fun m -> m "resolving %s@%a" name SourceSpec.pp sourceSpec) in
    let%bind source =
      match sourceSpec with
      | SourceSpec.Github {user; repo; ref; manifest;} ->
        let remote = Printf.sprintf "https://github.com/%s/%s.git" user repo in
        let%bind commit = Git.lsRemote ?ref ~remote () in
        begin match commit, ref with
        | Some commit, _ ->
          return (Source.Github {user; repo; commit; manifest;})
        | None, Some ref ->
          if Git.isCommitLike ref
          then return (Source.Github {user; repo; commit = ref; manifest;})
          else errorResolvingSource "cannot resolve commit"
        | None, None ->
          errorResolvingSource "cannot resolve commit"
        end

      | SourceSpec.Git {remote; ref; manifest;} ->
        let%bind commit = Git.lsRemote ?ref ~remote () in
        begin match commit, ref  with
        | Some commit, _ ->
          return (Source.Git {remote; commit; manifest;})
        | None, Some ref ->
          if Git.isCommitLike ref
          then return (Source.Git {remote; commit = ref; manifest;})
          else errorResolvingSource "cannot resolve commit"
        | None, None ->
          errorResolvingSource "cannot resolve commit"
        end

      | SourceSpec.NoSource ->
        return (Source.NoSource)

      | SourceSpec.Archive {url; checksum = None} ->
        failwith ("archive sources without checksums are not implemented: " ^ url)
      | SourceSpec.Archive {url; checksum = Some checksum} ->
        return (Source.Archive {url; checksum})

      | SourceSpec.LocalPath {path; manifest;} ->
        return (Source.LocalPath {path; manifest;})

      | SourceSpec.LocalPathLink {path; manifest;} ->
        return (Source.LocalPathLink {path; manifest;})
    in
    Hashtbl.replace resolver.sourceSpecToSource sourceSpec source;
    return source
  end

let resolve' ~fullMetadata ~name ~spec resolver =
  let open RunAsync.Syntax in
  match spec with

  | VersionSpec.Npm _
  | VersionSpec.NpmDistTag _ ->

    let%bind resolutions =
      let%lwt () = Logs_lwt.debug (fun m -> m "resolving %s" name) in
      let%bind {NpmRegistry. versions; distTags;} =
        match%bind
          NpmRegistry.versions ~fullMetadata ~name resolver.npmRegistry ()
        with
        | None -> errorf "no npm package %s found" name
        | Some versions -> return versions
      in

      Hashtbl.replace resolver.npmDistTags name distTags;

      let resolutions =
        let f version =
          let version = Version.Npm version in
          {Resolution. name; resolution = Version version}
        in
        List.map ~f versions
      in

      return resolutions
    in

    let resolutions =
      let tryCheckConformsToSpec resolution =
        match resolution.Resolution.resolution with
        | Version version ->
          versionMatchesReq resolver (Req.make ~name ~spec) resolution.name version
        | SourceOverride _ -> true (* do not filter them out yet *)
      in

      resolutions
      |> List.sort ~cmp:(fun a b -> Resolution.compare b a)
      |> List.filter ~f:tryCheckConformsToSpec
    in

    return resolutions

  | VersionSpec.Opam _ ->
    let%bind resolutions =
      ResolutionCache.compute resolver.resolutionCache name begin fun () ->
        let%lwt () = Logs_lwt.debug (fun m -> m "resolving %s" name) in
        let%bind versions =
          let%bind name = opamname name in
          OpamRegistry.versions
            ?ocamlVersion:(toOpamOcamlVersion resolver.ocamlVersion)
            ~name
            resolver.opamRegistry
        in
        let f (resolution : OpamRegistry.resolution) =
          let version = Version.Opam resolution.version in
          {Resolution. name; resolution = Version version}
        in
        return (List.map ~f versions)
      end
    in

    let resolutions =
      let tryCheckConformsToSpec resolution =
        match resolution.Resolution.resolution with
        | Version version ->
          versionMatchesReq resolver (Req.make ~name ~spec) resolution.name version
        | SourceOverride _ -> true (* do not filter them out yet *)
      in

      resolutions
      |> List.sort ~cmp:(fun a b -> Resolution.compare b a)
      |> List.filter ~f:tryCheckConformsToSpec
    in

    return resolutions

  | VersionSpec.Source sourceSpec ->
    let%bind source = resolveSource ~name ~sourceSpec resolver in
    let version = Version.Source source in
    let resolution = {
      Resolution.
      name;
      resolution = Resolution.Version version;
    } in
    return [resolution]

let resolve ?(fullMetadata=false) ~(name : string) ?(spec : VersionSpec.t option) (resolver : t) =
  let open RunAsync.Syntax in
  match Resolutions.find resolver.resolutions name with
  | Some resolution -> return [resolution]
  | None ->
    let spec =
      match spec with
      | None ->
        if Package.isOpamPackageName name
        then VersionSpec.Opam [[OpamPackageVersion.Constraint.ANY]]
        else VersionSpec.Npm [[SemverVersion.Constraint.ANY]]
      | Some spec -> spec
    in
    resolve' ~fullMetadata ~name ~spec resolver
