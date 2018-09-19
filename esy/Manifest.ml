module BuildType = struct
  include EsyLib.BuildType
  include EsyLib.BuildType.AsInPackageJson
end

module SandboxSpec = EsyInstall.SandboxSpec
module PackageJson = EsyInstall.PackageJson
module Source = EsyInstall.Source
module SourceType = EsyLib.SourceType
module Command = PackageJson.Command
module CommandList = PackageJson.CommandList
module ExportedEnv = PackageJson.ExportedEnv
module Env = PackageJson.Env

module Build = struct

  (* aliases for opam types with to_yojson implementations *)
  module OpamTypes = struct
    type filter = OpamTypes.filter

    let filter_to_yojson filter = `String (OpamFilter.to_string filter)

    type command = arg list * filter option [@@deriving to_yojson]
    and arg = simple_arg * filter option
    and simple_arg = OpamTypes.simple_arg =
      | CString of string
      | CIdent of string
  end

  type commands =
    | OpamCommands of OpamTypes.command list
    | EsyCommands of CommandList.t
    [@@deriving to_yojson]

  type patch = Path.t * OpamTypes.filter option

  let patch_to_yojson (path, filter) =
    let filter =
      match filter with
      | None -> `Null
      | Some filter -> `String (OpamFilter.to_string filter)
    in
    `Assoc ["path", Path.to_yojson path; "filter", filter]

  type t = {
    buildType : BuildType.t;
    buildCommands : commands;
    installCommands : commands;
    patches : patch list;
    substs : Path.t list;
    exportedEnv : ExportedEnv.t;
    buildEnv : Env.t;
  } [@@deriving to_yojson]

  let empty = {
    buildType = BuildType.OutOfSource;
    buildCommands = EsyCommands [];
    installCommands = EsyCommands [];
    patches = [];
    substs = [];
    exportedEnv = ExportedEnv.empty;
    buildEnv = StringMap.empty;
  }

end

module Dependencies = struct
  type t = {
    dependencies : string list list;
    devDependencies : string list list;
    buildTimeDependencies : string list list;
    optDependencies : string list list;
  } [@@deriving show]

  let empty = {
    dependencies = [];
    devDependencies = [];
    buildTimeDependencies = [];
    optDependencies = [];
  }
end

module Release = struct
  type t = {
    releasedBinaries: string list;
    deleteFromBinaryRelease: (string list [@default []]);
  } [@@deriving (of_yojson { strict = false })]
end

module Scripts = struct

  [@@@ocaml.warning "-32"]
  type script = {
    command : Command.t;
  }
  [@@deriving ord]

  type t =
    script StringMap.t
    [@@deriving ord]

  let empty = StringMap.empty

  let of_yojson =
    let script (json: Json.t) =
      match CommandList.of_yojson json with
      | Ok command ->
        begin match command with
        | [] -> Error "empty command"
        | [command] -> Ok {command;}
        | _ -> Error "multiple script commands are not supported"
        end
      | Error err -> Error err
    in
    Json.Parse.stringMap script

  let find (cmd: string) (scripts: t) = StringMap.find_opt cmd scripts
end

module type MANIFEST = sig
  (**
   * Manifest.
   *
   * This can be either esy manifest (package.json/esy.json) or opam manifest but
   * this type abstracts them out.
   *)
  type t

  (** Name. *)
  val name : t -> string

  (** Version. *)
  val version : t -> string

  (** License. *)
  val license : t -> Json.t option

  (** Description. *)
  val description : t -> string option

  (**
   * Extract dependency info.
   *)
  val dependencies : t -> Dependencies.t

  (**
   * Extract build config from manifest
   *
   * Not all packages have build config defined so we return `None` in this case.
   *)
  val build : t -> Build.t option

  (**
   * Extract release config from manifest
   *
   * Not all packages have release config defined so we return `None` in this
   * case.
   *)
  val release : t -> Release.t option

  (**
   * Extract release config from manifest
   *
   * Not all packages have release config defined so we return `None` in this
   * case.
   *)
  val scripts : t -> Scripts.t Run.t

  val sandboxEnv : t -> Env.t Run.t
end

module type QUERY_MANIFEST = sig
  include MANIFEST

  [@@@ocaml.warning "-32"]
  val manifest : t
end

module Esy : sig
  include MANIFEST

  val ofFile : Path.t -> t RunAsync.t
end = struct

  module EsyManifest = struct

    type t = {
      build: (CommandList.t [@default CommandList.empty]);
      install: (CommandList.t [@default CommandList.empty]);
      buildsInSource: (BuildType.t [@default BuildType.OutOfSource]);
      exportedEnv: (ExportedEnv.t [@default ExportedEnv.empty]);
      buildEnv: (Env.t [@default Env.empty]);
      sandboxEnv: (Env.t [@default Env.empty]);
      release: (Release.t option [@default None]);
    } [@@deriving (of_yojson { strict = false })]

  end

  module JsonManifest = struct
    type t = {
      name : (string option [@default None]);
      version : (string option [@default None]);
      description : (string option [@default None]);
      license : (Json.t option [@default None]);
      dependencies : (PackageJson.Dependencies.t [@default PackageJson.Dependencies.empty]);
      peerDependencies : (PackageJson.Dependencies.t [@default PackageJson.Dependencies.empty]);
      devDependencies : (PackageJson.Dependencies.t [@default PackageJson.Dependencies.empty]);
      optDependencies : (PackageJson.Dependencies.t [@default PackageJson.Dependencies.empty]);
      buildTimeDependencies : (PackageJson.Dependencies.t [@default PackageJson.Dependencies.empty]);
      esy: EsyManifest.t option [@default None];
    } [@@deriving (of_yojson {strict = false})]
  end

  type manifest = {
    name : string;
    version : string;
    description : string option;
    license : Json.t option;
    dependencies : PackageJson.Dependencies.t;
    peerDependencies : PackageJson.Dependencies.t;
    devDependencies : PackageJson.Dependencies.t;
    optDependencies : PackageJson.Dependencies.t;
    buildTimeDependencies : PackageJson.Dependencies.t;
    esy: EsyManifest.t option;
  }

  type t = manifest * Json.t

  let name (manifest, _) = manifest.name
  let version (manifest, _) = manifest.version
  let description (manifest, _) = manifest.description
  let license (manifest, _) = manifest.license

  let dependencies (manifest, _) =
    let names reqs = List.map ~f:(fun req -> [req.EsyInstall.Req.name]) reqs in
    let dependencies =
      let dependencies = names manifest.dependencies in
      let peerDependencies = names manifest.peerDependencies in
      dependencies @ peerDependencies
    in
    let devDependencies = names manifest.devDependencies in
    let optDependencies = names manifest.optDependencies in
    let buildTimeDependencies = names manifest.buildTimeDependencies in
    {
      Dependencies.
      dependencies;
      devDependencies;
      optDependencies;
      buildTimeDependencies
    }

  let release (m, _) =
    let open Option.Syntax in
    let%bind m = m.esy in
    let%bind c = m.EsyManifest.release in
    return c

  let scripts (_, json) =
    let open Run.Syntax in
    match json with
    | `Assoc items ->
      let f (name, _) = name = "scripts" in
      begin match List.find_opt ~f items with
      | Some (_, json) -> Run.ofStringError (Scripts.of_yojson json)
      | None -> return Scripts.empty
      end
    | _ -> return Scripts.empty

  let sandboxEnv (m, _) =
    match m.esy with
    | None -> Run.return Env.empty
    | Some m -> Run.return m.sandboxEnv

  let build (m, _) =
    let open Option.Syntax in
    let%bind esy = m.esy in
    Some {
      Build.
      buildType = esy.EsyManifest.buildsInSource;
      exportedEnv = esy.EsyManifest.exportedEnv;
      buildEnv = esy.EsyManifest.buildEnv;
      buildCommands = EsyCommands (esy.EsyManifest.build);
      installCommands = EsyCommands (esy.EsyManifest.install);
      patches = [];
      substs = [];
    }

  let ofJsonManifest (jsonManifest: JsonManifest.t) (path: Path.t) =
    let name = 
      match jsonManifest.name with
      | Some name  -> name
      | None -> Path.basename path
    in
    let version =
      match jsonManifest.version with
      | Some version  -> version
      | None -> "0.0.0"
    in
    {
      name;
      version;
      description = jsonManifest.description;
      license = jsonManifest.license;
      dependencies = jsonManifest.dependencies;
      peerDependencies = jsonManifest.peerDependencies;
      devDependencies = jsonManifest.devDependencies;
      optDependencies = jsonManifest.optDependencies;
      buildTimeDependencies = jsonManifest.buildTimeDependencies;
      esy = jsonManifest.esy;
    }

  let ofFile (path : Path.t) =
    let open RunAsync.Syntax in
    let%bind json = Fs.readJsonFile path in
    let%bind jsonManifest =
      RunAsync.ofRun (Json.parseJsonWith JsonManifest.of_yojson json)
    in
    let manifest = ofJsonManifest jsonManifest path in
    return (manifest, json)

end

module Opam : sig
  include MANIFEST

  val ofFiles :
    ?name:string
    -> Path.t list
    -> t RunAsync.t

  val ofInstallation :
    ?name:string
    -> Path.t
    -> t option RunAsync.t

  val ofFile :
    name:string
    -> version:string
    -> Path.t
    -> t RunAsync.t

end = struct
  type t =
    | Installed of {
        name : string option;
        info : EsyInstall.Solution.Record.Opam.t;
      }
    | AggregatedRoot of {
        name : string option;
        opam : (string * OpamFile.OPAM.t) list
      }

  let opamname = function
    | Installed {info; _} -> OpamPackage.Name.to_string info.name
    | AggregatedRoot _ -> "root"

  let name manifest =
    match manifest with
    | Installed {name = Some name; _} -> name
    | AggregatedRoot {name = Some name; _} -> name
    | manifest -> "@opam/" ^ (opamname manifest)

  let version = function
    | Installed {info;_} -> OpamPackage.Version.to_string info.version
    | AggregatedRoot _ -> "dev"

  let listPackageNamesOfFormula ~build ~test ~post ~doc ~dev formula =
    let formula =
      OpamFilter.filter_deps
        ~default:true ~build ~post ~test ~doc ~dev
        formula
    in
    let cnf = OpamFormula.to_cnf formula in
    let f atom =
      let name, _ = atom in
      match OpamPackage.Name.to_string name with
      | "ocaml" -> "ocaml"
      | name -> "@opam/" ^ name
    in
    List.map ~f:(List.map ~f) cnf

  let dependencies manifest =
    let dependencies =

      let dependsOfOpam opam =
        let f = OpamFile.OPAM.depends opam in
        let dependencies =
          listPackageNamesOfFormula
            ~build:true ~test:false ~post:true ~doc:false ~dev:false
            f
        in
        let dependencies = ["ocaml"]::["@esy-ocaml/substs"]::dependencies in

        dependencies
      in
      match manifest with
      | Installed {info; name = _} ->
        let dependencies = dependsOfOpam info.opam in
        begin
        match info.override with
        | Some {EsyInstall.Package.OpamOverride. dependencies = extraDependencies; _} ->
          let extraDependencies =
            extraDependencies
            |> List.map ~f:(fun req -> [req.EsyInstall.Req.name])
          in
          List.append dependencies extraDependencies
        | None -> dependencies
        end
      | AggregatedRoot {opam; _} ->
        let namesPresent =
          let f names (name, _) = StringSet.add ("@opam/" ^ name) names in
          List.fold_left ~f ~init:StringSet.empty opam
        in
        let f dependencies (_name, opam) =
          let update = dependsOfOpam opam in
          let update =
            let f name = not (StringSet.mem name namesPresent) in
            List.map ~f:(List.filter ~f) update
          in
          let update =
            let f = function | [] -> false | _ -> true in
            List.filter ~f update
          in
          dependencies @ update
        in
        List.fold_left ~f ~init:[] opam
    in

    let optDependencies =
      match manifest with
      | Installed {info;_} ->
        let dependencies =
          let f = OpamFile.OPAM.depopts info.opam in
          let dependencies =
            listPackageNamesOfFormula
              ~build:true ~test:false ~post:true ~doc:false ~dev:false
              f
          in
          match dependencies with
          | [] -> []
          | [single] -> List.map ~f:(fun name -> [name]) single
          | _multi ->
            (** apparently depopts has a different structure than depends in opam,
            * it's always a single list of packages in cnf
            * TODO: cleanup this mess
            *)
            assert false
        in
        dependencies
      | AggregatedRoot _ -> []
    in
    {
      Dependencies.
      dependencies;
      buildTimeDependencies = [];
      devDependencies = [];
      optDependencies;
    }

  let ofInstallation ?name (path : Path.t) =
    let open RunAsync.Syntax in
    match%bind EsyInstall.EsyLinkFile.ofDirIfExists path with
    | None
    | Some { EsyInstall.EsyLinkFile. opam = None; _ } ->
      return None
    | Some { EsyInstall.EsyLinkFile. opam = Some info; _ } ->
      return (Some (Installed {info; name}))

  let readOpam path =
    let open RunAsync.Syntax in
    let%bind data = Fs.readFile path in
    if String.trim data = ""
    then return None
    else
      let opam = OpamFile.OPAM.read_from_string data in
      let name = Path.(path |> remExt |> basename) in
      return (Some (name, opam))

  let ofFile ~name ~version (path : Path.t) =
    let open RunAsync.Syntax in
    match%bind readOpam path with
    | None -> errorf "unable to load opam manifest at %a" Path.pp path
    | Some (_, opam) ->
      let version = OpamPackage.Version.of_string version in
      return (Installed {
        name = Some name;
        info = {
          EsyInstall.Solution.Record.Opam.
          name = OpamPackage.Name.of_string "pkg"; version; opam;
          override = None;
        }
      })

  let ofFiles ?name paths =
    let open RunAsync.Syntax in
    let%bind opams =
      paths
      |> List.map ~f:readOpam
      |> RunAsync.List.joinAll
    in
    return (AggregatedRoot {name; opam = List.filterNone opams;})

  let release _ = None

  let description _ = None
  let license _ = None

  let build m =
    let buildCommands =
      match m with
      | Installed manifest ->
        begin match manifest.info.override with
        | Some {EsyInstall.Package.OpamOverride. build = Some build; _} ->
          Build.EsyCommands build
        | Some {EsyInstall.Package.OpamOverride. build = None; _}
        | None ->
          Build.OpamCommands (OpamFile.OPAM.build manifest.info.opam)
        end
      | AggregatedRoot {opam = [_name, opam]; _} ->
        Build.OpamCommands (OpamFile.OPAM.build opam)
      | AggregatedRoot _ ->
        Build.OpamCommands []
    in

    let installCommands =
      match m with
      | Installed manifest ->
        begin match manifest.info.override with
        | Some {EsyInstall.Package.OpamOverride. install = Some install; _} ->
          Build.EsyCommands install
        | Some {EsyInstall.Package.OpamOverride. install = None; _}
        | None ->
          Build.OpamCommands (OpamFile.OPAM.install manifest.info.opam)
        end
      | AggregatedRoot {opam = [_name, opam]; _} ->
        Build.OpamCommands (OpamFile.OPAM.install opam)
      | AggregatedRoot _ ->
        Build.OpamCommands []
    in

    let patches =
      match m with
      | Installed manifest ->
        let patches = OpamFile.OPAM.patches manifest.info.opam in
        let f (name, filter) =
          let name = Path.v (OpamFilename.Base.to_string name) in
          (name, filter)
        in
        List.map ~f patches
      | AggregatedRoot _ -> []
    in

    let substs =
      match m with
      | Installed manifest ->
        let names = OpamFile.OPAM.substs manifest.info.opam in
        let f name = Path.v (OpamFilename.Base.to_string name) in
        List.map ~f names
      | AggregatedRoot _ -> []
    in

    let buildType =
      match m with
      | Installed _ -> BuildType.InSource
      | AggregatedRoot _ -> BuildType.Unsafe
    in

    let exportedEnv =
      match m with
      | Installed manifest ->
        begin match manifest.info.override with
        | Some {EsyInstall.Package.OpamOverride. exportedEnv;_} -> exportedEnv
        | None -> ExportedEnv.empty
        end
      | AggregatedRoot _ -> ExportedEnv.empty
    in

    Some {
      Build.
      buildType;
      exportedEnv;
      buildEnv = Env.empty;
      buildCommands;
      installCommands;
      patches;
      substs;
    }

  let scripts _ = Run.return Scripts.empty

  let sandboxEnv _ = Run.return Env.empty

end

module EsyOrOpamManifest : sig
  include MANIFEST

  val dirHasManifest : Path.t -> bool RunAsync.t
  val ofSandboxSpec : SandboxSpec.t -> (t * Path.Set.t) RunAsync.t
  val ofDir :
    ?name:string
    -> ?manifest:SandboxSpec.ManifestSpec.t
    -> Path.t
    -> (t * Path.Set.t) option RunAsync.t

end = struct

  module type QUERY_MANIFEST = sig
    include MANIFEST

    val manifest : t
  end

  type t = (module QUERY_MANIFEST)

  let name (module M : QUERY_MANIFEST) = M.name M.manifest
  let version (module M : QUERY_MANIFEST) = M.version M.manifest
  let description (module M : QUERY_MANIFEST) = M.description M.manifest
  let license (module M : QUERY_MANIFEST) = M.license M.manifest
  let dependencies (module M : QUERY_MANIFEST) = M.dependencies M.manifest
  let build (module M : QUERY_MANIFEST) = M.build M.manifest
  let release (module M : QUERY_MANIFEST) = M.release M.manifest
  let scripts (module M : QUERY_MANIFEST) = M.scripts M.manifest
  let sandboxEnv (module M : QUERY_MANIFEST) = M.sandboxEnv M.manifest

  let ofDir ?name ?manifest (path : Path.t) =
    let open RunAsync.Syntax in

    let discoverOfDir path =

      let filenames =
        let dirname = Path.basename path in
        [
          `Esy, Path.v "esy.json";
          `Esy, Path.v "package.json";
          `Opam, Path.(v dirname |> addExt ".opam");
          `Opam, Path.v "opam";
        ]
      in

      let rec tryLoad = function
        | [] -> return None
        | (kind, fname)::rest ->
          let fname = Path.(path // fname) in
          if%bind Fs.exists fname
          then (
            match kind with
            | `Esy ->
              let%bind manifest = Esy.ofFile fname in
              let m =
                (module struct
                  include Esy
                  let manifest = manifest
                end : QUERY_MANIFEST)
              in
              return (Some (m, Path.Set.singleton fname))
            | `Opam ->
              let name =
                match name with
                | Some name -> name
                | None -> Path.basename path
              in
              let%bind manifest = Opam.ofFile ~name ~version:"dev" fname in
              let m =
                (module struct
                  include Opam
                  let manifest = manifest
                end : QUERY_MANIFEST)
              in
              return (Some (m, Path.Set.singleton fname))
          )
          else tryLoad rest
      in

      tryLoad filenames
    in

    match manifest with
    | None ->
      begin match%bind Opam.ofInstallation ?name path with
      | Some manifest ->
        let m =
          (module struct
            include Opam
            let manifest = manifest
          end : QUERY_MANIFEST)
        in
        return (Some (m, Path.Set.empty))
      | None -> discoverOfDir path
      end
    | Some (SandboxSpec.ManifestSpec.OpamAggregated _) ->
      errorf "unable to load manifest from aggregated opam files"
    | Some (SandboxSpec.ManifestSpec.Esy fname) ->
      let path = Path.(path / fname) in
      let%bind manifest = Esy.ofFile path in
      let m =
        (module struct
          include Esy
          let manifest = manifest
        end : QUERY_MANIFEST)
      in
      return (Some (m, Path.Set.singleton path))
    | Some (SandboxSpec.ManifestSpec.Opam fname) ->
      let name =
        match name with
        | Some name -> name
        | None -> Path.basename path
      in
      let path = Path.(path / fname) in
      let%bind manifest = Opam.ofFile ~name ~version:"dev" path in
      let m =
        (module struct
          include Opam
          let manifest = manifest
        end : QUERY_MANIFEST)
      in
      return (Some (m, Path.Set.singleton path))

  let ofSandboxSpec (spec : SandboxSpec.t) =
    let open RunAsync.Syntax in
    match spec.manifest with
    | SandboxSpec.ManifestSpec.Esy fname ->
      let path = Path.(spec.path / fname) in
      let%bind manifest = Esy.ofFile path in
      let m =
        (module struct
          include Esy
          let manifest = manifest
        end : QUERY_MANIFEST)
      in
      return (m, Path.Set.singleton path)
    | SandboxSpec.ManifestSpec.Opam fname ->
      let path = Path.(spec.path / fname) in
      let%bind manifest = Opam.ofFiles [path] in
      let m =
        (module struct
          include Opam
          let manifest = manifest
        end : QUERY_MANIFEST)
      in
      return (m, Path.Set.singleton path)
    | SandboxSpec.ManifestSpec.OpamAggregated fnames ->
      let paths = List.map ~f:(fun fname -> Path.(spec.path / fname)) fnames in
      let%bind manifest = Opam.ofFiles paths in
      let m =
        (module struct
          include Opam
          let manifest = manifest
        end : QUERY_MANIFEST)
      in
      return (m, Path.Set.of_list paths)

  let dirHasManifest (path : Path.t) =
    let open RunAsync.Syntax in
    let%bind names = Fs.listDir path in
    let f = function
      | "esy.json" | "package.json" | "opam" -> true
      | name -> Path.(name |> v |> hasExt ".opam")
    in
    return (List.exists ~f names)
end

include EsyOrOpamManifest
