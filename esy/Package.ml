(**
 * A list of commands as specified in "esy.build" and "esy.install".
 *)
module CommandList = struct

  module Command = struct

    type t =
      | Parsed of string list
      | Unparsed of string
      [@@deriving (show, to_yojson, eq, ord)]

    let of_yojson (json : Json.t) =
      match json with
      | `String command -> Ok (Unparsed command)
      | `List command ->
        begin match Json.Parse.(list string (`List command)) with
        | Ok args -> Ok (Parsed args)
        | Error err -> Error err
        end
      | _ -> Error "expected either a string or an array of strings"

  end

  type t =
    Command.t list option
    [@@deriving (show, eq, ord)]

  let of_yojson (json : Json.t) =
    let open Result.Syntax in
    let commands =
      match json with
      | `Null -> Ok []
      | `List commands ->
        Json.Parse.list Command.of_yojson (`List commands)
      | `String command ->
        let%bind command = Command.of_yojson (`String command) in
        Ok [command]
      | _ -> Error "expected either a null, a string or an array"
    in
    match%bind commands with
    | [] -> Ok None
    | commands -> Ok (Some commands)

  let to_yojson commands =
    match commands with
    | None -> `List []
    | Some commands -> `List (List.map ~f:Command.to_yojson commands)

end

(**
 * Scripts with keys as specified in "scripts".
 *)
module Scripts = struct

  type script = {
    command : Cmd.t;
  }
  [@@deriving (show, eq, ord)]

  type t =
    script StringMap.t
    [@@deriving (eq, ord)]

  let pp =
    let open Fmt in
    let ppBinding = hbox (pair (quote string) (quote pp_script)) in
    vbox ~indent:1 (iter_bindings ~sep:comma StringMap.iter ppBinding)

  let of_yojson =
    let open Json.Parse in
    let errorMsg =
      "A command in \"scripts\" expects a string or an array of strings"
    in
    let script (json: Json.t) = match cmd ~errorMsg json with
      | Ok command -> Ok {command;}
      | Error err -> Error err
    in
    stringMap script

  let find (cmd: string) (scripts: t) = StringMap.find_opt cmd scripts
end

(**
 * Environment for the entire sandbox as specified in "esy.sandboxEnv".
 *)
module SandboxEnv = struct

  type item = {
    name : string;
    value : string;
  }
  [@@deriving (show, eq, ord)]

  type t =
    item list
    [@@deriving (show, eq, ord)]

  let of_yojson = function
    | `Assoc items ->
      let open Result.Syntax in
      let f items ((k, v): (string * Yojson.Safe.json)) = match v with
      | `String value ->
        Ok ({name = k; value;}::items)
      | _ -> Error "expected string"
      in
      let%bind items = Result.List.foldLeft ~f ~init:[] items in
      Ok (List.rev items)
    | _ -> Error "expected an object"
end

(**
 * Environment exported from a package as specified in "esy.exportedEnv".
 *)
module ExportedEnv = struct

  type scope =
    | Local
    | Global
    [@@deriving (show, eq, ord)]

  let scope_of_yojson = function
    | `String "global" -> Ok Global
    | `String "local" -> Ok Local
    | _ -> Error "expected either \"local\" or \"global\""

  module Item = struct
    type t = {
      value : string [@key "val"];
      scope : (scope [@default Local]);
      exclusive : (bool [@default false]);
    }
    [@@deriving of_yojson]
  end

  type item = {
    name : string;
    value : string;
    scope : scope;
    exclusive : bool;
  }
  [@@deriving (show, eq, ord)]

  type t =
    item list
    [@@deriving (show, eq, ord)]

  let of_yojson = function
    | `Assoc items ->
      let open Result.Syntax in
      let f items (k, v) =
        let%bind {Item. value; scope; exclusive} = Item.of_yojson v in
        Ok ({name = k; value; scope; exclusive}::items)
      in
      let%bind items = Result.List.foldLeft ~f ~init:[] items in
      Ok (List.rev items)
    | _ -> Error "expected an object"

end

module BuildType = struct
  type t =
    | InSource
    | OutOfSource
    | JBuilderLike
    [@@deriving (show, eq, ord)]

  let of_yojson = function
    | `String "_build" -> Ok JBuilderLike
    | `Bool true -> Ok InSource
    | `Bool false -> Ok OutOfSource
    | _ -> Error "expected false, true or \"_build\""

end

module SourceType = struct
  type t =
    | Immutable
    | Development
    [@@deriving (show, eq, ord)]
end

module EsyReleaseConfig = struct
  type t = {
    releasedBinaries: string list;
    deleteFromBinaryRelease: (string list [@default []]);
  } [@@deriving (show, of_yojson { strict = false })]
end

module EsyManifest = struct

  type t = {
    build: (CommandList.t [@default None]);
    install: (CommandList.t [@default None]);
    buildsInSource: (BuildType.t [@default BuildType.OutOfSource]);
    exportedEnv: (ExportedEnv.t [@default []]);
    sandboxEnv: (SandboxEnv.t [@default []]);
    release: (EsyReleaseConfig.t option [@default None]);
  } [@@deriving (show, of_yojson { strict = false })]

  let empty = {
    build = None;
    install = None;
    buildsInSource = BuildType.OutOfSource;
    exportedEnv = [];
    sandboxEnv = [];
    release = None;
  }
end

module ManifestDependencyMap = struct
  type t = string StringMap.t

  let pp =
    let open Fmt in
    let ppBinding = hbox (pair (quote string) (quote string)) in
    vbox ~indent:1 (iter_bindings ~sep:comma StringMap.iter ppBinding)

  let of_yojson =
    Json.Parse.(stringMap string)

end

module Manifest = struct
  type t = {
    name : string;
    version : string;
    description : (string option [@default None]);
    license : (string option [@default None]);
    scripts: (Scripts.t [@default StringMap.empty]);
    dependencies : (ManifestDependencyMap.t [@default StringMap.empty]);
    peerDependencies : (ManifestDependencyMap.t [@default StringMap.empty]);
    devDependencies : (ManifestDependencyMap.t [@default StringMap.empty]);
    optDependencies : (ManifestDependencyMap.t [@default StringMap.empty]);
    buildTimeDependencies : (ManifestDependencyMap.t [@default StringMap.empty]);
    esy: EsyManifest.t option [@default None];
    _resolved: (string option [@default None]);
  } [@@deriving (show, of_yojson { strict = false })]

  let ofFile path =
    let open RunAsync.Syntax in
    if%bind (Fs.exists path) then (
      let%bind json = Fs.readJsonFile path in
      match of_yojson json with
      | Ok manifest -> return (Some manifest)
      | Error err -> error err
    ) else
      return None

  let ofDir (path : Path.t) =
    let open RunAsync.Syntax in
    let esyJson = Path.(path / "esy.json")
    and packageJson = Path.(path / "package.json")
    in match%bind ofFile esyJson with
    | None -> begin match%bind ofFile packageJson with
      | Some manifest -> return (Some (manifest, packageJson))
      | None -> return None
      end
    | Some manifest -> return (Some (manifest, esyJson))
end

type t = {
  id : string;
  name : string;
  version : string;
  dependencies : dependencies;
  buildCommands : CommandList.t;
  installCommands : CommandList.t;
  buildType : BuildType.t;
  sourceType : SourceType.t;
  exportedEnv : ExportedEnv.t;
  sandboxEnv : SandboxEnv.t;
  sourcePath : Config.ConfigPath.t;
  resolution : string option;
}
[@@deriving (show, eq, ord)]

and dependencies =
  dependency list
  [@@deriving show]

and dependency =
  | Dependency of t
  | PeerDependency of t
  | OptDependency of t
  | DevDependency of t
  | BuildTimeDependency of t
  | InvalidDependency of {
    pkgName: string;
    reason: string;
  }
  [@@deriving (show, ord)]

type pkg = t
type pkg_dependency = dependency

let packageOf (dep : dependency) = match dep with
| Dependency pkg
| PeerDependency pkg
| OptDependency pkg
| DevDependency pkg
| BuildTimeDependency pkg -> Some pkg
| InvalidDependency _ -> None

module DependencyGraph = DependencyGraph.Make(struct

  type t = pkg

  let compare a b = compare a b

  module Dependency = struct
    type t = pkg_dependency
    let compare a b = compare_dependency a b
  end

  let id (pkg : t) = pkg.id

  let traverse pkg =
    let f acc dep = match dep with
      | Dependency pkg
      | OptDependency pkg
      | DevDependency pkg
      | BuildTimeDependency pkg
      | PeerDependency pkg -> (pkg, dep)::acc
      | InvalidDependency _ -> acc
    in
    pkg.dependencies
    |> ListLabels.fold_left ~f ~init:[]
    |> ListLabels.rev

end)

module DependencySet = Set.Make(struct
  type t = dependency
  let compare = compare_dependency
end)
