module Scripts = struct

  [@@@ocaml.warning "-32"]
  type script = {
    command : Metadata.Command.t;
  }
  [@@deriving (eq, ord)]

  type t =
    script StringMap.t
    [@@deriving (eq, ord)]

  let empty = StringMap.empty

  let of_yojson =
    let script (json: Json.t) =
      match Metadata.CommandList.of_yojson json with
      | Ok command ->
        begin match command with
        | [] -> Error "empty command"
        | [command] -> Ok {command;}
        | _ -> Error "multiple script commands are not supported"
        end
      | Error err -> Error err
    in
    Json.Decode.stringMap script

  let find (cmd: string) (scripts: t) = StringMap.find_opt cmd scripts
end

module Env = struct

  include Metadata.Env

  let empty = []

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

  let to_yojson env =
    let items =
      let f {name; value} = name, `String value in
      List.map ~f env
    in
    `Assoc items
end

module ExportedEnv = struct

  include Metadata.ExportedEnv

  let scope_of_yojson = function
    | `String "global" -> Ok Global
    | `String "local" -> Ok Local
    | _ -> Error "expected either \"local\" or \"global\""

  let scope_to_yojson = function
    | Local -> `String "local"
    | Global -> `String "global"

  module Item = struct
    type t = {
      value : string [@key "val"];
      scope : (scope [@default Local]);
      exclusive : (bool [@default false]);
    }
    [@@deriving of_yojson]
  end

  let empty = []

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

  let to_yojson env =
    let items =
      let f {name; value; scope; exclusive;} =
        name, `Assoc [
          "val", `String value;
          "scope", scope_to_yojson scope;
          "exclusive", `Bool exclusive;
        ]
      in
      List.map ~f env
    in
    `Assoc items

end

module Dependencies = struct
  include Metadata.Dependencies

  let empty = []

  let pp fmt deps =
    Fmt.pf fmt "@[<hov>[@;%a@;]@]" (Fmt.list ~sep:(Fmt.unit ", ") Req.pp) deps

  let of_yojson json =
    let open Result.Syntax in
    let%bind items = Json.Decode.assoc json in
    let f deps (name, json) =
      let%bind spec = Json.Decode.string json in
      let%bind req = Req.parse (name ^ "@" ^ spec) in
      return (req::deps)
    in
    Result.List.foldLeft ~f ~init:empty items

  let to_yojson (reqs : t) =
    let items =
      let f (req : Req.t) = (req.name, VersionSpec.to_yojson req.spec) in
      List.map ~f reqs
    in
    `Assoc items

  let override deps update =
    let map =
      let f map (req : Req.t) = StringMap.add req.name req map in
      let map = StringMap.empty in
      let map = List.fold_left ~f ~init:map deps in
      let map = List.fold_left ~f ~init:map update in
      map
    in
    StringMap.values map

  let find ~name reqs =
    let f (req : Req.t) = req.name = name in
    List.find_opt ~f reqs
end

module Resolutions = struct
  include Metadata.Resolutions

  let empty = StringMap.empty

  let find resolutions pkgName =
    StringMap.find_opt pkgName resolutions

  let entries = StringMap.bindings

  let to_yojson v =
    let items =
      let f k v items = (k, (`String (Version.show v)))::items in
      StringMap.fold f v []
    in
    `Assoc items

  let of_yojson =
    let open Result.Syntax in
    let parseKey k =
      match PackagePath.parse k with
      | Ok ((_path, name)) -> Ok name
      | Error err -> Error err
    in
    let parseValue key json =
      match json with
      | `String v -> begin
        match Astring.String.cut ~sep:"/" key with
        | Some ("@opam", _) -> Version.parse ~tryAsOpam:true v
        | _ -> Version.parse v
        end
      | `Assoc _ ->
        let%bind source = Source.of_yojson json in
        return (Version.Source source)
      | _ -> Error "expected string"
    in
    function
    | `Assoc items ->
      let f res (key, json) =
        let%bind key = parseKey key in
        let%bind value = parseValue key json in
        Ok (StringMap.add key value res)
      in
      Result.List.foldLeft ~f ~init:empty items
    | _ -> Error "expected object"

end

module EsyPackageJson = struct
  type t = {
    _dependenciesForNewEsyInstaller : (Dependencies.t option [@default None]);
  } [@@deriving of_yojson { strict = false }]
end

type t = {
  name : (string option [@default None]);
  version : (SemverVersion.Version.t option [@default None]);
  dependencies : (Dependencies.t [@default Dependencies.empty]);
  devDependencies : (Dependencies.t [@default Dependencies.empty]);
  esy : (EsyPackageJson.t option [@default None]);
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
