open EsyPackageConfig

module Scripts = struct
  [@@@ocaml.warning "-32"]
  type t =
    script StringMap.t
    [@@deriving ord]

  and script = {
    command : PackageConfig.Command.t;
  }
  [@@deriving ord]

  let of_yojson =
    let script (json: Json.t) =
      match PackageConfig.CommandList.of_yojson json with
      | Ok command ->
        begin match command with
        | [] -> Error "empty command"
        | [command] -> Ok {command;}
        | _ -> Error "multiple script commands are not supported"
        end
      | Error err -> Error err
    in
    Json.Decode.stringMap script

  let empty = StringMap.empty

  let find (cmd: string) (scripts: t) =
    StringMap.find_opt cmd scripts
end

module OfPackageJson = struct
  type t = {
    scripts : Scripts.t [@default Scripts.empty];
  } [@@deriving of_yojson { strict = false }]
end

type t = Scripts.t
type script = Scripts.script = { command : PackageConfig.Command.t; }

let empty = Scripts.empty
let find = Scripts.find

let ofSandbox (spec : EsyInstall.SandboxSpec.t) =
  let open RunAsync.Syntax in
  match spec.manifest with

  | EsyInstall.SandboxSpec.Manifest (Esy, filename) ->
    let%bind json = Fs.readJsonFile Path.(spec.path / filename) in
    let%bind pkgJson = RunAsync.ofRun (Json.parseJsonWith OfPackageJson.of_yojson json) in
    return pkgJson.OfPackageJson.scripts

  | EsyInstall.SandboxSpec.Manifest (Opam, _)
  | EsyInstall.SandboxSpec.ManifestAggregate _ ->
    return Scripts.empty
