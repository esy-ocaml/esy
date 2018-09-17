type t = {
  path : Path.t;
  manifest : SandboxSpec.ManifestSpec.t option [@default None];
  override : Package.Override.t option [@default None];
} [@@deriving yojson]

let ofFile path =
  let open RunAsync.Syntax in
  let%bind json = Fs.readJsonFile path in
  RunAsync.ofRun (Json.parseJsonWith of_yojson json)

let toFile file path =
  let json = to_yojson file in
  Fs.writeJsonFile ~json path
