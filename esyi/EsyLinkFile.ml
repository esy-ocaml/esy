type t = {
  source : Source.source;
  manifest : SandboxSpec.ManifestSpec.t option;
  override : Source.Override.t;
} [@@deriving yojson]

let ofFile path =
  let open RunAsync.Syntax in
  let%bind json = Fs.readJsonFile path in
  RunAsync.ofRun (Json.parseJsonWith of_yojson json)

let toFile file path =
  let json = to_yojson file in
  Fs.writeJsonFile ~json path
