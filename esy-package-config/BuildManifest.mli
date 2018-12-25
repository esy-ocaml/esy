type commands =
  | OpamCommands of OpamTypes.command list
  | EsyCommands of CommandList.t
  | NoCommands

val commands_to_yojson : commands Json.encoder

type t = {
  name : string option;
  version : Version.t option;
  buildType : BuildType.t;
  build : commands;
  buildDev : CommandList.t option;
  install : commands;
  patches : (Path.t * OpamTypes.filter option) list;
  substs : Path.t list;
  exportedEnv : ExportedEnv.t;
  buildEnv : BuildEnv.t;
}

val empty : name:string option -> version:Version.t option -> unit -> t

include S.PRINTABLE with type t := t

val to_yojson : t Json.encoder
