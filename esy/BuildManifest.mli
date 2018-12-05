(**
 * This module represents manifests and info which can be parsed out of it.
 *)

module BuildType : module type of EsyLib.BuildType
module SourceType : module type of EsyLib.SourceType

module Source : module type of EsyInstall.Source
module Version : module type of EsyInstall.Version
module Command : module type of EsyInstall.Package.Command
module CommandList : module type of EsyInstall.Package.CommandList
module ExportedEnv : module type of EsyInstall.Package.ExportedEnv
module Env : module type of EsyInstall.Package.Env

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
  buildEnv : Env.t;
}

include S.PRINTABLE with type t := t
val empty : name:string option -> version:Version.t option -> unit -> t

val to_yojson : t Json.encoder

val ofInstallationLocation :
  cfg:Config.t
  -> EsyInstall.Solution.Package.t
  -> EsyInstall.Installation.location
  -> (t option * Fpath.set) RunAsync.t
