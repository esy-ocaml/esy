module SandboxPath : module type of EsyBuildPackage.Config.Path
module SandboxValue : module type of EsyBuildPackage.Config.Value
module SandboxEnvironment : module type of EsyBuildPackage.Config.Environment

type t

val make :
  platform:System.Platform.t
  -> sandboxEnv:SandboxEnvironment.Bindings.t
  -> id:string
  -> name:string
  -> version:EsyInstall.Version.t
  -> sourceType:BuildManifest.SourceType.t
  -> sourcePath:SandboxPath.t
  -> EsyInstall.Solution.Package.t
  -> BuildManifest.t
  -> t
(** An initial scope for the package. *)

val add : direct:bool -> dep:t -> t -> t
(** Add new pkg *)

val pkg : t -> EsyInstall.Solution.Package.t
val id : t -> string
val name : t -> string
val version : t -> EsyInstall.Version.t
val sourceType : t -> BuildManifest.SourceType.t
val buildType : t -> BuildManifest.BuildType.t
val storePath : t -> SandboxPath.t
val rootPath : t -> SandboxPath.t
val sourcePath : t -> SandboxPath.t
val buildPath : t -> SandboxPath.t
val buildInfoPath : t -> SandboxPath.t
val stagePath : t -> SandboxPath.t
val installPath : t -> SandboxPath.t
val logPath : t -> SandboxPath.t

val pp : t Fmt.t

val env : includeBuildEnv:bool -> buildIsInProgress:bool -> t -> SandboxEnvironment.Bindings.t Run.t

val renderCommandExpr : ?environmentVariableName:string -> buildIsInProgress:bool -> t -> string -> string Run.t

val toOpamEnv : buildIsInProgress:bool -> t -> OpamFilter.env

val exposeUserEnvWith : (string -> SandboxValue.t -> SandboxValue.t Environment.Binding.t) -> string -> t -> t
