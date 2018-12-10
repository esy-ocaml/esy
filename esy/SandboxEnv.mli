type t = BuildManifest.Env.t

val empty : t

val ofSandbox : EsyInstall.SandboxSpec.t -> t RunAsync.t

include S.JSONABLE with type t := t
