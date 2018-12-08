open Esy

type t = {
  buildspec : BuildSandbox.BuildSpec.t;
  execenvspec : BuildSandbox.EnvSpec.t;
  commandenvspec : BuildSandbox.EnvSpec.t;
  buildenvspec : BuildSandbox.EnvSpec.t;
}

val defaultDepspecForAll : BuildSandbox.DepSpec.t
val defaultDepspecForLinked : BuildSandbox.DepSpec.t

val default : t
