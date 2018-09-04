(**
 * This represents sandbox.
 *)

module Package : sig

  type t = {
    id : string;
    name : string;
    version : string;
    dependencies : dependency list;
    build : Manifest.Build.t;
    sourcePath : EsyBuildPackage.Config.Path.t;
    resolution : string option;
  }

  and dependency = (dependencyKind * t, dependencyError) result

  and dependencyError =
    | InvalidDependency of { name : string; message : string; }
    | MissingDependency of { name : string; }

  and dependencyKind =
    | Dependency
    | OptDependency
    | DevDependency
    | BuildTimeDependency

  val pp : t Fmt.t
  val compare : t -> t -> int

  val pp_dependency : dependency Fmt.t
  val compare_dependency : dependency -> dependency -> int

  module Graph : DependencyGraph.DependencyGraph
    with type node = t
    and type dependency := dependency

  module Map : Map.S with type key = t
end

type t = {
  name : string option;
  cfg : Config.t;
  buildConfig: EsyBuildPackage.Config.t;
  root : Package.t;
  scripts : Manifest.Scripts.t;
  env : Manifest.Env.t;
}

type info = (Path.t * float) list

(** Check if a directory is a sandbox *)
val isSandbox : Path.t -> bool RunAsync.t

(** Init sandbox from given the config *)
val make : cfg:Config.t -> Path.t -> Project.sandbox -> (t * info) RunAsync.t

val init : t -> unit RunAsync.t

module Value : module type of EsyBuildPackage.Config.Value
module Environment : module type of EsyBuildPackage.Config.Environment
module Path : module type of EsyBuildPackage.Config.Path
