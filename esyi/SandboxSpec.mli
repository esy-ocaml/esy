type t = {
  path : Path.t;
  manifest : ManifestSpec.t;
}

include S.PRINTABLE with type t := t
include S.COMPARABLE with type t := t

module Set : Set.S with type elt = t
module Map : Map.S with type key = t

val doesPathReferToConcreteManifest : Path.t -> bool
val isDefault : t -> bool

val cachePath : t -> Path.t
val storePath : t -> Path.t
val buildPath : t -> Path.t
val nodeModulesPath : t -> Path.t
val lockfilePath : t -> Path.t

val ofPath : Path.t -> t RunAsync.t
