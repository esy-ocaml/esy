(**

  This represents symbolic paths used in dists/sources/source-specs.

  They are always rendered using forward slashes (/), unix-like.

  [DistPath.t] values are relative to some base [Path.t] value.

 *)

type t

include S.JSONABLE with type t := t
include S.COMPARABLE with type t := t

val v : string -> t

val toPath : Path.t -> t -> Path.t
(** [toPath base p] converts [p] to [Path.t] by rebasing on top of [base]. *)

val ofPath : Path.t -> t

val rebase : base:t -> t -> t

val sexp_of_t : t -> Sexplib0.Sexp.t

val show : t -> string
val showPretty : t -> string
