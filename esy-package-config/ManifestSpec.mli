type t =
  kind * string

and kind =
  | Esy
  | Opam

include S.PRINTABLE with type t := t
include S.COMPARABLE with type t := t
include S.JSONABLE with type t := t

val sexp_of_t : t -> Sexplib0.Sexp.t

val ofString : string -> (t, string) result
val ofStringExn : string -> t
val parser : t Parse.t

val inferPackageName : t -> string option
