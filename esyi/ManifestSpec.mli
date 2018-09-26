module Filename : sig
  type t =
    | Esy of string
    | Opam of string

  include S.PRINTABLE with type t := t
  include S.COMPARABLE with type t := t
  include S.JSONABLE with type t := t

  val ofString : string -> (t, string) result
  val ofStringExn : string -> t
  val parser : t Parse.t
end

type t =
  | One of Filename.t
  | ManyOpam of string list


include S.PRINTABLE with type t := t
include S.COMPARABLE with type t := t
include S.JSONABLE with type t := t

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
