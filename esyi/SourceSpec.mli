(**
 * This is a spec for a source, which at some point will be resolved to a
 * concrete source Source.t.
 *)

include module type of Metadata.SourceSpec

include S.PRINTABLE with type t := t

val to_yojson : t -> [> `String of string ]
val ofSource : Source.t -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val matches : source:Source.t -> t -> bool

val parser : t Parse.t
val parse : string -> (t, string) result

module Map : Map.S with type key := t
module Set : Set.S with type elt := t
