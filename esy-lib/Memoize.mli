(** Parametrized API *)

type ('k, 'v) t
val make : ?size:int -> unit -> ('k, 'v) t
val compute : ('k, 'v) t -> 'k -> (unit -> 'v) -> 'v
val ensureComputed : ('k, 'v) t -> 'k -> (unit -> 'v) -> unit
val put : ('k, 'v) t -> 'k -> 'v -> unit
val get : ('k, 'v) t -> 'k -> 'v option
val values : ('k, 'v) t -> ('k * 'v) list

(** Functorized API *)

module type MEMOIZEABLE = sig
  type key
  type value
end

module type MEMOIZE = sig
  type t

  type key
  type value

  val make : ?size:int -> unit -> t
  val compute : t -> key -> (unit -> value) -> value
  val ensureComputed : t -> key -> (unit -> value) -> unit
  val put : t -> key -> value -> unit
  val get : t -> key -> value option
  val values : t -> (key * value) list
end

module Make : functor (C: MEMOIZEABLE) -> sig
  include MEMOIZE with
    type key := C.key
    and type value := C.value
end
