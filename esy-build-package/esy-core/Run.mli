(**
 * A computation which might result in an error.
 *)
type 'a t = ('a, error) result
and error

val return : 'a -> 'a t

val error : string -> 'a t

(**
 * Wrap computation with a context which will be reported in case of error
 *)
val withContext : string -> 'a t -> 'a t

(**
 * Wrap computation with a context which will be reported in case of error
 *)
val withContextOfLog : ?header:string -> string -> 'a t -> 'a t

(**
 * Format error.
 *)
val formatError : error -> string

val foldLeft : f:('a -> 'b -> 'a t) -> init:'a -> 'b list -> 'a t

val waitAll : unit t list -> unit t

module Syntax : sig

  val return : 'a -> 'a t
  val error : string -> 'a t

  module Let_syntax : sig
    val bind : f:('a -> 'b t) -> 'a t -> 'b t
  end
end


val liftOfStringError : ('a, string) result -> 'a t
val liftOfBosError : ('a, [< `Msg of string]) result -> 'a t
