module type PRINTABLE = sig
  type t

  val pp : t Fmt.t
  val show : t -> string
end

module type JSONABLE = sig
  type t

  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t, string) result
end

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type COMMON = sig
  type t

  include COMPARABLE with type t := t
  include PRINTABLE with type t := t
  include JSONABLE with type t := t
end


