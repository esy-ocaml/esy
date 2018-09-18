(**
 * This is a spec for a source, which at some point will be resolved to a
 * concrete source Source.t.
 *)

type t =
  | Archive of {
      url : string;
      checksum : Checksum.t option;
    }
  | Git of {
      remote : string;
      ref : string option;
      manifest : SandboxSpec.ManifestSpec.t option;
    }
  | Github of {
      user : string;
      repo : string;
      ref : string option;
      manifest : SandboxSpec.ManifestSpec.t option;
    }
  | LocalPath of {
      path : Path.t;
      manifest : SandboxSpec.ManifestSpec.t option;
    }
  | LocalPathLink of {
      path : Path.t;
      manifest : SandboxSpec.ManifestSpec.t option;
    }
  | NoSource

include S.PRINTABLE with type t := t
include S.COMPARABLE with type t := t

val to_yojson : t -> [> `String of string ]
val ofSource : Source.t -> t
val matches : source:Source.t -> t -> bool

val parser : t Parse.t
val parse : string -> (t, string) result

module Map : Map.S with type key := t
module Set : Set.S with type elt := t
