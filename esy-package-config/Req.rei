/**

  Request for a package, a pair of package name and a version spec (constraint
  or source spec).


 */;

type t =
  pri {
    name: string,
    spec: VersionSpec.t,
  };

include S.COMPARABLE with type t := t;
include S.PRINTABLE with type t := t;

let to_yojson: Json.encoder(t);

let parse: string => result(t, string);
let name: t => string;

let make: (~name: string, ~spec: VersionSpec.t) => t;

module Map: Map.S with type key := t;
module Set: Set.S with type elt := t;
