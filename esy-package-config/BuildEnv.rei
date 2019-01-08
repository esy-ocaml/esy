type t = StringMap.t(item)
and item = {
  name: string,
  value: string,
};

let empty: t;

include S.COMPARABLE with type t := t;
include S.JSONABLE with type t := t;
include S.PRINTABLE with type t := t;

module Override: {
  type t = StringMap.Override.t(item);

  include S.COMPARABLE with type t := t;
  include S.PRINTABLE with type t := t;
  include S.JSONABLE with type t := t;
};
