module type STRING = {
  type t;
  type ctx;

  let v: string => t;
  let render: (ctx, t) => string;

  let concat: (string, list(t)) => t;

  include S.PRINTABLE with type t := t;
  include S.COMPARABLE with type t := t;
  include S.JSONABLE with type t := t;
};

module type PATH = {
  type t;
  type ctx;

  let v: string => t;
  let (/): (t, string) => t;

  include S.PRINTABLE with type t := t;
  include S.COMPARABLE with type t := t;
  include S.JSONABLE with type t := t;

  let ofPath: (ctx, Path.t) => t;
  let toPath: (ctx, t) => Path.t;
};

module type STRING_CORE = {
  type ctx;
  let render: (ctx, string) => string;
};

module String = {
  module Make = (Core: STRING_CORE) : (STRING with type ctx = Core.ctx) => {
    type t = string;
    type ctx = Core.ctx;

    let v = v => v;
    let render = Core.render;

    let concat = String.concat;

    let show = v => v;
    let pp = Fmt.string;

    let compare = String.compare;

    let of_yojson = Json.Decode.string;
    let to_yojson = v => `String(v);
  };
};
