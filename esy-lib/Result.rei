type t('v, 'err) = result('v, 'err) =
  | Ok('v)
  | Error('err)

let map : ('a => 'b, result('a, 'err)) => result('b, 'err);
let join : t(t('a, 'b), 'b) => t('a, 'b);

module List : {
  let map: (~f: 'a => t('b, 'err), list('a)) => t(list('b), 'err);
  let foldLeft: (~f: ('a, 'b) => t('a, 'err), ~init: 'a, list('b)) => t('a, 'err);
};

module Syntax : {
  let return: 'v => t('v, _);
  let error: 'err => t(_, 'err);
  let errorf : format4('a, Format.formatter, unit, t(_, string)) => 'a;

  module Let_syntax: {
    let bind: (~f: 'a => t('b, 'err), t('a, 'err)) => t('b, 'err);
    module Open_on_rhs: {
      let return: 'a => t('a, 'b);
    };
  };
};
