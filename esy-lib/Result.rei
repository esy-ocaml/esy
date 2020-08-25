type t('v, 'err) = result('v, 'err) = | Ok('v) | Error('err);

let return: 'v => t('v, _);
let error: 'err => t(_, 'err);
let errorf: format4('a, Format.formatter, unit, t(_, string)) => 'a;

let map: (~f: 'a => 'b, result('a, 'err)) => result('b, 'err);
let join: t(t('a, 'b), 'b) => t('a, 'b);

let isOk: result(_, _) => bool;
let isError: result(_, _) => bool;

let getOr: ('a, result('a, _)) => 'a;

module List: {
  let map: (~f: 'a => t('b, 'err), list('a)) => t(list('b), 'err);
  let filter:
    (~f: 'a => result(bool, 'b), list('a)) => result(list('a), 'b);
  let iter: (~f: 'a => t(unit, 'err), list('a)) => t(unit, 'err);
  let foldLeft:
    (~f: ('a, 'b) => t('a, 'err), ~init: 'a, list('b)) => t('a, 'err);
};

module Syntax: {
  let return: 'v => t('v, _);
  let error: 'err => t(_, 'err);
  let errorf: format4('a, Format.formatter, unit, t(_, string)) => 'a;
  let (>>): (t(unit, 'err), unit => t('b, 'err)) => t('b, 'err);

  module Let_syntax: {
    let bind: (~f: 'a => t('b, 'err), t('a, 'err)) => t('b, 'err);
    let map: (~f: 'a => 'b, t('a, 'err)) => t('b, 'err);
    module Open_on_rhs: {let return: 'a => t('a, 'b);};
  };
};
