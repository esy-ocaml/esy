type t('v, 'err) = result('v, 'err) = | Ok('v) | Error('err);

let return = v => Ok(v);
let error = err => Error(err);
let errorf = fmt => {
  let kerr = _ => Error(Format.flush_str_formatter());
  Format.kfprintf(kerr, Format.str_formatter, fmt);
};

let join = rr =>
  switch (rr) {
  | Ok(Ok(v)) => Ok(v)
  | Ok(v) => v
  | Error(msg) => Error(msg)
  };

let map = (~f) =>
  fun
  | Ok(v) => Ok(f(v))
  | Error(err) => Error(err);

let isOk =
  fun
  | Ok(_) => true
  | Error(_) => false;

let isError =
  fun
  | Ok(_) => false
  | Error(_) => true;

let getOr = onError =>
  fun
  | Ok(v) => v
  | Error(_) => onError;

module Syntax = {
  let return = return;
  let error = error;
  let errorf = errorf;

  let (>>) = (v, f) => {
    switch (v) {
    | Ok () => f()
    | Error(err) => Error(err)
    };
  };

  module Let_syntax = {
    let map = map;
    let bind = (~f, v) =>
      switch (v) {
      | Ok(v) => f(v)
      | Error(e) => Error(e)
      };
    module Open_on_rhs = {
      let return = v => Ok(v);
    };
  };
};

module List = {
  let map =
      (~f: 'a => result('b, 'err), xs: list('a)): result(list('b), 'err) => {
    let f = (prev, x) =>
      switch (prev) {
      | Ok(xs) =>
        switch (f(x)) {
        | Ok(x) => Ok([x, ...xs])
        | Error(err) => Error(err)
        }
      | error => error
      };
    xs |> List.fold_left(~f, ~init=Ok([])) |> map(~f=List.rev);
  };
  let iter =
      (~f: 'a => result(unit, 'err), xs: list('a)): result(unit, 'err) => {
    let f = (prev, x) =>
      switch (prev) {
      | Ok () =>
        switch (f(x)) {
        | Ok () => Ok()
        | Error(err) => Error(err)
        }
      | Error(err) => Error(err)
      };
    xs |> List.fold_left(~f, ~init=Ok());
  };
  let foldLeft = (~f: ('a, 'b) => result('a, 'e), ~init: 'a, xs: list('b)) => {
    let rec fold = (acc, xs) =>
      switch (acc, xs) {
      | (Error(err), _) => Error(err)
      | (Ok(acc), []) => Ok(acc)
      | (Ok(acc), [x, ...xs]) => fold(f(acc, x), xs)
      };
    fold(Ok(init), xs);
  };
  let filter = (~f, xs) => {
    open Syntax;
    let f = (xs, x) =>
      if%bind (f(x)) {
        return([x, ...xs]);
      } else {
        return(xs);
      };
    let%bind xs = foldLeft(~f, ~init=[], xs);
    return(List.rev(xs));
  };
};
