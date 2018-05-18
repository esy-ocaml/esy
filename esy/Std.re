module Result = {
  include Rresult;
  let ok = Ok();
  let join = rr =>
    switch (rr) {
    | Ok(Ok(v)) => Ok(v)
    | Ok(v) => v
    | Error(msg) => Error(msg)
    };
  let map = f =>
    fun
    | Ok(v) => Ok(f(v))
    | Error(err) => Error(err);
  let (>>) = (a, b) =>
    switch (a) {
    | Ok () => b()
    | Error(msg) => Error(msg)
    };
  module Let_syntax = {
    let bind = (~f, v) =>
      switch (v) {
      | Ok(v) => f(v)
      | Error(e) => Error(e)
      };
    module Open_on_rhs = {
      let return = v => Ok(v);
    };
  };
  let listMap =
      (~f: 'a => result('b, 'err), xs: list('a))
      : result(list('b), 'err) => {
    let f = (prev, x) =>
      switch (prev) {
      | Ok(xs) =>
        switch (f(x)) {
        | Ok(x) => Ok([x, ...xs])
        | Error(err) => Error(err)
        }
      | error => error
      };
    xs |> List.fold_left(f, Ok([])) |> map(List.rev);
  };
  let listFoldLeft =
      (~f: ('a, 'b) => result('a, 'e), ~init: 'a, xs: list('b)) => {
    let rec fold = (acc, xs) =>
      switch (acc, xs) {
      | (Error(err), _) => Error(err)
      | (Ok(acc), []) => Ok(acc)
      | (Ok(acc), [x, ...xs]) => fold(f(acc, x), xs)
      };
    fold(Ok(init), xs);
  };
};

module List = {
  include List;
  let filterNone = l => {
    let rec loop = (o, accum) =>
      switch (o) {
      | [] => accum
      | [hd, ...tl] =>
        switch (hd) {
        | Some(v) => loop(tl, [v, ...accum])
        | None => loop(tl, accum)
        }
      };
    loop(l, []);
  };
  let diff = (list1, list2) =>
    List.filter(elem => ! List.mem(elem, list2), list1);
  let intersect = (list1, list2) =>
    List.filter(elem => List.mem(elem, list2), list1);
};
