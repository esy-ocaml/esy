type t = pri {
  fastreplacestringPath: Fpath.t,
  projectPath: Fpath.t,
  sandboxPath: Fpath.t,
  storePath: Fpath.t,
  localStorePath: Fpath.t,
};

let make : (
  ~fastreplacestringPath: Fpath.t=?,
  ~projectPath: Fpath.t=?,
  ~sandboxPath: Fpath.t=?,
  ~storePath: Fpath.t=?,
  unit
) => Run.t(t, _);

type config = t

/* Config parametrized string value */
module Value: {
  include EsyLib.Abstract.STRING with type ctx = config;
  let store : t;
  let localStore : t;
  let project : t;
};

module Path: {
  include EsyLib.Abstract.PATH with type ctx = config;
  let toValue: t => Value.t;
  let store : t;
  let localStore : t;
  let project : t;
}

module Environment : EsyLib.Environment.S
  with type value = Value.t
  and type ctx = t
