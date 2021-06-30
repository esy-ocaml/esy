open EsyPackageConfig;

module Repr: {
  type t;
  include S.JSONABLE with type t := t;
};

type t;

let make:
  (
    ~ocamlPkgName: string,
    ~packageId: PackageId.t,
    ~build: BuildManifest.t,
    ~mode: BuildSpec.mode,
    ~platform: System.Platform.t,
    ~arch: System.Arch.t,
    ~sandboxEnv: BuildEnv.t,
    ~dependencies: list(t),
    ~buildCommands: BuildManifest.commands,
    unit
  ) =>
  (t, Repr.t);

include S.PRINTABLE with type t := t;
include S.JSONABLE with type t := t;
include S.COMPARABLE with type t := t;

module Set: Set.S with type elt := t;
