/**

  This module represents the dependency graph with concrete package versions
  which was solved by solver and is ready to be fetched by the package fetcher.


  */
module Path = EsyLib.Path;

module Source = {
  [@deriving yojson]
  type t = (info, option(Types.opamFile))
  and info =
    /* url & checksum */
    | Archive(string, string)
    /* url & commit */
    | GitSource(string, string)
    | GithubSource(string, string, string)
    | File(string)
    | NoSource;
};

[@deriving yojson]
type t = {
  root: rootPackage,
  buildDependencies: list(rootPackage),
}
and rootPackage = {
  package: fullPackage,
  runtimeBag: list(fullPackage),
}
and fullPackage = {
  name: string,
  version: Lockfile.realVersion,
  source: Source.t,
  requested: Types.depsByKind,
  runtime: list(resolved),
  build: list(resolved),
}
and resolved = (string, Types.requestedDep, Lockfile.realVersion);

/* TODO: use RunAsync */
let ofFile = (filename: Path.t) => {
  let json = Yojson.Safe.from_file(Path.toString(filename));
  switch (of_yojson(json)) {
  | Error(_a) => failwith("Bad lockfile")
  | Ok(a) => a
  };
};

/* TODO: use RunAsync */
let toFile = (filename: Path.t, solution: t) => {
  let json = to_yojson(solution);
  let chan = open_out(Path.toString(filename));
  Yojson.Safe.pretty_to_channel(chan, json);
  close_out(chan);
};
