module Package = EsyInstall.Package;
module SandboxPath = EsyBuildPackage.Config.Path;
module SandboxValue = EsyBuildPackage.Config.Value;

type t =
  | Host(config)
and config = {
  path: SandboxValue.t,
  destdir: SandboxValue.t,
  stdlib: SandboxValue.t,
  ldconf: SandboxValue.t,
  commands: list((string, SandboxValue.t)),
};

let isCompiler = (pkg: Package.t) => {
  let compilers = ["ocaml"];
  List.mem(pkg.name, ~set=compilers);
};

let commands = (sysroot: SandboxPath.t) => {
  let commands = [
    "ocamlc",
    "ocamlopt",
    "ocamlcp",
    "ocamlmklib",
    "ocamlmktop",
    "ocamldoc",
    "ocamldep",
    "ocamllex",
  ];
  let f = cmd => (cmd, SandboxPath.(sysroot / "bin" / cmd |> toValue));
  List.map(~f, commands);
};

let path = prefix => {
  SandboxPath.(prefix / "etc" / "findlib.conf");
};

let name = (~prefix) =>
  fun
  | Host(_) => path(prefix) |> SandboxPath.toValue;

let content = t => {
  let toConfigVar = (findlib, name, value) => {
    let field =
      switch (findlib) {
      | Host(_) => name
      };

    Printf.sprintf("%s = \"%s\"", field, SandboxValue.show(value));
  };

  switch (t) {
  | Host(findlib) =>
    String.concat(
      "\n",
      [
        toConfigVar(t, "path", findlib.path),
        toConfigVar(t, "destdir", findlib.destdir),
        toConfigVar(t, "stdlib", findlib.stdlib),
        toConfigVar(t, "ldconf", findlib.ldconf),
      ]
      @ List.map(
          ~f=((name, cmd)) => toConfigVar(t, name, cmd),
          findlib.commands,
        ),
    )
    |> SandboxValue.v
  };
};

let renderConfig = (~prefix, config) => {
  let path = name(~prefix, config);
  let content = content(config);
  {EsyBuildPackage.Plan.path, content};
};
