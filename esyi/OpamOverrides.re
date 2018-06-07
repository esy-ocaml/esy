module PackageNameMap = Map.Make(OpamFile.PackageName);

module Override = {
  module Opam = {
    [@deriving of_yojson]
    type t = {
      source: [@default None] option(source),
      files,
    }
    and source = {
      url: string,
      checksum: string,
    }
    and files = list(file)
    and file = {
      name: Path.t,
      content: string,
    };

    let empty = {source: None, files: []};
  };

  module Command = {
    type t = list(string);

    let of_yojson = (json: Json.t) =>
      switch (json) {
      | `List(_) => Json.Parse.(list(string, json))
      | `String(cmd) => Ok([cmd])
      | _ => Error("expected either a list or a string")
      };
  };

  [@deriving of_yojson]
  type t = {
    build: [@default None] option(list(Command.t)),
    install: [@default None] option(list(Command.t)),
    dependencies:
      [@default PackageInfo.Dependencies.empty] PackageInfo.Dependencies.t,
    peerDependencies:
      [@default PackageInfo.Dependencies.empty] PackageInfo.Dependencies.t,
    exportedEnv:
      [@default PackageJson.ExportedEnv.empty] PackageJson.ExportedEnv.t,
    opam: [@default Opam.empty] Opam.t,
  };
};

type t = PackageNameMap.t(list((OpamFile.Formula.t, Fpath.t)));

type override = Override.t;

let rec yamlToJson = value =>
  switch (value) {
  | `A(items) => `List(List.map(yamlToJson, items))
  | `O(items) =>
    `Assoc(List.map(((name, value)) => (name, yamlToJson(value)), items))
  | `String(s) => `String(s)
  | `Float(s) => `Float(s)
  | `Bool(b) => `Bool(b)
  | `Null => `Null
  };

let init = (~cfg, ()) : RunAsync.t(t) => {
  open RunAsync.Syntax;

  let packagesDir = Path.(cfg.Config.esyOpamOverrideCheckoutPath / "packages");

  let%bind () =
    Git.ShallowClone.update(
      ~branch="4",
      ~dst=cfg.Config.esyOpamOverrideCheckoutPath,
      "https://github.com/esy-ocaml/esy-opam-override",
    );

  let%bind names = Fs.listDir(packagesDir);
  module String = Astring.String;

  let parseOverrideSpec = spec =>
    switch (String.cut(~sep=".", spec)) {
    | None => (OpamFile.PackageName.ofString(spec), OpamVersion.Formula.ANY)
    | Some((name, constr)) =>
      let constr =
        String.map(
          fun
          | '_' => ' '
          | c => c,
          constr,
        );
      let constr = OpamVersion.Formula.parse(constr);
      (OpamFile.PackageName.ofString(name), constr);
    };

  let overrides = {
    let f = (overrides, dirName) => {
      let (name, formula) = parseOverrideSpec(dirName);
      let items =
        switch (PackageNameMap.find_opt(name, overrides)) {
        | Some(items) => items
        | None => []
        };
      PackageNameMap.add(
        name,
        [(formula, Path.(packagesDir / dirName)), ...items],
        overrides,
      );
    };
    ListLabels.fold_left(~f, ~init=PackageNameMap.empty, names);
  };

  return(overrides);
};

let load = baseDir => {
  open RunAsync.Syntax;
  let packageJson = Path.(baseDir / "package.json");
  let packageYaml = Path.(baseDir / "package.yaml");
  if%bind (Fs.exists(packageJson)) {
    RunAsync.withContext(
      "Reading " ++ Path.toString(packageJson),
      {
        let%bind json = Fs.readJsonFile(packageJson);
        RunAsync.ofRun(Json.parseJsonWith(Override.of_yojson, json));
      },
    );
  } else {
    RunAsync.withContext(
      "Reading " ++ Path.toString(packageYaml),
      if%bind (Fs.exists(packageYaml)) {
        let%bind data = Fs.readFile(packageYaml);
        let%bind yaml =
          Yaml.of_string(data) |> Run.ofBosError |> RunAsync.ofRun;
        let json = yamlToJson(yaml);
        RunAsync.ofRun(Json.parseJsonWith(Override.of_yojson, json));
      } else {
        error(
          "must have either package.json or package.yaml "
          ++ Path.toString(baseDir),
        );
      },
    );
  };
};

let get = (overrides, name: OpamFile.PackageName.t, version) =>
  RunAsync.Syntax.(
    switch (PackageNameMap.find_opt(name, overrides)) {
    | Some(items) =>
      switch (
        List.find_opt(
          ((formula, _path)) =>
            OpamVersion.Formula.matches(formula, version),
          items,
        )
      ) {
      | Some((_formula, path)) =>
        let%bind override = load(path);
        return(Some(override));
      | None => return(None)
      }
    | None => return(None)
    }
  );

let apply = (manifest: OpamFile.manifest, override: Override.t) => {
  let source =
    switch (override.opam.Override.Opam.source) {
    | Some(source) => PackageInfo.Source.Archive(source.url, source.checksum)
    | None => manifest.source
    };

  let files =
    manifest.files
    @ List.map(f => Override.Opam.(f.name, f.content), override.opam.files);
  {
    ...manifest,
    build: Option.orDefault(~default=manifest.build, override.Override.build),
    install:
      Option.orDefault(~default=manifest.install, override.Override.install),
    dependencies:
      PackageInfo.Dependencies.merge(
        manifest.dependencies,
        override.Override.dependencies,
      ),
    peerDependencies:
      PackageInfo.Dependencies.merge(
        manifest.peerDependencies,
        override.Override.peerDependencies,
      ),
    files,
    source,
    exportedEnv: override.Override.exportedEnv,
  };
};
