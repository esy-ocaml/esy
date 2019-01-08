module String = Astring.String;

module OpamPathsByVersion =
  Memoize.Make({
    type key = OpamPackage.Name.t;
    type value = RunAsync.t(option(OpamPackage.Version.Map.t(Path.t)));
  });

type t = {
  init: unit => RunAsync.t(registry),
  lock: Lwt_mutex.t,
  mutable registry: option(registry),
}
and registry = {
  version: option(OpamVersion.t),
  repoPath: Path.t,
  overrides: OpamOverrides.t,
  pathsCache: OpamPathsByVersion.t,
  opamCache: OpamManifest.File.Cache.t,
};

let readOpamFileOfRegistry = (res, registry) => {
  let path = Path.(OpamResolution.path(res) / "opam");
  OpamManifest.File.ofPath(
    ~upgradeIfOpamVersionIsLessThan=?registry.version,
    ~cache=registry.opamCache,
    path,
  );
};

let readUrlFileOfRegistry = (res, _registry) => {
  open RunAsync.Syntax;
  let path = Path.(OpamResolution.path(res) / "url");
  if%bind (Fs.exists(path)) {
    let%bind data = Fs.readFile(path);
    return(Some(OpamFile.URL.read_from_string(data)));
  } else {
    return(None);
  };
};

let make = (~cfg, ()) => {
  let init = () => {
    open RunAsync.Syntax;
    let%bind repoPath =
      switch (cfg.Config.opamRepository) {
      | Config.Local(local) => return(local)
      | [@implicit_arity] Config.Remote(remote, local) =>
        let update = () => {
          let%lwt () =
            Logs_lwt.app(m => m("checking %s for updates...", remote));
          let%bind () =
            Git.ShallowClone.update(~branch="master", ~dst=local, remote);
          return(local);
        };

        if (cfg.skipRepositoryUpdate) {
          if%bind (Fs.exists(local)) {
            return(local);
          } else {
            update();
          };
        } else {
          update();
        };
      };

    let%bind overrides = OpamOverrides.init(~cfg, ());

    let%bind repo = {
      let path = Path.(repoPath / "repo");
      let%bind data = Fs.readFile(path);
      let filename = OpamFile.make(OpamFilename.of_string(Path.show(path)));
      let repo = OpamFile.Repo.read_from_string(~filename, data);
      return(repo);
    };

    return({
      version: OpamFile.Repo.opam_version(repo),
      repoPath,
      pathsCache: OpamPathsByVersion.make(),
      opamCache: OpamManifest.File.Cache.make(),
      overrides,
    });
  };
  {init, lock: Lwt_mutex.create(), registry: None};
};

let initRegistry = (registry: t) => {
  let init = () =>
    RunAsync.Syntax.(
      switch (registry.registry) {
      | Some(v) => return(v)
      | None =>
        let%bind v = registry.init();
        registry.registry = Some(v);
        return(v);
      }
    );

  Lwt_mutex.with_lock(registry.lock, init);
};

let getPackageVersionIndex = (registry: registry, ~name: OpamPackage.Name.t) => {
  open RunAsync.Syntax;
  let f = () => {
    let path =
      Path.(
        registry.repoPath / "packages" / OpamPackage.Name.to_string(name)
      );
    if%bind (Fs.exists(path)) {
      let%bind entries = Fs.listDir(path);
      let f = (index, entry) => {
        let version =
          switch (String.cut(~sep=".", entry)) {
          | None => OpamPackage.Version.of_string("")
          | Some((_name, version)) => OpamPackage.Version.of_string(version)
          };

        OpamPackage.Version.Map.add(version, Path.(path / entry), index);
      };

      return(
        Some(
          List.fold_left(~init=OpamPackage.Version.Map.empty, ~f, entries),
        ),
      );
    } else {
      return(None);
    };
  };

  OpamPathsByVersion.compute(registry.pathsCache, name, f);
};

let findPackagePath = ((name, version), registry) => {
  open RunAsync.Syntax;
  switch%bind (getPackageVersionIndex(registry, ~name)) {
  | None =>
    errorf("no opam package %s found", OpamPackage.Name.to_string(name))
  | Some(index) =>
    switch (OpamPackage.Version.Map.find_opt(version, index)) {
    | None =>
      errorf(
        "no opam package %s@%s found",
        OpamPackage.Name.to_string(name),
        OpamPackage.Version.to_string(version),
      )
    | Some(path) => return(path)
    }
  };
};

let resolve =
    (
      ~ocamlVersion=?,
      ~name: OpamPackage.Name.t,
      ~version: OpamPackage.Version.t,
      registry: registry,
    ) => {
  open RunAsync.Syntax;
  let%bind path = findPackagePath((name, version), registry);
  let res = OpamResolution.make(name, version, path);
  let%bind available = {
    let env = (var: OpamVariable.Full.t) => {
      let scope = OpamVariable.Full.scope(var);
      let name = OpamVariable.Full.variable(var);
      let v =
        Option.Syntax.(
          OpamVariable.(
            switch (scope, OpamVariable.to_string(name)) {
            | (OpamVariable.Full.Global, "preinstalled") =>
              return(bool(false))
            | (OpamVariable.Full.Global, "compiler")
            | (OpamVariable.Full.Global, "ocaml-version") =>
              let%bind ocamlVersion = ocamlVersion;
              return(string(OpamPackage.Version.to_string(ocamlVersion)));
            | (OpamVariable.Full.Global, _) => None
            | (OpamVariable.Full.Self, _) => None
            | (OpamVariable.Full.Package(_), _) => None
            }
          )
        );
      v;
    };

    let%bind opam = readOpamFileOfRegistry(res, registry);
    let formula = OpamFile.OPAM.available(opam);
    let available = OpamFilter.eval_to_bool(~default=true, env, formula);
    return(available);
  };

  if (available) {
    return(Some(res));
  } else {
    return(None);
  };
};

/* Some opam packages don't make sense for esy. */
let isEnabledForEsy = name =>
  switch (OpamPackage.Name.to_string(name)) {
  | "ocaml-system" => false
  | _ => true
  };

let versions = (~ocamlVersion=?, ~name: OpamPackage.Name.t, registry) =>
  RunAsync.Syntax.(
    if (!isEnabledForEsy(name)) {
      return([]);
    } else {
      let%bind registry = initRegistry(registry);
      switch%bind (getPackageVersionIndex(registry, ~name)) {
      | None => return([])
      | Some(index) =>
        let%bind resolutions = {
          let getPackageVersion = version =>
            resolve(~ocamlVersion?, ~name, ~version, registry);

          RunAsync.List.mapAndJoin(
            ~concurrency=2,
            ~f=((version, _path)) => getPackageVersion(version),
            OpamPackage.Version.Map.bindings(index),
          );
        };

        return(List.filterNone(resolutions));
      };
    }
  );

let version = (~name: OpamPackage.Name.t, ~version, registry) =>
  RunAsync.Syntax.(
    if (!isEnabledForEsy(name)) {
      return(None);
    } else {
      let%bind registry = initRegistry(registry);
      switch%bind (resolve(~name, ~version, registry)) {
      | None => return(None)
      | Some(res) =>
        let%bind manifest = {
          let%bind opam = readOpamFileOfRegistry(res, registry);
          let%bind url =
            switch (OpamFile.OPAM.url(opam)) {
            | Some(url) => return(Some(url))
            | None => readUrlFileOfRegistry(res, registry)
            };

          return({
            OpamManifest.name,
            version,
            opam,
            url,
            override: None,
            opamRepositoryPath: Some(OpamResolution.path(res)),
          });
        };

        switch%bind (OpamOverrides.find(~name, ~version, registry.overrides)) {
        | None => return(Some(manifest))
        | Some(override) =>
          let manifest = {
            ...manifest,
            OpamManifest.override: Some(override),
          };
          return(Some(manifest));
        };
      };
    }
  );
