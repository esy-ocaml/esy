module StringMap = Map.Make(String);

module PathSet = Set.Make(Path);

module ConfigPath = Config.ConfigPath;

[@deriving show]
type t = {
  root: Package.t,
  manifestInfo: list((Path.t, float)),
};

let rec resolvePackage = (pkgName: string, basedir: Path.t) => {
  let packagePath = (pkgName, basedir) =>
    Path.(basedir / "node_modules" / pkgName);
  let scopedPackagePath = (scope, pkgName, basedir) =>
    Path.(basedir / "node_modules" / scope / pkgName);
  let packagePath =
    switch (pkgName.[0]) {
    | '@' =>
      switch (String.split_on_char('/', pkgName)) {
      | [scope, pkgName] => scopedPackagePath(scope, pkgName)
      | _ => packagePath(pkgName)
      }
    | _ => packagePath(pkgName)
    };
  let rec resolve = basedir => {
    open RunAsync.Syntax;
    let packagePath = packagePath(basedir);
    if%bind (Fs.exists(packagePath)) {
      return(Some(packagePath));
    } else {
      let nextBasedir = Path.parent(basedir);
      if (nextBasedir === basedir) {
        return(None);
      } else {
        resolve(nextBasedir);
      };
    };
  };
  resolve(basedir);
};

let ofDir = (cfg: Config.t) => {
  open RunAsync.Syntax;
  let manifestInfo = ref(PathSet.empty);
  let resolutionCache = Memoize.create(~size=200);
  let resolvePackageCached = (pkgName, basedir) => {
    let key = (pkgName, basedir);
    let compute = () => resolvePackage(pkgName, basedir);
    resolutionCache(key, compute);
  };
  let packageCache = Memoize.create(~size=200);
  let rec loadPackage = (path: Path.t, stack: list(Path.t)) => {
    let addDeps =
        (~skipUnresolved=false, ~make, dependencies, prevDependencies) => {
      let resolve = (pkgName: string) =>
        switch%lwt (resolvePackageCached(pkgName, path)) {
        | Ok(Some(depPackagePath)) =>
          switch%lwt (loadPackageCached(depPackagePath, [path, ...stack])) {
          | Ok(pkg) => Lwt.return_ok((pkgName, Some(pkg)))
          | Error(err) => Lwt.return_error((pkgName, Run.formatError(err)))
          }
        | Ok(None) => Lwt.return_ok((pkgName, None))
        | Error(err) => Lwt.return_error((pkgName, Run.formatError(err)))
        };
      let%lwt dependencies =
        StringMap.bindings(dependencies)
        |> Lwt_list.map_s(((pkgName, _)) => resolve(pkgName));
      let f = dependencies =>
        fun
        | Ok((_, Some(pkg))) => [make(pkg), ...dependencies]
        | Ok((pkgName, None)) =>
          if (skipUnresolved) {
            dependencies;
          } else {
            [
              Package.InvalidDependency({
                pkgName,
                reason: "unable to resolve package",
              }),
              ...dependencies,
            ];
          }
        | Error((pkgName, reason)) => [
            Package.InvalidDependency({pkgName, reason}),
            ...dependencies,
          ];
      Lwt.return(
        ListLabels.fold_left(~f, ~init=prevDependencies, dependencies),
      );
    };
    switch%bind (Package.Manifest.ofDir(path)) {
    | Some((manifest, manifestPath)) =>
      manifestInfo := PathSet.add(manifestPath, manifestInfo^);
      let (>>=) = Lwt.(>>=);
      let%lwt dependencies =
        Lwt.return([])
        >>= addDeps(
              ~make=pkg => Package.PeerDependency(pkg),
              manifest.Package.Manifest.peerDependencies,
            )
        >>= addDeps(
              ~make=pkg => Package.Dependency(pkg),
              manifest.Package.Manifest.dependencies,
            )
        >>= addDeps(
              ~make=pkg => Package.BuildTimeDependency(pkg),
              manifest.Package.Manifest.buildTimeDependencies,
            )
        >>= addDeps(
              ~skipUnresolved=true,
              ~make=pkg => Package.OptDependency(pkg),
              manifest.optDependencies,
            )
        >>= (
          dependencies =>
            if (Path.equal(cfg.sandboxPath, path)) {
              addDeps(
                ~skipUnresolved=true,
                ~make=pkg => Package.DevDependency(pkg),
                manifest.Package.Manifest.devDependencies,
                dependencies,
              );
            } else {
              Lwt.return(dependencies);
            }
        );
      let sourceType = {
        let isRootPath = path == cfg.sandboxPath;
        let hasDepWithSourceTypeDevelopment =
          List.exists(
            fun
            | Package.Dependency(pkg)
            | Package.PeerDependency(pkg)
            | Package.BuildTimeDependency(pkg)
            | Package.OptDependency(pkg) =>
              pkg.sourceType == Package.SourceType.Development
            | Package.DevDependency(_)
            | Package.InvalidDependency(_) => false,
            dependencies,
          );
        switch (
          isRootPath,
          hasDepWithSourceTypeDevelopment,
          manifest._resolved,
        ) {
        | (true, _, _) => Package.SourceType.Root
        | (_, true, _) => Package.SourceType.Development
        | (_, _, None) => Package.SourceType.Development
        | (_, _, Some(_)) => Package.SourceType.Immutable
        };
      };
      let%bind sourcePath = {
        let linkPath = Path.(path / "_esylink");
        if%bind (Fs.exists(linkPath)) {
          let%bind path = Fs.readFile(linkPath);
          path
          |> String.trim
          |> Path.of_string
          |> Run.liftOfBosError
          |> RunAsync.liftOfRun;
        } else {
          return(path);
        };
      };
      let pkg = {
        let esy =
          Std.Option.orDefault(Package.EsyManifest.empty, manifest.esy);
        Package.{
          id: Path.to_string(sourcePath),
          name: manifest.name,
          version: manifest.version,
          dependencies,
          buildCommands: esy.build,
          installCommands: esy.install,
          buildType: esy.buildsInSource,
          sourceType,
          exportedEnv: esy.exportedEnv,
          sourcePath: ConfigPath.ofPath(cfg, sourcePath),
          resolution: manifest._resolved,
        };
      };
      return(pkg);
    | None => error("unable to find manifest")
    };
  }
  and loadPackageCached = (path: Path.t, stack) =>
    if (List.mem(path, stack)) {
      error("circular dependency");
    } else {
      let compute = () => loadPackage(path, stack);
      packageCache(path, compute);
    };
  let%bind root = loadPackageCached(cfg.sandboxPath, []);
  let%bind manifestInfo =
    manifestInfo^
    |> PathSet.elements
    |> List.map(path => {
         let%bind stat = Fs.stat(path);
         return((path, stat.Unix.st_mtime));
       })
    |> RunAsync.joinAll;
  let sandbox = {root, manifestInfo};
  return(sandbox);
};
