open EsyPackageConfig;

module CachePaths = {
  let key = dist => Digest.to_hex(Digest.string(Dist.show(dist)));

  let fetchedDist = (sandbox, dist) =>
    Path.(SandboxSpec.distPath(sandbox) / key(dist));

  let cachedDist = (cfg, dist) =>
    Path.(cfg.Config.sourceFetchPath / key(dist));
};

/* dist which is fetched */
type fetchedDist =
  /* no sources, corresponds to Dist.NoSource */
  | Empty
  /* cached source path which could be safely removed */
  | Path(Path.t)
  /* source path from some local package, should be retained */
  | SourcePath(Path.t)
  /* downloaded tarball */
  | Tarball{
      tarballPath: Path.t,
      stripComponents: int,
    };

let cache = (fetched, tarballPath) =>
  RunAsync.Syntax.(
    switch (fetched) {
    | Empty =>
      let%bind unpackPath = Fs.randomPathVariation(tarballPath);
      let%bind tempTarballPath = Fs.randomPathVariation(tarballPath);
      let%bind () = Fs.createDir(unpackPath);
      let%bind () = Tarball.create(~filename=tempTarballPath, unpackPath);
      let%bind () = Fs.rename(~src=tempTarballPath, tarballPath);
      let%bind () = Fs.rmPath(unpackPath);
      return(Tarball({tarballPath, stripComponents: 0}));
    | SourcePath(path) =>
      let%bind tempTarballPath = Fs.randomPathVariation(tarballPath);
      let%bind () = Tarball.create(~filename=tempTarballPath, path);
      let%bind () = Fs.rename(~src=tempTarballPath, tarballPath);
      return(Tarball({tarballPath, stripComponents: 0}));
    | Path(path) =>
      let%bind tempTarballPath = Fs.randomPathVariation(tarballPath);
      let%bind () = Tarball.create(~filename=tempTarballPath, path);
      let%bind () = Fs.rename(~src=tempTarballPath, tarballPath);
      let%bind () = Fs.rmPath(path);
      return(Tarball({tarballPath, stripComponents: 0}));
    | Tarball(info) =>
      let%bind tempTarballPath = Fs.randomPathVariation(tarballPath);
      let%bind unpackPath = Fs.randomPathVariation(info.tarballPath);
      let%bind () =
        Tarball.unpack(~stripComponents=1, ~dst=unpackPath, info.tarballPath);
      let%bind () = Tarball.create(~filename=tempTarballPath, unpackPath);
      let%bind () = Fs.rename(~src=tempTarballPath, tarballPath);
      let%bind () = Fs.rmPath(info.tarballPath);
      let%bind () = Fs.rmPath(unpackPath);
      return(Tarball({tarballPath, stripComponents: 0}));
    }
  );

let ofCachedTarball = path =>
  Tarball({tarballPath: path, stripComponents: 0});
let ofDir = path => SourcePath(path);

let fetch' = (sandbox, dist) => {
  open RunAsync.Syntax;
  let tempPath = SandboxSpec.tempPath(sandbox);
  switch (dist) {
  | Dist.LocalPath({path: srcPath, manifest: _}) =>
    let srcPath = DistPath.toPath(sandbox.SandboxSpec.path, srcPath);
    return(SourcePath(srcPath));

  | Dist.NoSource => return(Empty)

  | Dist.Archive({url, checksum}) =>
    let path = CachePaths.fetchedDist(sandbox, dist);
    Fs.withTempDir(
      ~tempPath,
      stagePath => {
        let%bind () = Fs.createDir(stagePath);
        let tarballPath = Path.(stagePath / "archive");
        let%bind () = Curl.download(~output=tarballPath, url);
        let%bind () = Checksum.checkFile(~path=tarballPath, checksum);
        let%bind () = Fs.createDir(Path.parent(path));
        let%bind () = Fs.rename(~src=tarballPath, path);
        return(Tarball({tarballPath: path, stripComponents: 1}));
      },
    );

  | Dist.Github(github) =>
    let path = CachePaths.fetchedDist(sandbox, dist);
    let%bind () = Fs.createDir(Path.parent(path));
    Fs.withTempDir(
      ~tempPath,
      stagePath => {
        let%bind () = Fs.createDir(stagePath);
        let tarballPath = Path.(stagePath / "archive.tgz");
        let url =
          Printf.sprintf(
            "https://api.github.com/repos/%s/%s/tarball/%s",
            github.user,
            github.repo,
            github.commit,
          );

        let%bind () = Curl.download(~output=tarballPath, url);
        let%bind () = Fs.rename(~src=tarballPath, path);
        return(Tarball({tarballPath: path, stripComponents: 1}));
      },
    );

  | Dist.Git(git) =>
    let path = CachePaths.fetchedDist(sandbox, dist);
    let%bind () = Fs.createDir(Path.parent(path));
    Fs.withTempDir(
      ~tempPath,
      stagePath => {
        let%bind () = Fs.createDir(stagePath);
        let%bind () = Git.clone(~dst=stagePath, ~remote=git.remote, ());
        let%bind () = Git.checkout(~ref=git.commit, ~repo=stagePath, ());
        let%bind () = Fs.rename(~src=stagePath, path);
        return(Path(path));
      },
    );
  };
};

let fetch = (_cfg, sandbox, dist) =>
  RunAsync.contextf(
    fetch'(sandbox, dist),
    "fetching dist: %a",
    Dist.pp,
    dist,
  );

/* unpack fetched dist into directory */
let unpack = (fetched, path) =>
  RunAsync.Syntax.(
    switch (fetched) {
    | Empty => Fs.createDir(path)
    | SourcePath(srcPath)
    | Path(srcPath) =>
      let%bind names = Fs.listDir(srcPath);
      let copy = name => {
        let src = Path.(srcPath / name);
        let dst = Path.(path / name);
        Fs.copyPath(~src, ~dst);
      };

      let%bind () = RunAsync.List.mapAndWait(~f=copy, names);

      return();
    | Tarball({tarballPath, stripComponents}) =>
      Tarball.unpack(~stripComponents, ~dst=path, tarballPath)
    }
  );

let fetchIntoCache = (cfg, sandbox, dist: Dist.t) => {
  open RunAsync.Syntax;
  let path = CachePaths.cachedDist(cfg, dist);
  if%bind (Fs.exists(path)) {
    return(path);
  } else {
    let%bind fetched = fetch(cfg, sandbox, dist);
    let%bind () = unpack(fetched, path);
    return(path);
  };
};
