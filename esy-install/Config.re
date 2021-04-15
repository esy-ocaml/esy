[@deriving show]
type t = {
  sourceArchivePath: option(Path.t),
  sourceFetchPath: Path.t,
  sourceStagePath: Path.t,
  sourceInstallPath: Path.t,
  fetchConcurrency: option(int),
};

let cacheGCRoot = (prefixPath) => {
  open RunAsync.Syntax;
  let pathToProjects = Path.(prefixPath / "projects.json");

  if%bind (Fs.exists(pathToProjects)) {
    return();
  } else {
    Fs.writeJsonFile(`List([]), pathToProjects);
  }
};

let make =
    (
      ~prefixPath=?,
      ~cacheTarballsPath=?,
      ~cacheSourcesPath=?,
      ~fetchConcurrency=?,
      (),
    ) => {
  open RunAsync.Syntax;
  let%bind prefixPath =
    RunAsync.ofRun(
      Run.Syntax.(
        switch (prefixPath) {
        | Some(prefixPath) => return(prefixPath)
        | None =>
          let userDir = Path.homePath();
          return(Path.(userDir / ".esy"));
        }
      ),
    );

  let sourcePath =
    switch (cacheSourcesPath) {
    | Some(path) => path
    | None => Path.(prefixPath / "source")
    };
  let%bind () = Fs.createDir(sourcePath);

  let%bind sourceArchivePath =
    switch (cacheTarballsPath) {
    | Some(path) =>
      let%bind () = Fs.createDir(path);
      return(Some(path));
    | None => return(None)
    };

  let sourceFetchPath = Path.(sourcePath / "f");
  let%bind () = Fs.createDir(sourceFetchPath);

  let sourceStagePath = Path.(sourcePath / "s");
  let%bind () = Fs.createDir(sourceStagePath);

  let sourceInstallPath = Path.(sourcePath / "i");
  let%bind () = Fs.createDir(sourceInstallPath);

  let%bind () = cacheGCRoot(prefixPath);

  return({
    sourceArchivePath,
    sourceFetchPath,
    sourceStagePath,
    sourceInstallPath,
    fetchConcurrency,
  });
};
