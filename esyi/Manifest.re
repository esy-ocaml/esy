let getDeps = manifest => {
  let depsByKind =
    switch (manifest) {
    | `OpamFile(opam) => OpamFile.process(opam)
    | `PackageJson(json) => PackageJson.process(json)
    };
  depsByKind;
};

let getSource = (manifest, name, version) =>
  switch (version) {
  | Solution.Version.Github(user, repo, ref) =>
    Types.PendingSource.GithubSource(user, repo, ref)
  | Solution.Version.LocalPath(path) =>
    Types.PendingSource.File(Path.toString(path))
  | _ =>
    switch (manifest) {
    | `OpamFile(opam) =>
      Types.PendingSource.WithOpamFile(
        OpamFile.getSource(opam),
        OpamFile.toPackageJson(opam, name, version),
      )
    | `PackageJson(json) => PackageJson.getSource(json)
    }
  };
