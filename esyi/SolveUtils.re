let satisfies = (realVersion, req) =>
  switch (req, realVersion) {
  | (
      PackageJson.DependencyRequest.Github(user, repo, ref),
      Solution.Version.Github(user_, repo_, ref_),
    )
      when user == user_ && repo == repo_ && ref == ref_ =>
    true
  | (Npm(semver), Solution.Version.Npm(s))
      when NpmVersion.Formula.matches(semver, s) =>
    true
  | (Opam(semver), Solution.Version.Opam(s))
      when OpamVersion.Formula.matches(semver, s) =>
    true
  | (LocalPath(p1), Solution.Version.LocalPath(p2)) when Path.equal(p1, p2) =>
    true
  | _ => false
  };

let rec lockDownSource = pendingSource =>
  RunAsync.Syntax.(
    switch (pendingSource) {
    | Types.PendingSource.NoSource =>
      return({Solution.Source.src: NoSource, opam: None})
    | WithOpamFile(source, opamFile) =>
      switch%bind (lockDownSource(source)) {
      | {Solution.Source.src, opam: None} =>
        return({Solution.Source.src, opam: Some(opamFile)})
      | _ => error("can't nest withOpamFiles inside each other")
      }
    | Archive(url, None) =>
      return({
        /* TODO: checksum */
        Solution.Source.src: Solution.Source.Archive(url, "fake checksum"),
        opam: None,
      })
    | Archive(url, Some(checksum)) =>
      return({
        Solution.Source.src: Solution.Source.Archive(url, checksum),
        opam: None,
      })
    | GitSource(url, ref) =>
      let ref = Option.orDefault(~default="master", ref);
      /** TODO getting HEAD */
      let%bind sha = Git.lsRemote(~remote=url, ~ref, ());
      return({
        Solution.Source.src: Solution.Source.GitSource(url, sha),
        opam: None,
      });
    | GithubSource(user, name, ref) =>
      let ref = Option.orDefault(~default="master", ref);
      let url = "git://github.com/" ++ user ++ "/" ++ name ++ ".git";
      let%bind sha = Git.lsRemote(~remote=url, ~ref, ());
      return({
        Solution.Source.src: Solution.Source.GithubSource(user, name, sha),
        opam: None,
      });
    | File(s) =>
      return({Solution.Source.src: Solution.Source.File(s), opam: None})
    }
  );

let checkRepositories = config =>
  RunAsync.Syntax.(
    {
      let%bind () =
        Git.ShallowClone.update(
          ~branch="4",
          ~dst=config.Config.esyOpamOverridePath,
          "https://github.com/esy-ocaml/esy-opam-override",
        );
      let%bind () =
        Git.ShallowClone.update(
          ~branch="master",
          ~dst=config.Config.opamRepositoryPath,
          "https://github.com/ocaml/opam-repository",
        );
      return();
    }
  );

let runSolver = (~strategy="-notuptodate", rootName, deps, universe) => {
  let root = {
    ...Cudf.default_package,
    package: rootName,
    version: 1,
    depends: deps,
  };
  Cudf.add_package(universe, root);
  let request = {
    ...Cudf.default_request,
    install: [(root.Cudf.package, Some((`Eq, root.Cudf.version)))],
  };
  let preamble = Cudf.default_preamble;
  let solution =
    Mccs.resolve_cudf(
      ~verbose=false,
      ~timeout=5.,
      strategy,
      (preamble, universe, request),
    );
  switch (solution) {
  | None => None
  | Some((_preamble, universe)) =>
    let packages = Cudf.get_packages(~filter=p => p.Cudf.installed, universe);
    Some(packages);
  };
};
