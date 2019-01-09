open EsyBuild;
open Cmdliner;

[@deriving (show, to_yojson)]
type t = {
  mainprg: string,
  esyVersion: string,
  spec: EsyInstall.SandboxSpec.t,
  prefixPath: option(Path.t),
  cachePath: option(Path.t),
  cacheTarballsPath: option(Path.t),
  opamRepository: option(EsySolve.Config.checkoutCfg),
  esyOpamOverride: option(EsySolve.Config.checkoutCfg),
  npmRegistry: option(string),
  solveTimeout: option(float),
  skipRepositoryUpdate: bool,
  solveCudfCommand: option(Cmd.t),
};

let storePath = cfg => {
  let storePath =
    switch (cfg.prefixPath) {
    | None => EsyBuildPackage.Config.StorePathDefault
    | Some(path) => EsyBuildPackage.Config.StorePathOfPrefix(path)
    };

  Run.ofBosError(EsyBuildPackage.Config.(configureStorePath(storePath)));
};

module FindProject = {
  module Kind = {
    type t =
      | ProjectForced
      | Project
      | NoProject;

    let (+) = (a, b) =>
      switch (a, b) {
      | (ProjectForced, _)
      | (_, ProjectForced) => ProjectForced
      | (Project, _)
      | (_, Project) => Project
      | _ => NoProject
      };
  };

  let isProjectPath = path => {
    let rec check = items =>
      switch (items) {
      | [] => Kind.NoProject
      | [name, ...rest] =>
        switch (name) {
        | ".esyproject" => ProjectForced
        | "package.json"
        | "esy.json" => Kind.(Project + check(rest))
        | "opam" =>
          /* opam could easily by a directory name */
          let p = Path.(path / name);
          if (!Sys.is_directory(Path.(show(p)))) {
            Kind.(Project + check(rest));
          } else {
            check(rest);
          };
        | name =>
          let p = Path.(path / name);
          if (Path.hasExt(".opam", p) && !Sys.is_directory(Path.(show(p)))) {
            Kind.(Project + check(rest));
          } else {
            check(rest);
          };
        }
      };

    check(Array.to_list(Sys.readdir(Path.show(path))));
  };

  let climbFrom = currentPath => {
    open Run.Syntax;

    let parentPath = path => {
      let parent = Path.parent(path);
      if (!(Path.compare(path, parent) == 0)) {
        return(Path.parent(path));
      } else {
        errorf(
          "No esy project found (was looking from %a and up)",
          Path.ppPretty,
          currentPath,
        );
      };
    };

    let rec climb = path => {
      let kind = isProjectPath(path);
      switch (kind) {
      | NoProject =>
        let%bind parent = parentPath(path);
        climb(parent);
      | Project =>
        let next = {
          let%bind parent = parentPath(path);
          climb(parent);
        };
        switch (next) {
        | Error(_) => return((kind, path))
        | Ok((Kind.Project | NoProject, _)) => return((kind, path))
        | Ok((ProjectForced, path)) => return((Kind.ProjectForced, path))
        };
      | ProjectForced => return((kind, path))
      };
    };

    let%bind (_kind, path) = climb(currentPath);
    return(path);
  };

  let ofPath = projectPath => {
    open Run.Syntax;

    /* check if we can get projectPath from env */
    let projectPath =
      switch (projectPath) {
      | Some(_) => projectPath
      | None =>
        let v =
          StringMap.find_opt(
            BuildSandbox.EsyIntrospectionEnv.rootPackageConfigPath,
            System.Environment.current,
          );
        switch (v) {
        | None => None
        | Some(v) => Some(Path.v(v))
        };
      };

    let%bind projectPath =
      switch (projectPath) {
      | Some(path) => return(path)
      | None => climbFrom(Path.currentPath())
      };

    if (Path.isAbs(projectPath)) {
      return(Path.normalize(projectPath));
    } else {
      return(Path.(normalize(currentPath() /\/ projectPath)));
    };
  };
};

let commonOptionsSection = Manpage.s_common_options;

let projectPath = {
  let doc = "Specifies esy project path.";
  let env = Arg.env_var("ESY__PROJECT", ~doc);
  Arg.(
    value
    & opt(some(Cli.pathConv), None)
    & info(["P", "project-path"], ~env, ~docs=commonOptionsSection, ~doc)
  );
};

let prefixPath = {
  let doc = "Specifies esy prefix path.";
  let env = Arg.env_var("ESY__PREFIX", ~doc);
  Arg.(
    value
    & opt(some(Cli.pathConv), None)
    & info(["prefix-path"], ~env, ~docs=commonOptionsSection, ~doc)
  );
};

let opamRepositoryArg = {
  let doc = "Specifies an opam repository to use.";
  let docv = "REMOTE[:LOCAL]";
  let env = Arg.env_var("ESYI__OPAM_REPOSITORY", ~doc);
  Arg.(
    value
    & opt(some(Cli.checkoutConv), None)
    & info(
        ["opam-repository"],
        ~env,
        ~doc,
        ~docv,
        ~docs=commonOptionsSection,
      )
  );
};

let esyOpamOverrideArg = {
  let doc = "Specifies an opam override repository to use.";
  let docv = "REMOTE[:LOCAL]";
  let env = Arg.env_var("ESYI__OPAM_OVERRIDE", ~doc);
  Arg.(
    value
    & opt(some(Cli.checkoutConv), None)
    & info(
        ["opam-override-repository"],
        ~env,
        ~doc,
        ~docv,
        ~docs=commonOptionsSection,
      )
  );
};

let cacheTarballsPath = {
  let doc = "Specifies tarballs cache directory.";
  Arg.(
    value
    & opt(some(Cli.pathConv), None)
    & info(["cache-tarballs-path"], ~doc, ~docs=commonOptionsSection)
  );
};

let npmRegistryArg = {
  let doc = "Specifies npm registry to use.";
  let env = Arg.env_var("NPM_CONFIG_REGISTRY", ~doc);
  Arg.(
    value
    & opt(some(string), None)
    & info(["npm-registry"], ~env, ~doc, ~docs=commonOptionsSection)
  );
};

let solveTimeoutArg = {
  let doc = "Specifies timeout for running depsolver.";
  Arg.(
    value
    & opt(some(float), None)
    & info(["solve-timeout"], ~doc, ~docs=commonOptionsSection)
  );
};

let skipRepositoryUpdateArg = {
  let doc = "Skip updating opam-repository and esy-opam-overrides repositories.";
  Arg.(
    value
    & flag
    & info(["skip-repository-update"], ~doc, ~docs=commonOptionsSection)
  );
};

let cachePathArg = {
  let doc = "Specifies cache directory..";
  let env = Arg.env_var("ESYI__CACHE", ~doc);
  Arg.(
    value
    & opt(some(Cli.pathConv), None)
    & info(["cache-path"], ~env, ~doc, ~docs=commonOptionsSection)
  );
};

let solveCudfCommandArg = {
  let doc = "Set command which is used for solving CUDF problems.";
  let env = Arg.env_var("ESY__SOLVE_CUDF_COMMAND", ~doc);
  Arg.(
    value
    & opt(some(Cli.cmdConv), None)
    & info(["solve-cudf-command"], ~env, ~doc, ~docs=commonOptionsSection)
  );
};

let make =
    (
      projectPath,
      mainprg,
      prefixPath,
      cachePath,
      cacheTarballsPath,
      opamRepository,
      esyOpamOverride,
      npmRegistry,
      solveTimeout,
      skipRepositoryUpdate,
      solveCudfCommand,
    ) => {
  open RunAsync.Syntax;

  let%bind projectPath = RunAsync.ofRun(FindProject.ofPath(projectPath));
  let%bind spec = EsyInstall.SandboxSpec.ofPath(projectPath);

  let%bind prefixPath =
    switch (prefixPath) {
    | Some(prefixPath) => return(Some(prefixPath))
    | None =>
      let%bind rc = EsyRc.ofPath(spec.EsyInstall.SandboxSpec.path);
      return(rc.EsyRc.prefixPath);
    };

  return({
    mainprg,
    esyVersion: EsyRuntime.version,
    spec,
    prefixPath,
    cachePath,
    cacheTarballsPath,
    opamRepository,
    esyOpamOverride,
    npmRegistry,
    solveTimeout,
    skipRepositoryUpdate,
    solveCudfCommand,
  });
};

let promiseTerm = {
  let parse =
      (
        mainprg,
        projectPath,
        prefixPath,
        cachePath,
        cacheTarballsPath,
        opamRepository,
        esyOpamOverride,
        npmRegistry,
        solveTimeout,
        skipRepositoryUpdate,
        solveCudfCommand,
        (),
      ) =>
    make(
      projectPath,
      mainprg,
      prefixPath,
      cachePath,
      cacheTarballsPath,
      opamRepository,
      esyOpamOverride,
      npmRegistry,
      solveTimeout,
      skipRepositoryUpdate,
      solveCudfCommand,
    );

  Cmdliner.Term.(
    const(parse)
    $ main_name
    $ projectPath
    $ prefixPath
    $ cachePathArg
    $ cacheTarballsPath
    $ opamRepositoryArg
    $ esyOpamOverrideArg
    $ npmRegistryArg
    $ solveTimeoutArg
    $ skipRepositoryUpdateArg
    $ solveCudfCommandArg
    $ Cli.setupLogTerm
  );
};

let term =
  Cmdliner.Term.(ret(const(Cli.runAsyncToCmdlinerRet) $ promiseTerm));
