open EsyBuild;
open Cmdliner;

module ProjectArg = {
  type t =
    | ByName(string)
    | ByPath(Path.t);

  let pp = fmt =>
    fun
    | ByPath(path) => Path.pp(fmt, path)
    | ByName(name) => Fmt.string(fmt, name);

  let isPathLike = v =>
    v.[0] == '.'
    || v.[0] == '/'
    || String.contains(v, '/')
    || String.contains(v, '\\');

  let parse = v =>
    Result.Syntax.(
      if (String.length(v) == 0) {
        error("empty project argument");
      } else if (isPathLike(v)) {
        return(ByPath(Path.v(v)));
      } else {
        return(ByName(v));
      }
    );

  let ofPath = p => ByPath(p);

  let conv = {
    open Cmdliner;
    let parse = v => Rresult.R.error_to_msg(~pp_error=Fmt.string, parse(v));
    Arg.conv(~docv="PROJECT", (parse, pp));
  };

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

  let is_dir = path => {
    let path = Path.show(path);
    try(Sys.is_directory(path)) {
    // Might fail because of broken symlink or etc
    | Sys_error(_) => false
    };
  };

  let classifyPath = path => {
    let rec check = items =>
      switch (items) {
      | [] => Kind.NoProject
      | [name, ...rest] =>
        switch (name) {
        | ".esyproject" => ProjectForced
        | "package.json"
        | "esy.json" => Kind.(Project + check(rest))
        | "opam" =>
          let p = Path.(path / name);
          if (!is_dir(p)) {
            Kind.(Project + check(rest));
          } else {
            check(rest);
          };
        | name =>
          let p = Path.(path / name);
          if (Path.hasExt(".opam", p) && !is_dir(p)) {
            Kind.(Project + check(rest));
          } else {
            check(rest);
          };
        }
      };

    check(Array.to_list(Sys.readdir(Path.show(path))));
  };

  let checkPathByProjectName = (path, projectName) => {
    let rec check = items =>
      switch (items) {
      | [] => None
      | [name, ...rest] =>
        let p = Path.(path / name);
        if (!Path.hasExt(".json", p)) {
          check(rest);
        } else if (String.compare(name, projectName) == 0
                   || String.compare(name, projectName ++ ".json") == 0) {
          Some(p);
        } else {
          check(rest);
        };
      };
    check(Array.to_list(Sys.readdir(Path.show(path))));
  };

  let climbFrom = (currentPath, projectName) => {
    open Run.Syntax;

    let homePath = Path.homePath();

    let parentPath = path => {
      let path = Path.normalizeAndRemoveEmptySeg(path);
      let parent = Path.parent(path);
      /* do not climb further than root or home path */
      if (Path.compare(path, parent) != 0
          && Path.compare(path, homePath) != 0) {
        return(Path.parent(path));
      } else {
        errorf(
          "No esy project found (was looking from %a and up)",
          Path.ppPretty,
          currentPath,
        );
      };
    };

    let rec climb = path =>
      switch (projectName) {
      | None =>
        let kind = classifyPath(path);
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
      | Some(projectName) =>
        switch (checkPathByProjectName(path, projectName)) {
        | None =>
          let%bind parent = parentPath(path);
          climb(parent);
        | Some(path) => return((Kind.ProjectForced, path))
        }
      };

    let%bind (_kind, path) = climb(currentPath);
    return(path);
  };

  let resolve = (project: option(t)) => {
    open Run.Syntax;

    /* check if we can get projectPath from env */
    let project =
      switch (project) {
      | Some(_) => project
      | None =>
        let v =
          StringMap.find_opt(
            BuildSandbox.EsyIntrospectionEnv.rootPackageConfigPath,
            System.Environment.current,
          );
        switch (v) {
        | None => None
        | Some(v) => Some(ByPath(Path.v(v)))
        };
      };

    let%bind projectPath =
      switch (project) {
      | Some(ByPath(path)) => return(path)
      | Some(ByName(name)) => climbFrom(Path.currentPath(), Some(name))
      | None => climbFrom(Path.currentPath(), None)
      };

    if (Path.isAbs(projectPath)) {
      return(Path.normalize(projectPath));
    } else {
      return(Path.(normalize(currentPath() /\/ projectPath)));
    };
  };
};

[@deriving (show, to_yojson)]
type t = {
  mainprg: string,
  path: Path.t,
  esyVersion: string,
  spec: EsyInstall.SandboxSpec.t,
  prefixPath: option(Path.t),
  ocamlPkgName: string,
  ocamlVersion: string,
  cacheTarballsPath: option(Path.t),
  fetchConcurrency: option(int),
  gitUsername: option(string),
  gitPassword: option(string),
  buildConcurrency: option(int),
  opamRepository: option(EsySolve.Config.checkoutCfg),
  esyOpamOverride: option(EsySolve.Config.checkoutCfg),
  opamRepositoryLocal: option(Path.t),
  opamRepositoryRemote: option(string),
  esyOpamOverrideLocal: option(Path.t),
  esyOpamOverrideRemote: option(string),
  npmRegistry: option(string),
  solveTimeout: option(float),
  skipRepositoryUpdate: bool,
  solveCudfCommand: option(Cmd.t),
  globalPathVariable: option(string),
};

let globalStorePrefixPath = cfg => {
  switch (cfg.prefixPath) {
  | None => EsyBuildPackage.Config.storePrefixDefault
  | Some(prefixPath) => prefixPath
  };
};

let storePath = cfg => {
  let storePath =
    switch (cfg.prefixPath) {
    | None => EsyBuildPackage.Config.StorePathDefault
    | Some(path) => EsyBuildPackage.Config.StorePathOfPrefix(path)
    };

  Run.ofBosError(
    EsyBuildPackage.Config.(
      configureStorePath(
        ~ocamlPkgName=cfg.ocamlPkgName,
        ~ocamlVersion=cfg.ocamlVersion,
        storePath,
        globalStorePrefixPath(cfg),
      )
    ),
  );
};

module FindProject = {};

let commonOptionsSection = Manpage.s_common_options;

let ocamlPkgName = {
  let doc = "Specifies the name of the ocaml compiler package (not supported on opam projects yet)";
  let env = Arg.env_var("ESY__OCAML_PKG_NAME", ~doc);
  Arg.(
    value
    & opt(string, "ocaml")
    & info(["ocaml-pkg-name"], ~env, ~doc, ~docv="<OCAML COMPILER PACKAGE>")
  );
};

let ocamlVersion = {
  let doc = "Specifies the version of the ocaml compiler package (not supported on opam projects yet)";
  let env = Arg.env_var("ESY__OCAML_VERSION", ~doc);
  Arg.(
    value
    & opt(string, "n.00.0000")
    & info(["ocaml-version"], ~env, ~doc, ~docv="<OCAML VERSION>")
  );
};
let projectPath = {
  let doc = "Specifies esy project.";
  let env = Arg.env_var("ESY__PROJECT", ~doc);
  Arg.(
    value
    & opt(some(ProjectArg.conv), None)
    & info(["P", "project"], ~env, ~docs=commonOptionsSection, ~doc)
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
  let doc = "Specifies an opam repository to use. $(b,DEPRECATED): use opam-override-repository-local and opam-override-repository-remote instead";
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
  let doc = "Specifies an opam override repository to use. $(b,DEPRECATED): use opam-override-repository-local and opam-override-repository-remote instead";
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

let opamRepositoryLocalArg = {
  let doc = "Specifies a local opam repository to use.";
  let env = Arg.env_var("ESYI__OPAM_REPOSITORY_LOCAL", ~doc);
  Arg.(
    value
    & opt(some(Cli.pathConv), None)
    & info(["opam-repository-local"], ~env, ~doc, ~docs=commonOptionsSection)
  );
};
let opamRepositoryRemoteArg = {
  let doc = "Specifies a remote opam repository to use.";
  let env = Arg.env_var("ESYI__OPAM_REPOSITORY_REMOTE", ~doc);
  Arg.(
    value
    & opt(some(string), None)
    & info(
        ["opam-repository-remote"],
        ~env,
        ~doc,
        ~docs=commonOptionsSection,
      )
  );
};

let esyOpamOverrideLocalArg = {
  let doc = "Specifies a local opam override repository to use.";
  let env = Arg.env_var("ESYI__OPAM_OVERRIDE_LOCAL", ~doc);
  Arg.(
    value
    & opt(some(Cli.pathConv), None)
    & info(
        ["opam-override-repository-local"],
        ~env,
        ~doc,
        ~docs=commonOptionsSection,
      )
  );
};
let esyOpamOverrideRemoteArg = {
  let doc = "Specifies a remote opam override repository to use.";
  let env = Arg.env_var("ESYI__OPAM_OVERRIDE_REMOTE", ~doc);
  Arg.(
    value
    & opt(some(string), None)
    & info(
        ["opam-override-repository-remote"],
        ~env,
        ~doc,
        ~docs=commonOptionsSection,
      )
  );
};

let globalPathVariableArg = {
  let doc = "Specifies the PATH variable to look for global utils in the build env.";
  let env = Arg.env_var("ESY__GLOBAL_PATH", ~doc);
  Arg.(
    value
    & opt(some(string), None)
    & info(["global-path"], ~env, ~docs=commonOptionsSection, ~doc)
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

let fetchConcurrencyArg = {
  let doc = "Specifies number of concurrent fetch tasks.";
  let env = Arg.env_var("ESY__FETCH_CONCURRENCY", ~doc);
  Arg.(
    value
    & opt(some(int), None)
    & info(["fetch-concurrency"], ~env, ~doc, ~docs=commonOptionsSection)
  );
};

let gitUsername = {
  let doc = "Specifies username of the git repositories being fetched. Note: this username will be used for all repositories in the dependencies tree. This option is useful in environments where ssh isn't available.";
  let env = Arg.env_var("ESY__GIT_USERNAME", ~doc);
  Arg.(
    value
    & opt(some(string), None)
    & info(["git-username"], ~env, ~doc, ~docs=commonOptionsSection)
  );
};

let gitPassword = {
  let doc = "Specifies password of the git repositories being fetched. Note: Will be used everywhere (ref: username option). If your git repository services provides personal access token, it recommended you use them.";
  let env = Arg.env_var("ESY__GIT_PASSWORD", ~doc);
  Arg.(
    value
    & opt(some(string), None)
    & info(["git-password"], ~env, ~doc, ~docs=commonOptionsSection)
  );
};

let buildConcurrencyArg = {
  let doc = "Specifies number of concurrent build tasks";
  let env = Arg.env_var("ESY__BUILD_CONCURRENCY", ~doc);
  Arg.(
    value
    & opt(some(int), None)
    & info(["build-concurrency"], ~env, ~doc, ~docs=commonOptionsSection)
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
      project,
      ocamlPkgName,
      ocamlVersion,
      mainprg,
      prefixPath,
      cacheTarballsPath,
      fetchConcurrency,
      gitUsername,
      gitPassword,
      buildConcurrency,
      opamRepository,
      esyOpamOverride,
      opamRepositoryLocal,
      opamRepositoryRemote,
      esyOpamOverrideLocal,
      esyOpamOverrideRemote,
      npmRegistry,
      solveTimeout,
      skipRepositoryUpdate,
      solveCudfCommand,
      globalPathVariable,
    ) => {
  open RunAsync.Syntax;

  let%bind projectPath = RunAsync.ofRun(ProjectArg.resolve(project));
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
    ocamlPkgName,
    ocamlVersion,
    path: projectPath,
    esyVersion: EsyRuntime.version,
    spec,
    prefixPath,
    cacheTarballsPath,
    fetchConcurrency,
    gitUsername,
    gitPassword,
    buildConcurrency,
    opamRepository,
    esyOpamOverride,
    opamRepositoryLocal,
    opamRepositoryRemote,
    esyOpamOverrideLocal,
    esyOpamOverrideRemote,
    npmRegistry,
    solveTimeout,
    skipRepositoryUpdate,
    solveCudfCommand,
    globalPathVariable,
  });
};

let promiseTerm = {
  let parse =
      (
        mainprg,
        projectPath,
        ocamlPkgName,
        ocamlVersion,
        prefixPath,
        cacheTarballsPath,
        fetchConcurrency,
        gitUsername,
        gitPassword,
        buildConcurrency,
        opamRepository,
        esyOpamOverride,
        opamRepositoryLocal,
        opamRepositoryRemote,
        esyOpamOverrideLocal,
        esyOpamOverrideRemote,
        npmRegistry,
        solveTimeout,
        skipRepositoryUpdate,
        solveCudfCommand,
        globalPathVariable,
        (),
      ) =>
    make(
      projectPath,
      ocamlPkgName,
      ocamlVersion,
      mainprg,
      prefixPath,
      cacheTarballsPath,
      fetchConcurrency,
      gitUsername,
      gitPassword,
      buildConcurrency,
      opamRepository,
      esyOpamOverride,
      opamRepositoryLocal,
      opamRepositoryRemote,
      esyOpamOverrideLocal,
      esyOpamOverrideRemote,
      npmRegistry,
      solveTimeout,
      skipRepositoryUpdate,
      solveCudfCommand,
      globalPathVariable,
    );

  Cmdliner.Term.(
    const(parse)
    $ main_name
    $ projectPath
    $ ocamlPkgName
    $ ocamlVersion
    $ prefixPath
    $ cacheTarballsPath
    $ fetchConcurrencyArg
    $ gitUsername
    $ gitPassword
    $ buildConcurrencyArg
    $ opamRepositoryArg
    $ esyOpamOverrideArg
    $ opamRepositoryLocalArg
    $ opamRepositoryRemoteArg
    $ esyOpamOverrideLocalArg
    $ esyOpamOverrideRemoteArg
    $ npmRegistryArg
    $ solveTimeoutArg
    $ skipRepositoryUpdateArg
    $ solveCudfCommandArg
    $ globalPathVariableArg
    $ Cli.setupLogTerm
  );
};

let term =
  Cmdliner.Term.(ret(const(Cli.runAsyncToCmdlinerRet) $ promiseTerm));

let promiseTermForMultiplePaths = resolvedPathTerm => {
  let parse =
      (
        mainprg,
        paths,
        prefixPath,
        cacheTarballsPath,
        fetchConcurrency,
        gitUsername,
        gitPassword,
        buildConcurrency,
        opamRepository,
        esyOpamOverride,
        opamRepositoryLocal,
        opamRepositoryRemote,
        esyOpamOverrideLocal,
        esyOpamOverrideRemote,
        npmRegistry,
        solveTimeout,
        skipRepositoryUpdate,
        solveCudfCommand,
        globalPathVariable,
        (),
      ) => {
    open RunAsync.Syntax;
    let%bind paths =
      switch (paths) {
      | [] =>
        let prefixPath =
          switch (prefixPath) {
          | Some(path) => path
          | None => EsyBuildPackage.Config.storePrefixDefault
          };

        let projectsPath = Path.(prefixPath / "projects.json");
        let%bind json = Fs.readJsonFile(projectsPath);
        let%bind json =
          switch (Json.Decode.(list(string, json))) {
          | Ok(json) => return(json)
          | Error(err) =>
            errorf("%s cannot be parsed", projectsPath |> Path.show)
          };
        json |> List.map(~f=Path.v) |> return;
      | _ => return(paths)
      };
    paths
    |> List.map(~f=path =>
         make(
           Some(ProjectArg.ofPath(path)),
           mainprg,
           "ocaml", /* specifying ocaml package name for multiple paths is unsupported */
           "n.00.0000", /* specifying ocaml version for multiple paths is unsupported */
           prefixPath,
           cacheTarballsPath,
           fetchConcurrency,
           gitUsername,
           gitPassword,
           buildConcurrency,
           opamRepository,
           esyOpamOverride,
           opamRepositoryLocal,
           opamRepositoryRemote,
           esyOpamOverrideLocal,
           esyOpamOverrideRemote,
           npmRegistry,
           solveTimeout,
           skipRepositoryUpdate,
           solveCudfCommand,
           globalPathVariable,
         )
       )
    |> RunAsync.List.joinAll;
  };

  Cmdliner.Term.(
    const(parse)
    $ main_name
    $ Arg.(
        value
        & pos_all(resolvedPathTerm, [])
        & info(
            [],
            ~docv="ROOT",
            ~doc="Project roots for which built artifacts must be kept",
          )
      )
    $ prefixPath
    $ cacheTarballsPath
    $ fetchConcurrencyArg
    $ gitUsername
    $ gitPassword
    $ buildConcurrencyArg
    $ opamRepositoryArg
    $ esyOpamOverrideArg
    $ opamRepositoryLocalArg
    $ opamRepositoryRemoteArg
    $ esyOpamOverrideLocalArg
    $ esyOpamOverrideRemoteArg
    $ npmRegistryArg
    $ solveTimeoutArg
    $ skipRepositoryUpdateArg
    $ solveCudfCommandArg
    $ globalPathVariableArg
    $ Cli.setupLogTerm
  );
};

let multipleProjectConfigsTerm = paths =>
  Cmdliner.Term.(
    ret(
      const(Cli.runAsyncToCmdlinerRet) $ promiseTermForMultiplePaths(paths),
    )
  );