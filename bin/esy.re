open EsyBuild;
open EsyPackageConfig;

module SandboxSpec = EsyInstall.SandboxSpec;
module Installation = EsyInstall.Installation;
module Solution = EsyInstall.Solution;
module SolutionLock = EsyInstall.SolutionLock;
module Package = EsyInstall.Package;

let splitBy = (line, ch) =>
  switch (String.index(line, ch)) {
  | idx =>
    let key = String.sub(line, 0, idx);
    let pos = idx + 1;
    let val_ = String.(trim(sub(line, pos, length(line) - pos)));
    Some((key, val_));
  | exception Not_found => None
  };

let chdirTerm =
  Cmdliner.Arg.(
    value
    & flag
    & info(
        ["C", "change-directory"],
        ~doc="Change directory to package's root before executing the command",
      )
  );

let pkgTerm =
  Cmdliner.Arg.(
    value
    & opt(PkgArg.conv, PkgArg.ByDirectoryPath(Path.currentPath()))
    & info(["p", "package"], ~doc="Package to work on", ~docv="PACKAGE")
  );

let cmdAndPkgTerm = {
  let cmd =
    Cli.cmdOptionTerm(
      ~doc="Command to execute within the environment.",
      ~docv="COMMAND",
    );

  let pkg =
    Cmdliner.Arg.(
      value
      & opt(some(PkgArg.conv), None)
      & info(["p", "package"], ~doc="Package to work on", ~docv="PACKAGE")
    );

  let make = (pkg, cmd) =>
    switch (pkg, cmd) {
    | (None, None) => `Ok(None)
    | (None, Some(cmd)) => `Ok(Some((None, cmd)))
    | (Some(pkgarg), Some(cmd)) => `Ok(Some((Some(pkgarg), cmd)))
    | (Some(_), None) =>
      `Error((
        false,
        "missing a command to execute (required when '-p <name>' is passed)",
      ))
    };

  Cmdliner.Term.(ret(const(make) $ pkg $ cmd));
};

let depspecConv = {
  open Cmdliner;
  open Result.Syntax;
  let parse = v => {
    let lexbuf = Lexing.from_string(v);
    try(
      return(
        EsyInstall.DepSpecParser.start(EsyInstall.DepSpecLexer.read, lexbuf),
      )
    ) {
    | EsyInstall.DepSpecLexer.Error(msg) =>
      let msg = Printf.sprintf("error parsing DEPSPEC: %s", msg);
      error(`Msg(msg));
    | EsyInstall.DepSpecParser.Error => error(`Msg("error parsing DEPSPEC"))
    };
  };

  let pp = EsyInstall.Solution.DepSpec.pp;
  Arg.conv(~docv="DEPSPEC", (parse, pp));
};

let modeTerm = {
  let make = release =>
    if (release) {BuildSpec.Build} else {BuildSpec.BuildDev};

  Cmdliner.Term.(
    const(make)
    $ Cmdliner.Arg.(
        value & flag & info(["release"], ~doc="Build in release mode")
      )
  );
};

module Findlib = {
  type meta = {
    package: string,
    description: string,
    version: string,
    archive: string,
    location: string,
  };

  let query = (~ocamlfind, ~task, proj, lib) => {
    open RunAsync.Syntax;
    let ocamlpath =
      Path.(
        BuildSandbox.Task.installPath(proj.Project.buildCfg, task) / "lib"
      );

    let env =
      ChildProcess.CustomEnv(
        Astring.String.Map.(empty |> add("OCAMLPATH", Path.show(ocamlpath))),
      );
    let cmd =
      Cmd.(
        v(p(ocamlfind))
        % "query"
        % "-predicates"
        % "byte,native"
        % "-long-format"
        % lib
      );
    let%bind out = ChildProcess.runOut(~env, cmd);
    let lines =
      String.split_on_char('\n', out)
      |> List.map(~f=line => splitBy(line, ':'))
      |> List.filterNone
      |> List.rev;

    let findField = (~name) => {
      let f = ((field, value)) => field == name ? Some(value) : None;

      lines |> List.map(~f) |> List.filterNone |> List.hd;
    };

    return({
      package: findField(~name="package"),
      description: findField(~name="description"),
      version: findField(~name="version"),
      archive: findField(~name="archive(s)"),
      location: findField(~name="location"),
    });
  };

  let libraries = (~ocamlfind, ~builtIns=?, ~task=?, proj) => {
    open RunAsync.Syntax;
    let ocamlpath =
      switch (task) {
      | Some(task) =>
        Path.(
          BuildSandbox.Task.installPath(proj.Project.buildCfg, task)
          / "lib"
          |> show
        )
      | None => ""
      };

    let env =
      ChildProcess.CustomEnv(
        Astring.String.Map.(empty |> add("OCAMLPATH", ocamlpath)),
      );
    let cmd = Cmd.(v(p(ocamlfind)) % "list");
    let%bind out = ChildProcess.runOut(~env, cmd);
    let libs =
      String.split_on_char('\n', out)
      |> List.map(~f=line => splitBy(line, ' '))
      |> List.filterNone
      |> List.map(~f=((key, _)) => key)
      |> List.rev;

    switch (builtIns) {
    | Some(discard) => return(List.diff(libs, discard))
    | None => return(libs)
    };
  };

  let modules = (~ocamlobjinfo, archive) => {
    open RunAsync.Syntax;
    let env = ChildProcess.CustomEnv(Astring.String.Map.empty);
    let cmd = Cmd.(v(p(ocamlobjinfo)) % archive);
    let%bind out = ChildProcess.runOut(~env, cmd);
    let startsWith = (s1, s2) => {
      let len1 = String.length(s1);
      let len2 = String.length(s2);
      len1 < len2 ? false : String.sub(s1, 0, len2) == s2;
    };

    let lines = {
      let f = line =>
        startsWith(line, "Name: ") || startsWith(line, "Unit name: ");

      String.split_on_char('\n', out)
      |> List.filter(~f)
      |> List.map(~f=line => splitBy(line, ':'))
      |> List.filterNone
      |> List.map(~f=((_, val_)) => val_)
      |> List.rev;
    };

    return(lines);
  };
};

let resolvedPathTerm = {
  open Cmdliner;
  let parse = v =>
    switch (Path.ofString(v)) {
    | Ok(path) =>
      if (Path.isAbs(path)) {
        Ok(path);
      } else {
        Ok(Path.(EsyRuntime.currentWorkingDir /\/ path |> normalize));
      }
    | err => err
    };

  let print = Path.pp;
  Arg.conv(~docv="PATH", (parse, print));
};

let buildDependencies = (all, mode, pkgarg, proj: Project.t) => {
  open RunAsync.Syntax;
  let f = (pkg: Package.t) => {
    let%bind plan = Project.plan(mode, proj);
    Project.buildDependencies(~buildLinked=all, proj, plan, pkg);
  };

  Project.withPackage(proj, pkgarg, f);
};

let cleanup = (projCfgs: list(ProjectConfig.t), dryRun) => {
  open RunAsync.Syntax;
  let mode = BuildSpec.BuildDev;
  let%bind (dirsToKeep, allDirs) =
    RunAsync.List.foldLeft(
      ~init=(Path.Set.empty, Path.Set.empty),
      ~f=
        (acc, projCfg) => {
          let (dirsToKeep, allDirs) = acc;
          let%bind (proj, _) = Project.make(projCfg);
          let%bind plan = Project.plan(mode, proj);
          let%bind allProjectDependencies =
            BuildSandbox.Plan.all(plan)
            |> List.map(~f=task =>
                 Scope.installPath(task.BuildSandbox.Task.scope)
                 |> Project.renderSandboxPath(proj.Project.buildCfg)
               )
            |> Path.Set.of_list
            |> RunAsync.return;
          let%bind storePath =
            RunAsync.ofRun(ProjectConfig.storePath(projCfg));
          let%bind allDirs' =
            Fs.listDir(Path.(storePath / Store.installTree));
          let shortBuildPath =
            Path.(
              ProjectConfig.globalStorePrefixPath(projCfg)
              / Store.version
              / Store.buildTree
            );
          RunAsync.return((
            Path.Set.union(dirsToKeep, allProjectDependencies),
            Path.Set.union(
              allDirs,
              Path.Set.of_list([
                Path.(storePath / Store.buildTree),
                Path.(storePath / Store.stageTree),
                shortBuildPath,
                ...allDirs'
                   |> List.map(~f=x =>
                        Path.(storePath / Store.installTree / x)
                      ),
              ]),
            ),
          ));
        },
      projCfgs,
    );

  let buildsToBePurged = Path.Set.diff(allDirs, dirsToKeep);

  if (dryRun) {
    print_endline("Will be purging the following");
    Path.Set.iter(p => p |> Path.show |> print_endline, buildsToBePurged);
    RunAsync.return();
  } else {
    let queue = LwtTaskQueue.create(~concurrency=40, ());
    Path.Set.elements(buildsToBePurged)
    |> List.map(~f=p => LwtTaskQueue.submit(queue, () => Fs.rmPath(p)))
    |> RunAsync.List.waitAll;
  };
};

let execCommand =
    (
      buildIsInProgress,
      includeBuildEnv,
      includeCurrentEnv,
      includeEsyIntrospectionEnv,
      includeNpmBin,
      plan,
      envspec,
      chdir,
      pkgarg,
      cmd,
      proj: Project.t,
    ) => {
  let envspec = {
    EnvSpec.buildIsInProgress,
    includeBuildEnv,
    includeCurrentEnv,
    includeNpmBin,
    includeEsyIntrospectionEnv,
    augmentDeps: envspec,
  };
  let f = pkg =>
    Project.execCommand(
      ~checkIfDependenciesAreBuilt=false,
      ~buildLinked=false,
      ~changeDirectoryToPackageRoot=chdir,
      proj,
      envspec,
      plan,
      pkg,
      cmd,
    );

  Project.withPackage(proj, pkgarg, f);
};

let printEnv =
    (
      asJson,
      includeBuildEnv,
      includeCurrentEnv,
      includeEsyIntrospectionEnv,
      includeNpmBin,
      plan,
      envspec,
      pkgarg,
      proj: Project.t,
    ) => {
  let envspec = {
    EnvSpec.buildIsInProgress: false,
    includeBuildEnv,
    includeCurrentEnv,
    includeEsyIntrospectionEnv,
    includeNpmBin,
    augmentDeps: envspec,
  };
  Project.printEnv(proj, envspec, plan, asJson, pkgarg, ());
};

module Status = {
  [@deriving to_yojson]
  type t = {
    isProject: bool,
    isProjectSolved: bool,
    isProjectFetched: bool,
    isProjectReadyForDev: bool,
    rootBuildPath: option(Path.t),
    rootInstallPath: option(Path.t),
    rootPackageConfigPath: option(Path.t),
  };

  let notAProject = {
    isProject: false,
    isProjectSolved: false,
    isProjectFetched: false,
    isProjectReadyForDev: false,
    rootBuildPath: None,
    rootInstallPath: None,
    rootPackageConfigPath: None,
  };
};

let status = (maybeProject: RunAsync.t(Project.t), _asJson, ()) => {
  open RunAsync.Syntax;
  open Status;

  let protectRunAsync = v =>
    try%lwt(v) {
    | _ => RunAsync.error("fatal error which is ignored by status command")
    };

  let%bind status =
    switch%lwt (protectRunAsync(maybeProject)) {
    | Error(_) => return(notAProject)
    | Ok(proj) =>
      let%lwt isProjectSolved = {
        let%lwt solved = Project.solved(proj);
        Lwt.return(Result.isOk(solved));
      };

      let%lwt isProjectFetched = {
        let%lwt fetched = Project.fetched(proj);
        Lwt.return(Result.isOk(fetched));
      };

      let%lwt built =
        protectRunAsync(
          {
            let%bind fetched = Project.fetched(proj);
            let%bind configured = Project.configured(proj);
            let checkTask = (built, task) =>
              if (built) {
                switch (Scope.sourceType(task.BuildSandbox.Task.scope)) {
                | Immutable
                | ImmutableWithTransientDependencies =>
                  BuildSandbox.isBuilt(fetched.Project.sandbox, task)
                | Transient => return(built)
                };
              } else {
                return(built);
              };

            RunAsync.List.foldLeft(
              ~f=checkTask,
              ~init=true,
              BuildSandbox.Plan.all(configured.Project.planForDev),
            );
          },
        );
      let%lwt rootBuildPath = {
        open RunAsync.Syntax;
        let%bind configured = Project.configured(proj);
        let root = configured.Project.root;
        return(
          Some(BuildSandbox.Task.buildPath(proj.Project.buildCfg, root)),
        );
      };

      let%lwt rootInstallPath = {
        open RunAsync.Syntax;
        let%bind configured = Project.configured(proj);
        let root = configured.Project.root;
        return(
          Some(BuildSandbox.Task.installPath(proj.Project.buildCfg, root)),
        );
      };

      let rootPackageConfigPath =
        EsyInstall.SandboxSpec.manifestPath(proj.projcfg.spec);

      return({
        isProject: true,
        isProjectSolved,
        isProjectFetched,
        isProjectReadyForDev: Result.getOr(false, built),
        rootBuildPath: Result.getOr(None, rootBuildPath),
        rootInstallPath: Result.getOr(None, rootInstallPath),
        rootPackageConfigPath,
      });
    };

  Format.fprintf(
    Format.std_formatter,
    "%a@.",
    Json.Print.ppRegular,
    Status.to_yojson(status),
  );
  return();
};

let buildPlan = (mode, pkgarg, proj: Project.t) => {
  open RunAsync.Syntax;

  let%bind plan = Project.plan(mode, proj);

  let f = (pkg: Package.t) =>
    switch (BuildSandbox.Plan.get(plan, pkg.id)) {
    | Some(task) =>
      let json = BuildSandbox.Task.to_yojson(task);
      let data = Yojson.Safe.pretty_to_string(json);
      print_endline(data);
      return();
    | None => errorf("not build defined for %a", PkgArg.pp, pkgarg)
    };

  Project.withPackage(proj, pkgarg, f);
};

let buildShell = (mode, pkgarg, proj: Project.t) => {
  open RunAsync.Syntax;

  let%bind fetched = Project.fetched(proj);
  let%bind configured = Project.configured(proj);

  let f = (pkg: Package.t) => {
    let%bind () =
      Project.buildDependencies(
        ~buildLinked=true,
        proj,
        configured.Project.planForDev,
        pkg,
      );

    let p =
      BuildSandbox.buildShell(
        proj.Project.workflow.buildspec,
        mode,
        fetched.Project.sandbox,
        pkg.id,
      );

    switch%bind (p) {
    | Unix.WEXITED(0) => return()
    | Unix.WEXITED(n)
    | Unix.WSTOPPED(n)
    | Unix.WSIGNALED(n) => exit(n)
    };
  };

  Project.withPackage(proj, pkgarg, f);
};

let build =
    (
      ~buildOnly=true,
      ~skipStalenessCheck=false,
      mode,
      pkgarg,
      disableSandbox,
      cmd,
      proj: Project.t,
    ) => {
  open RunAsync.Syntax;

  let%bind fetched = Project.fetched(proj);
  let%bind plan = Project.plan(mode, proj);

  let f = pkg =>
    switch (cmd) {
    | None =>
      let%bind () =
        Project.buildDependencies(
          ~buildLinked=true,
          ~skipStalenessCheck,
          proj,
          plan,
          pkg,
        );

      Project.buildPackage(
        ~quiet=true,
        ~buildOnly,
        ~disableSandbox,
        proj.projcfg,
        fetched.Project.sandbox,
        plan,
        pkg,
      );
    | Some(cmd) =>
      let%bind () =
        Project.buildDependencies(
          ~buildLinked=true,
          ~skipStalenessCheck,
          proj,
          plan,
          pkg,
        );

      Project.execCommand(
        ~checkIfDependenciesAreBuilt=false,
        ~buildLinked=false,
        proj,
        proj.workflow.buildenvspec,
        mode,
        pkg,
        cmd,
      );
    };

  Project.withPackage(proj, pkgarg, f);
};

let buildEnv = (asJson, mode, pkgarg, proj: Project.t) =>
  Project.printEnv(
    ~name="Build environment",
    proj,
    proj.workflow.buildenvspec,
    mode,
    asJson,
    pkgarg,
    (),
  );

let commandEnv = (asJson, pkgarg, proj: Project.t) =>
  Project.printEnv(
    ~name="Command environment",
    proj,
    proj.workflow.commandenvspec,
    BuildDev,
    asJson,
    pkgarg,
    (),
  );

let execEnv = (asJson, pkgarg, proj: Project.t) =>
  Project.printEnv(
    ~name="Exec environment",
    proj,
    proj.workflow.execenvspec,
    BuildDev,
    asJson,
    pkgarg,
    (),
  );

let exec = (mode, chdir, pkgarg, disableSandbox, cmd, proj: Project.t) => {
  open RunAsync.Syntax;
  let%bind () =
    build(~buildOnly=false, mode, PkgArg.root, disableSandbox, None, proj);
  let f = pkg =>
    Project.execCommand(
      ~checkIfDependenciesAreBuilt=false, /* not needed as we build an entire sandbox above */
      ~buildLinked=false,
      ~changeDirectoryToPackageRoot=chdir,
      proj,
      proj.workflow.execenvspec,
      mode,
      pkg,
      cmd,
    );

  Project.withPackage(proj, pkgarg, f);
};

let runScript = (script, args, proj: Project.t) => {
  open RunAsync.Syntax;

  let%bind fetched = Project.fetched(proj);
  let%bind configured = Project.configured(proj);

  let (scriptArgs, envspec) = {
    let peekArgs =
      fun
      | ["esy", "x", ...args] => (["x", ...args], proj.workflow.execenvspec)
      | ["esy", "b", ...args]
      | ["esy", "build", ...args] => (
          ["build", ...args],
          proj.workflow.buildenvspec,
        )
      | ["esy", ...args] => (args, proj.workflow.commandenvspec)
      | args => (args, proj.workflow.commandenvspec);

    switch (script.Scripts.command) {
    | Parsed(args) =>
      let (args, spec) = peekArgs(args);
      (Command.Parsed(args), spec);
    | Unparsed(line) =>
      let (args, spec) = peekArgs(Astring.String.cuts(~sep=" ", line));
      (Command.Unparsed(String.concat(" ", args)), spec);
    };
  };

  let%bind (cmd, cwd) =
    RunAsync.ofRun(
      {
        open Run.Syntax;

        let id = configured.Project.root.pkg.id;
        let%bind (env, scope) =
          BuildSandbox.configure(
            envspec,
            proj.workflow.buildspec,
            BuildDev,
            fetched.Project.sandbox,
            id,
          );

        let%bind env =
          Run.ofStringError(Scope.SandboxEnvironment.Bindings.eval(env));

        let expand = v => {
          let%bind v =
            Scope.render(
              ~env,
              ~buildIsInProgress=envspec.buildIsInProgress,
              scope,
              v,
            );
          return(Scope.SandboxValue.render(proj.buildCfg, v));
        };

        let%bind scriptArgs =
          switch (scriptArgs) {
          | Parsed(args) => Result.List.map(~f=expand, args)
          | Unparsed(line) =>
            let%bind line = expand(line);
            ShellSplit.split(line);
          };

        let%bind args = Result.List.map(~f=expand, args);

        let cmd =
          Cmd.(
            v(p(EsyRuntime.currentExecutable))
            |> addArgs(scriptArgs)
            |> addArgs(args)
          );

        let cwd =
          Some(
            Scope.(
              rootPath(scope)
              |> SandboxPath.toValue
              |> SandboxValue.render(proj.buildCfg)
            ),
          );

        return((cmd, cwd));
      },
    );

  let%bind status =
    ChildProcess.runToStatus(
      ~resolveProgramInEnv=true,
      ~cwd?,
      ~stderr=`FD_copy(Unix.stderr),
      ~stdout=`FD_copy(Unix.stdout),
      ~stdin=`FD_copy(Unix.stdin),
      cmd,
    );

  switch (status) {
  | Unix.WEXITED(n)
  | Unix.WSTOPPED(n)
  | Unix.WSIGNALED(n) => exit(n)
  };
};

let runScriptCommand = (cmd, proj: Project.t) => {
  open RunAsync.Syntax;
  let%bind _ = Project.fetched(proj);
  let script = Cmd.getTool(cmd);
  switch (Scripts.find(script, proj.scripts)) {
  | Some(script) => runScript(script, Cmd.getArgs(cmd), proj)
  | None => errorf("Script '%s' not found", script)
  };
};

let devExec = (chdir: bool, pkgarg: PkgArg.t, proj: Project.t, cmd, ()) => {
  let f = (pkg: Package.t) =>
    Project.execCommand(
      ~checkIfDependenciesAreBuilt=true,
      ~buildLinked=false,
      ~changeDirectoryToPackageRoot=chdir,
      proj,
      proj.workflow.commandenvspec,
      BuildDev,
      pkg,
      cmd,
    );

  Project.withPackage(proj, pkgarg, f);
};

let devShell = (pkgarg, proj: Project.t) => {
  let shell =
    try(Sys.getenv("SHELL")) {
    | Not_found => "/bin/bash"
    };

  let f = (pkg: Package.t) =>
    Project.execCommand(
      ~checkIfDependenciesAreBuilt=true,
      ~buildLinked=false,
      proj,
      proj.workflow.commandenvspec,
      BuildDev,
      pkg,
      Cmd.v(shell),
    );

  Project.withPackage(proj, pkgarg, f);
};

let makeLsCommand =
    (~computeTermNode, ~includeTransitive, mode, pkgarg, proj: Project.t) => {
  open RunAsync.Syntax;

  let%bind solved = Project.solved(proj);
  let%bind plan = Project.plan(mode, proj);

  let seen = ref(PackageId.Set.empty);

  let rec draw = (root, pkg) => {
    let id = pkg.Package.id;
    if (PackageId.Set.mem(id, seen^)) {
      return(None);
    } else {
      let isRoot = Package.compare(root, pkg) == 0;
      seen := PackageId.Set.add(id, seen^);
      switch (BuildSandbox.Plan.get(plan, id)) {
      | None => return(None)
      | Some(task) =>
        let%bind children =
          if (!includeTransitive && !isRoot) {
            return([]);
          } else {
            let dependencies = {
              let spec = BuildSandbox.Plan.spec(plan);
              Solution.dependenciesBySpec(solved.Project.solution, spec, pkg);
            };

            dependencies |> List.map(~f=draw(root)) |> RunAsync.List.joinAll;
          };

        let children = children |> List.filterNone;
        computeTermNode(task, children);
      };
    };
  };

  let f = pkg =>
    switch%bind (draw(pkg, pkg)) {
    | Some(tree) => return(print_endline(TermTree.render(tree)))
    | None => return()
    };

  Project.withPackage(proj, pkgarg, f);
};

let formatPackageInfo = (~built: bool, task: BuildSandbox.Task.t) => {
  open RunAsync.Syntax;
  let version = Chalk.grey("@" ++ Version.show(Scope.version(task.scope)));
  let status =
    switch (Scope.sourceType(task.scope), built) {
    | (SourceType.Immutable, true) => Chalk.green("[built]")
    | (_, _) => Chalk.blue("[build pending]")
    };

  let line =
    Printf.sprintf("%s%s %s", Scope.name(task.scope), version, status);
  return(line);
};

let lsBuilds = (includeTransitive, mode, pkgarg, proj: Project.t) => {
  open RunAsync.Syntax;
  let%bind fetched = Project.fetched(proj);
  let computeTermNode = (task, children) => {
    let%bind built = BuildSandbox.isBuilt(fetched.Project.sandbox, task);
    let%bind line = formatPackageInfo(~built, task);
    return(Some(TermTree.Node({line, children})));
  };

  makeLsCommand(~computeTermNode, ~includeTransitive, mode, pkgarg, proj);
};

let lsLibs = (includeTransitive, mode, pkgarg, proj: Project.t) => {
  open RunAsync.Syntax;
  let%bind fetched = Project.fetched(proj);

  let%bind ocamlfind = {
    let%bind p = Project.ocamlfind(proj);
    return(Path.(p / "bin" / "ocamlfind"));
  };

  let%bind builtIns = Findlib.libraries(~ocamlfind, proj);

  let computeTermNode = (task: BuildSandbox.Task.t, children) => {
    let%bind built = BuildSandbox.isBuilt(fetched.Project.sandbox, task);
    let%bind line = formatPackageInfo(~built, task);

    let%bind libs =
      if (built) {
        Findlib.libraries(~ocamlfind, ~builtIns, ~task, proj);
      } else {
        return([]);
      };

    let libs =
      libs
      |> List.map(~f=lib => {
           let line = Chalk.yellow(lib);
           TermTree.Node({line, children: []});
         });

    return(Some(TermTree.Node({line, children: libs @ children})));
  };

  makeLsCommand(~computeTermNode, ~includeTransitive, mode, pkgarg, proj);
};

let lsModules = (only, mode, pkgarg, proj: Project.t) => {
  open RunAsync.Syntax;

  let%bind fetched = Project.fetched(proj);
  let%bind configured = Project.configured(proj);

  let%bind ocamlfind = {
    let%bind p = Project.ocamlfind(proj);
    return(Path.(p / "bin" / "ocamlfind"));
  };

  let%bind ocamlobjinfo = {
    let%bind p = Project.ocaml(proj);
    return(Path.(p / "bin" / "ocamlobjinfo"));
  };

  let%bind builtIns = Findlib.libraries(~ocamlfind, proj);

  let formatLibraryModules = (~task, lib) => {
    let%bind meta = Findlib.query(~ocamlfind, ~task, proj, lib);
    Findlib.(
      if (String.length(meta.archive) === 0) {
        let description = Chalk.dim(meta.description);
        return([TermTree.Node({line: description, children: []})]);
      } else {
        Path.ofString(meta.location ++ Path.dirSep ++ meta.archive)
        |> (
          fun
          | Ok(archive) =>
            if%bind (Fs.exists(archive)) {
              let archive = Path.show(archive);
              let%bind lines = Findlib.modules(~ocamlobjinfo, archive);

              let modules = {
                let isPublicModule = name =>
                  !Astring.String.is_infix(~affix="__", name);

                let toTermNode = name => {
                  let line = Chalk.cyan(name);
                  TermTree.Node({line, children: []});
                };

                lines
                |> List.filter(~f=isPublicModule)
                |> List.map(~f=toTermNode);
              };

              return(modules);
            } else {
              return([]);
            }
          | Error(`Msg(msg)) => error(msg)
        );
      }
    );
  };

  let computeTermNode = (task: BuildSandbox.Task.t, children) => {
    let%bind built = BuildSandbox.isBuilt(fetched.Project.sandbox, task);
    let%bind line = formatPackageInfo(~built, task);

    let%bind libs =
      if (built) {
        Findlib.libraries(~ocamlfind, ~builtIns, ~task, proj);
      } else {
        return([]);
      };

    let isNotRoot =
      PackageId.compare(task.pkg.id, configured.Project.root.pkg.id) != 0;
    let constraintsSet = List.length(only) != 0;
    let noMatchedLibs = List.length(List.intersect(only, libs)) == 0;

    if (isNotRoot && constraintsSet && noMatchedLibs) {
      return(None);
    } else {
      let%bind libs =
        libs
        |> List.filter(~f=lib =>
             if (List.length(only) == 0) {
               true;
             } else {
               List.mem(lib, ~set=only);
             }
           )
        |> List.map(~f=lib => {
             let line = Chalk.yellow(lib);
             let%bind children = formatLibraryModules(~task, lib);

             return(TermTree.Node({line, children}));
           })
        |> RunAsync.List.joinAll;

      return(Some(TermTree.Node({line, children: libs @ children})));
    };
  };

  makeLsCommand(
    ~computeTermNode,
    ~includeTransitive=false,
    mode,
    pkgarg,
    proj,
  );
};

let getSandboxSolution =
    (~dumpCudfInput=None, ~dumpCudfOutput=None, solvespec, proj: Project.t) => {
  open EsySolve;
  open RunAsync.Syntax;
  let%bind solution =
    Solver.solve(
      ~gitUsername=proj.projcfg.gitUsername,
      ~gitPassword=proj.projcfg.gitPassword,
      ~dumpCudfInput,
      ~dumpCudfOutput,
      solvespec,
      proj.solveSandbox,
    );
  let lockPath = SandboxSpec.solutionLockPath(proj.solveSandbox.Sandbox.spec);
  let%bind () = {
    let%bind digest = Sandbox.digest(solvespec, proj.solveSandbox);

    EsyInstall.SolutionLock.toPath(
      ~digest,
      proj.installSandbox,
      solution,
      lockPath,
      proj.projcfg.gitUsername,
      proj.projcfg.gitPassword,
    );
  };

  let unused = Resolver.getUnusedResolutions(proj.solveSandbox.resolver);
  let%lwt () = {
    let log = resolution =>
      Logs_lwt.warn(m =>
        m(
          "resolution %a is unused (defined in %a)",
          Fmt.(quote(string)),
          resolution,
          EsyInstall.SandboxSpec.pp,
          proj.installSandbox.spec,
        )
      );

    Lwt_list.iter_s(log, unused);
  };

  return(solution);
};

let solve = (force, dumpCudfInput, dumpCudfOutput, proj: Project.t) => {
  open RunAsync.Syntax;
  let run = () => {
    let%bind _: Solution.t =
      getSandboxSolution(
        ~dumpCudfInput,
        ~dumpCudfOutput,
        proj.workflow.solvespec,
        proj,
      );
    return();
  };

  if (force) {
    run();
  } else {
    let%bind digest =
      EsySolve.Sandbox.digest(proj.workflow.solvespec, proj.solveSandbox);
    let path = SandboxSpec.solutionLockPath(proj.solveSandbox.spec);
    switch%bind (
      EsyInstall.SolutionLock.ofPath(~digest, proj.installSandbox, path)
    ) {
    | Some(_) => return()
    | None => run()
    };
  };
};

let fetch = (proj: Project.t) => {
  open RunAsync.Syntax;
  let lockPath = SandboxSpec.solutionLockPath(proj.projcfg.spec);
  switch%bind (SolutionLock.ofPath(proj.installSandbox, lockPath)) {
  | Some(solution) =>
    EsyInstall.Fetch.fetch(
      proj.workflow.installspec,
      proj.installSandbox,
      solution,
      proj.projcfg.gitUsername,
      proj.projcfg.gitPassword,
    )
  | None => error("no lock found, run 'esy solve' first")
  };
};

let addProjectToGCRoot = (proj: Project.t) => {
  let projectsPath = switch(proj.projcfg.prefixPath) {
  | Some(prefixPath) => Path.(prefixPath / "projects.json")
  | None => Path.(EsyBuildPackage.Config.storePrefixDefault / "projects.json")
  };
  let currentProj = Path.show(proj.projcfg.path);

  open RunAsync.Syntax;
  let%bind json = Fs.readJsonFile(projectsPath);
  open Json;
  let%bind projects =
    switch (Decode.(list(string, json))) {
    | Ok(projects) =>
      let projects =
        if (List.mem(currentProj, projects)) {
          projects;
        } else {
          [currentProj, ...projects];
        };
      Encode.(list(string, projects)) |> return;
    | Error(err) =>
      errorf("%s cannot be parsed", projectsPath |> Path.show)
    };

  Fs.writeJsonFile(projects, projectsPath);
};

let solveAndFetch = (proj: Project.t) => {
  open RunAsync.Syntax;
  let lockPath = SandboxSpec.solutionLockPath(proj.projcfg.spec);
  let%bind digest =
    EsySolve.Sandbox.digest(proj.workflow.solvespec, proj.solveSandbox);
  let%bind () = switch%bind (SolutionLock.ofPath(~digest, proj.installSandbox, lockPath)) {
  | Some(solution) =>
    switch%bind (
      EsyInstall.Fetch.maybeInstallationOfSolution(
        proj.workflow.installspec,
        proj.installSandbox,
        solution,
      )
    ) {
    | Some(_installation) => return()
    | None => fetch(proj)
    }
  | None =>
    let%bind () = solve(false, None, None, proj);
    let%bind () = fetch(proj);
    return();
  };
  addProjectToGCRoot(proj);
};

let add = (reqs: list(string), devDependency: bool, proj: Project.t) => {
  open EsySolve;
  open RunAsync.Syntax;
  let opamError = "add dependencies manually when working with opam sandboxes";

  let%bind reqs =
    RunAsync.ofStringError(Result.List.map(~f=Req.parse, reqs));

  let solveSandbox = proj.solveSandbox;

  let%bind solveSandbox = {
    let addReqs = origDeps =>
      InstallManifest.Dependencies.(
        switch (origDeps) {
        | NpmFormula(prevReqs) => return(NpmFormula(reqs @ prevReqs))
        | OpamFormula(_) => error(opamError)
        }
      );

    let%bind combinedDeps =
      devDependency
        ? addReqs(solveSandbox.root.devDependencies)
        : addReqs(solveSandbox.root.dependencies);

    let root =
      devDependency
        ? {...solveSandbox.root, devDependencies: combinedDeps}
        : {...solveSandbox.root, dependencies: combinedDeps};

    return({...solveSandbox, root});
  };

  let proj = {...proj, solveSandbox};

  let%bind solution = getSandboxSolution(proj.workflow.solvespec, proj);
  let%bind () = fetch(proj);

  let%bind (addedDependencies, configPath) = {
    let records = {
      let f = (record: EsyInstall.Package.t, _, map) =>
        StringMap.add(record.name, record, map);

      Solution.fold(~f, ~init=StringMap.empty, solution);
    };

    let addedDependencies = {
      let f = ({Req.name, _}) =>
        switch (StringMap.find(name, records)) {
        | Some(record) =>
          let constr =
            switch (record.EsyInstall.Package.version) {
            | Version.Npm(version) =>
              SemverVersion.Formula.DNF.show(
                SemverVersion.caretRangeOfVersion(version),
              )
            | Version.Opam(version) => OpamPackage.Version.to_string(version)
            | Version.Source(_) =>
              Version.show(record.EsyInstall.Package.version)
            };

          (name, `String(constr));
        | None => assert(false)
        };

      List.map(~f, reqs);
    };

    let%bind path = {
      let spec = proj.solveSandbox.Sandbox.spec;
      switch (spec.manifest) {
      | [@implicit_arity] EsyInstall.SandboxSpec.Manifest(Esy, fname) =>
        return(Path.(spec.SandboxSpec.path / fname))
      | [@implicit_arity] Manifest(Opam, _) => error(opamError)
      | ManifestAggregate(_) => error(opamError)
      };
    };

    return((addedDependencies, path));
  };

  let%bind json = {
    let keyToUpdate = devDependency ? "devDependencies" : "dependencies";

    let%bind json = Fs.readJsonFile(configPath);
    let%bind json =
      RunAsync.ofStringError(
        {
          open Result.Syntax;
          let%bind items = Json.Decode.assoc(json);
          let%bind items = {
            let mergeWithExisting = ((key, json)) =>
              if (key == keyToUpdate) {
                let%bind dependencies = Json.Decode.assoc(json);
                let dependencies =
                  Json.mergeAssoc(dependencies, addedDependencies);
                return((key, `Assoc(dependencies)));
              } else {
                return((key, json));
              };

            let hasDependencies =
              items |> List.exists(~f=((key, _json)) => key == keyToUpdate);

            if (hasDependencies) {
              Result.List.map(~f=mergeWithExisting, items);
            } else {
              Ok(
                List.append(
                  items,
                  [(keyToUpdate, `Assoc(addedDependencies))],
                ),
              );
            };
          };

          let json = `Assoc(items);
          return(json);
        },
      );
    return(json);
  };

  let%bind () = Fs.writeJsonFile(~json, configPath);

  let%bind () = {
    let%bind solveSandbox =
      EsySolve.Sandbox.make(
        ~gitUsername=proj.projcfg.gitUsername,
        ~gitPassword=proj.projcfg.gitPassword,
        ~cfg=solveSandbox.cfg,
        solveSandbox.spec,
      );

    let proj = {...proj, solveSandbox};
    let%bind digest =
      EsySolve.Sandbox.digest(proj.workflow.solvespec, proj.solveSandbox);

    /* we can only do this because we keep invariant that the constraint we
     * save in manifest covers the installed version */
    EsyInstall.SolutionLock.unsafeUpdateChecksum(
      ~digest,
      SandboxSpec.solutionLockPath(solveSandbox.spec),
    );
  };

  return();
};

let exportBuild = (buildPath, proj: Project.t) => {
  let outputPrefixPath = Path.(EsyRuntime.currentWorkingDir / "_export");
  BuildSandbox.exportBuild(~outputPrefixPath, proj.buildCfg, buildPath);
};

let exportDependencies = (mode: EsyBuild.BuildSpec.mode, proj: Project.t) => {
  open RunAsync.Syntax;

  let%bind configured = Project.configured(proj);
  let%bind plan = Project.plan(mode, proj);

  let%bind allProjectDependencies =
    BuildSandbox.Plan.all(plan)
    |> List.map(~f=task => task.BuildSandbox.Task.pkg)
    |> List.filter(~f=pkg =>
         switch (pkg.Package.source) {
         | Link(_) => false
         | Install(_) => true
         }
       )
    |> RunAsync.return;

  let exportBuild = pkg =>
    switch (
      BuildSandbox.Plan.get(configured.Project.planForDev, pkg.Package.id)
    ) {
    | None => return()
    | Some(task) =>
      let%lwt () =
        Logs_lwt.app(m =>
          m("Exporting %s@%a", pkg.name, Version.pp, pkg.version)
        );
      let buildPath = BuildSandbox.Task.installPath(proj.buildCfg, task);
      if%bind (Fs.exists(buildPath)) {
        let outputPrefixPath = Path.(EsyRuntime.currentWorkingDir / "_export");
        BuildSandbox.exportBuild(~outputPrefixPath, proj.buildCfg, buildPath);
      } else {
        errorf(
          "%s@%a was not built, run 'esy build' first",
          pkg.name,
          Version.pp,
          pkg.version,
        );
      };
    };

  RunAsync.List.mapAndWait(
    ~concurrency=8,
    ~f=exportBuild,
    allProjectDependencies,
  );
};

let importBuild = (fromPath, buildPaths, projcfg: ProjectConfig.t) => {
  open RunAsync.Syntax;
  let%bind buildPaths =
    switch (fromPath) {
    | Some(fromPath) =>
      let%bind lines = Fs.readFile(fromPath);
      return(
        buildPaths
        @ (
          lines
          |> String.split_on_char('\n')
          |> List.filter(~f=line => String.trim(line) != "")
          |> List.map(~f=line => Path.v(line))
        ),
      );
    | None => return(buildPaths)
    };

  let%bind storePath = RunAsync.ofRun(ProjectConfig.storePath(projcfg));

  RunAsync.List.mapAndWait(
    ~concurrency=8,
    ~f=path => BuildSandbox.importBuild(storePath, path),
    buildPaths,
  );
};

let importDependencies =
    (fromPath, mode: EsyBuild.BuildSpec.mode, proj: Project.t) => {
  open RunAsync.Syntax;

  let%bind fetched = Project.fetched(proj);
  let%bind configured = Project.configured(proj);

  let%bind plan = Project.plan(mode, proj);
  let%bind allProjectDependencies =
    BuildSandbox.Plan.all(plan)
    |> List.map(~f=task => task.BuildSandbox.Task.pkg)
    |> List.filter(~f=pkg =>
         switch (pkg.Package.source) {
         | Link(_) => false
         | Install(_) => true
         }
       )
    |> RunAsync.return;

  let fromPath =
    switch (fromPath) {
    | Some(fromPath) => fromPath
    | None => Path.(proj.buildCfg.projectPath / "_export")
    };

  let importBuild = pkg =>
    switch (
      BuildSandbox.Plan.get(configured.Project.planForDev, pkg.Package.id)
    ) {
    | Some(task) =>
      if%bind (BuildSandbox.isBuilt(fetched.Project.sandbox, task)) {
        return();
      } else {
        let id = Scope.id(task.scope);
        let pathDir = Path.(fromPath / BuildId.show(id));
        let pathTgz = Path.(fromPath / (BuildId.show(id) ++ ".tar.gz"));
        if%bind (Fs.exists(pathDir)) {
          BuildSandbox.importBuild(proj.buildCfg.storePath, pathDir);
        } else {
          if%bind (Fs.exists(pathTgz)) {
            BuildSandbox.importBuild(proj.buildCfg.storePath, pathTgz);
          } else {
            let%lwt () =
              Logs_lwt.warn(m =>
                m("no prebuilt artifact found for %a", BuildId.pp, id)
              );
            return();
          };
        };
      }
    | None => return()
    };

  RunAsync.List.mapAndWait(
    ~concurrency=16,
    ~f=importBuild,
    allProjectDependencies,
  );
};

let show = (_asJson, req, proj: Project.t) => {
  open EsySolve;
  open RunAsync.Syntax;
  let%bind req = RunAsync.ofStringError(Req.parse(req));
  let%bind resolver =
    Resolver.make(~cfg=proj.solveSandbox.cfg, ~sandbox=proj.spec, ());
  let%bind resolutions =
    RunAsync.contextf(
      Resolver.resolve(
        ~gitUsername=proj.projcfg.gitUsername,
        ~gitPassword=proj.projcfg.gitPassword,
        ~name=req.name,
        ~spec=req.spec,
        resolver,
      ),
      "resolving %a",
      Req.pp,
      req,
    );

  switch (req.Req.spec) {
  | VersionSpec.Npm([[SemverVersion.Constraint.ANY]])
  | VersionSpec.Opam([[OpamPackageVersion.Constraint.ANY]]) =>
    let f = (res: Resolution.t) =>
      switch (res.resolution) {
      | Version(v) => `String(Version.showSimple(v))
      | _ => failwith("unreachable")
      };

    `Assoc([
      ("name", `String(req.name)),
      ("versions", `List(List.map(~f, resolutions))),
    ])
    |> Yojson.Safe.pretty_to_string
    |> print_endline;
    return();
  | _ =>
    switch (resolutions) {
    | [] => errorf("No package found for %a", Req.pp, req)
    | [resolution, ..._] =>
      let%bind pkg =
        RunAsync.contextf(
          Resolver.package(
            ~gitUsername=proj.projcfg.gitUsername,
            ~gitPassword=proj.projcfg.gitPassword,
            ~resolution,
            resolver,
          ),
          "resolving metadata %a",
          Resolution.pp,
          resolution,
        );

      let%bind pkg = RunAsync.ofStringError(pkg);
      InstallManifest.to_yojson(pkg)
      |> Yojson.Safe.pretty_to_string
      |> print_endline;
      return();
    }
  };
};

let printHeader = (~spec=?, name) =>
  switch (spec) {
  | Some(spec) =>
    let needReportProjectPath =
      Path.compare(
        spec.EsyInstall.SandboxSpec.path,
        EsyRuntime.currentWorkingDir,
      )
      != 0;

    if (needReportProjectPath) {
      Logs_lwt.app(m =>
        m(
          "%s %s (using %a)@;found project at %a",
          name,
          EsyRuntime.version,
          EsyInstall.SandboxSpec.pp,
          spec,
          Path.ppPretty,
          spec.path,
        )
      );
    } else {
      Logs_lwt.app(m =>
        m(
          "%s %s (using %a)",
          name,
          EsyRuntime.version,
          EsyInstall.SandboxSpec.pp,
          spec,
        )
      );
    };
  | None => Logs_lwt.app(m => m("%s %s", name, EsyRuntime.version))
  };

let default = (chdir, cmdAndPkg, proj: Project.t) => {
  open RunAsync.Syntax;
  let disableSandbox = false;
  let%lwt fetched = Project.fetched(proj);
  switch (fetched, cmdAndPkg) {
  | (Ok(_), None) =>
    let%lwt () = printHeader(~spec=proj.projcfg.spec, "esy");
    build(BuildDev, PkgArg.root, disableSandbox, None, proj);
  | (Ok(_), Some((None, cmd))) =>
    switch (Scripts.find(Cmd.getTool(cmd), proj.scripts)) {
    | Some(script) => runScript(script, Cmd.getArgs(cmd), proj)
    | None =>
      let pkgarg = PkgArg.ByDirectoryPath(Path.currentPath());
      devExec(chdir, pkgarg, proj, cmd, ());
    }
  | (Ok(_), Some((Some(pkgarg), cmd))) =>
    devExec(chdir, pkgarg, proj, cmd, ())
  | (Error(_), None) =>
    let%lwt () = printHeader(~spec=proj.projcfg.spec, "esy");
    let%bind () = solveAndFetch(proj);
    let%bind (proj, files) = Project.make(proj.projcfg);
    let%bind () = Project.write(proj, files);
    build(BuildDev, PkgArg.root, disableSandbox, None, proj);
  | (Error(_) as err, Some((None, cmd))) =>
    switch (Scripts.find(Cmd.getTool(cmd), proj.scripts)) {
    | Some(script) => runScript(script, Cmd.getArgs(cmd), proj)
    | None => Lwt.return(err)
    }
  | (Error(_) as err, Some(_)) => Lwt.return(err)
  };
};

let commonSection = "COMMON COMMANDS";
let aliasesSection = "ALIASES";
let introspectionSection = "INTROSPECTION COMMANDS";
let lowLevelSection = "LOW LEVEL PLUMBING COMMANDS";
let otherSection = "OTHER COMMANDS";

let makeCommand =
    (~header=`Standard, ~docs=?, ~doc=?, ~stop_on_pos=false, ~name, cmd) => {
  let info =
    Cmdliner.Term.info(
      ~exits=Cmdliner.Term.default_exits,
      ~docs?,
      ~doc?,
      ~stop_on_pos,
      ~version=EsyRuntime.version,
      name,
    );

  let cmd = {
    let f = comp => {
      let () =
        switch (header) {
        | `Standard => Lwt_main.run(printHeader(name))
        | `No => ()
        };

      Cli.runAsyncToCmdlinerRet(comp);
    };

    Cmdliner.Term.(ret(app(const(f), cmd)));
  };

  (cmd, info);
};

let makeAlias = (~docs=aliasesSection, ~stop_on_pos=false, command, alias) => {
  let (term, info) = command;
  let name = Cmdliner.Term.name(info);
  let doc = Printf.sprintf("An alias for $(b,%s) command", name);
  let info =
    Cmdliner.Term.info(
      alias,
      ~version=EsyRuntime.version,
      ~doc,
      ~docs,
      ~stop_on_pos,
    );

  (term, info);
};

let commandsConfig = {
  open Cmdliner;

  let makeProjectCommand =
      (~header=`Standard, ~docs=?, ~doc=?, ~stop_on_pos=?, ~name, cmd) => {
    let cmd = {
      let run = (cmd, project) => {
        let () =
          switch (header) {
          | `Standard =>
            Lwt_main.run(
              printHeader(~spec=project.Project.projcfg.spec, name),
            )
          | `No => ()
          };

        cmd(project);
      };

      Cmdliner.Term.(pure(run) $ cmd $ Project.term);
    };

    makeCommand(~header=`No, ~docs?, ~doc?, ~stop_on_pos?, ~name, cmd);
  };

  let defaultCommand =
    makeProjectCommand(
      ~header=`No,
      ~name="esy",
      ~doc="package.json workflow for native development with Reason/OCaml",
      ~docs=commonSection,
      ~stop_on_pos=true,
      Term.(const(default) $ chdirTerm $ cmdAndPkgTerm),
    );

  let commands = {
    let buildCommand = {
      let run =
          (
            mode,
            pkgarg,
            disableSandbox,
            install,
            skipStalenessCheck,
            cmd,
            proj,
          ) => {
        let () =
          switch (cmd) {
          | None =>
            Lwt_main.run(
              printHeader(~spec=proj.Project.projcfg.spec, "esy build"),
            )
          | Some(_) => ()
          };

        build(
          ~buildOnly=!install,
          ~skipStalenessCheck,
          mode,
          pkgarg,
          disableSandbox,
          cmd,
          proj,
        );
      };

      makeProjectCommand(
        ~header=`No,
        ~name="build",
        ~doc="Build the entire sandbox",
        ~docs=commonSection,
        ~stop_on_pos=true,
        Term.(
          const(run)
          $ modeTerm
          $ pkgTerm
          $ Arg.(
              value
              & flag
              & info(["disable-sandbox"], ~doc="Disables sandbox")
            )
          $ Arg.(
              value
              & flag
              & info(["install"], ~doc="Install built artifacts")
            )
          $ Arg.(
              value
              & flag
              & info(
                  ["skip-staleness-check"],
                  ~doc="Skip staleness check for link-dev: packages",
                )
            )
          $ Cli.cmdOptionTerm(
              ~doc="Command to execute within the build environment.",
              ~docv="COMMAND",
            )
        ),
      );
    };

    let installCommand =
      makeProjectCommand(
        ~name="install",
        ~doc="Solve & fetch dependencies",
        ~docs=commonSection,
        Term.(const(solveAndFetch)),
      );

    let staticArg =
      Cmdliner.Arg.(
        value
        & flag
        & info(
            ["static"],
            ~doc=
              "Ensures that wrappers binaries are statically linked. Useful on Alpine.",
          )
      );

    let noEnv =
      Cmdliner.Arg.(
        value
        & flag
        & info(
            ["no-env"],
            ~doc=
              "Ensures that wrappers binaries are not wrapped with the environment. Useful for debugging wrapped environments.",
          )
      );

    let npmReleaseCommand =
      makeProjectCommand(
        ~name="npm-release",
        ~doc="Produce npm package with prebuilt artifacts",
        ~docs=otherSection,
        Term.(const(NpmReleaseCommand.run) $ staticArg $ noEnv),
      );
    [
      /* COMMON COMMANDS */
      installCommand,
      buildCommand,
      makeProjectCommand(
        ~name="build-shell",
        ~doc="Enter the build shell",
        ~docs=commonSection,
        Term.(const(buildShell) $ modeTerm $ pkgTerm),
      ),
      makeProjectCommand(
        ~name="shell",
        ~doc="Enter esy sandbox shell",
        ~docs=commonSection,
        Term.(const(devShell) $ pkgTerm),
      ),
      makeProjectCommand(
        ~header=`No,
        ~name="x",
        ~doc="Execute command as if the package is installed",
        ~docs=commonSection,
        ~stop_on_pos=true,
        Term.(
          const(exec)
          $ modeTerm
          $ chdirTerm
          $ pkgTerm
          $ Arg.(
              value
              & flag
              & info(["disable-sandbox"], ~doc="Disables sandbox")
            )
          $ Cli.cmdTerm(
              ~doc="Command to execute within the sandbox environment.",
              ~docv="COMMAND",
              Cmdliner.Arg.pos_all,
            )
        ),
      ),
      makeProjectCommand(
        ~header=`No,
        ~name="run-script",
        ~doc="Execute project script",
        ~docs=commonSection,
        ~stop_on_pos=true,
        Term.(
          const(runScriptCommand)
          $ Cli.cmdTerm(
              ~doc="Script to execute within the project environment.",
              ~docv="SCRIPT",
              Cmdliner.Arg.pos_all,
            )
        ),
      ),
      makeProjectCommand(
        ~name="add",
        ~doc="Add a new dependency",
        ~docs=commonSection,
        Term.(
          const(add)
          $ Arg.(
              non_empty
              & pos_all(string, [])
              & info([], ~docv="PACKAGE", ~doc="Package to install")
            )
          $ Arg.(
              value
              & flag
              & info(["dev", "D"], ~doc="Install as a devDependency")
            )
        ),
      ),
      makeCommand(
        ~name="show",
        ~doc="Display information about available packages",
        ~docs=commonSection,
        ~header=`No,
        Term.(
          const(show)
          $ Arg.(
              value & flag & info(["json"], ~doc="Format output as JSON")
            )
          $ Arg.(
              required
              & pos(0, some(string), None)
              & info(
                  [],
                  ~docv="PACKAGE",
                  ~doc="Package to display information about",
                )
            )
          $ Project.term
        ),
      ),
      makeCommand(
        ~name="help",
        ~doc="Show this message and exit",
        ~docs=commonSection,
        Term.(ret(const(() => `Help((`Auto, None))) $ const())),
      ),
      makeCommand(
        ~name="version",
        ~doc="Print esy version and exit",
        ~docs=commonSection,
        Term.(
          const(() => {
            print_endline(EsyRuntime.version);
            RunAsync.return();
          })
          $ const()
        ),
      ),
      /* ALIASES */
      makeAlias(buildCommand, ~stop_on_pos=true, "b"),
      makeAlias(installCommand, "i"),
      /* OTHER COMMANDS */
      npmReleaseCommand,
      makeAlias(~docs=otherSection, npmReleaseCommand, "release"),
      makeProjectCommand(
        ~name="export-build",
        ~doc="Export build from the store",
        ~docs=otherSection,
        Term.(
          const(exportBuild)
          $ Arg.(
              required
              & pos(0, some(resolvedPathTerm), None)
              & info([], ~doc="Path with builds.")
            )
        ),
      ),
      makeCommand(
        ~name="import-build",
        ~doc="Import build into the store",
        ~docs=otherSection,
        Term.(
          const(importBuild)
          $ Arg.(
              value
              & opt(some(resolvedPathTerm), None)
              & info(["from", "f"], ~docv="FROM")
            )
          $ Arg.(
              value & pos_all(resolvedPathTerm, []) & info([], ~docv="BUILD")
            )
          $ ProjectConfig.term
        ),
      ),
      makeProjectCommand(
        ~name="export-dependencies",
        ~doc="Export sandbox dependendencies as prebuilt artifacts",
        ~docs=otherSection,
        Term.(const(exportDependencies) $ modeTerm),
      ),
      makeProjectCommand(
        ~name="import-dependencies",
        ~doc="Import sandbox dependencies",
        ~docs=otherSection,
        Term.(
          const(importDependencies)
          $ Arg.(
              value
              & pos(0, some(resolvedPathTerm), None)
              & info([], ~doc="Path with builds.")
            )
          $ modeTerm
        ),
      ),
      /* INTROSPECTION COMMANDS */
      makeProjectCommand(
        ~name="ls-builds",
        ~doc=
          "Output a tree of packages in the sandbox along with their status",
        ~docs=introspectionSection,
        Term.(
          const(lsBuilds)
          $ Arg.(
              value
              & flag
              & info(
                  ["T", "include-transitive"],
                  ~doc="Include transitive dependencies",
                )
            )
          $ modeTerm
          $ pkgTerm
        ),
      ),
      makeProjectCommand(
        ~name="ls-libs",
        ~doc=
          "Output a tree of packages along with the set of libraries made available by each package dependency.",
        ~docs=introspectionSection,
        Term.(
          const(lsLibs)
          $ Arg.(
              value
              & flag
              & info(
                  ["T", "include-transitive"],
                  ~doc="Include transitive dependencies",
                )
            )
          $ modeTerm
          $ pkgTerm
        ),
      ),
      makeProjectCommand(
        ~name="ls-modules",
        ~doc=
          "Output a tree of packages along with the set of libraries and modules made available by each package dependency.",
        ~docs=introspectionSection,
        Term.(
          const(lsModules)
          $ Arg.(
              value
              & pos_all(string, [])
              & info(
                  [],
                  ~docv="LIB",
                  ~doc="Output modules only for specified lib(s)",
                )
            )
          $ modeTerm
          $ pkgTerm
        ),
      ),
      makeCommand(
        ~header=`No,
        ~name="status",
        ~doc="Print esy sandbox status",
        ~docs=introspectionSection,
        Term.(
          const(status)
          $ Project.promiseTerm
          $ Arg.(
              value & flag & info(["json"], ~doc="Format output as JSON")
            )
          $ Cli.setupLogTerm
        ),
      ),
      makeProjectCommand(
        ~header=`No,
        ~name="build-plan",
        ~doc="Print build plan to stdout",
        ~docs=introspectionSection,
        Term.(const(buildPlan) $ modeTerm $ pkgTerm),
      ),
      makeProjectCommand(
        ~header=`No,
        ~name="build-env",
        ~doc="Print build environment to stdout",
        ~docs=introspectionSection,
        Term.(
          const(buildEnv)
          $ Arg.(
              value & flag & info(["json"], ~doc="Format output as JSON")
            )
          $ modeTerm
          $ pkgTerm
        ),
      ),
      makeProjectCommand(
        ~header=`No,
        ~name="command-env",
        ~doc="Print command environment to stdout",
        ~docs=introspectionSection,
        Term.(
          const(commandEnv)
          $ Arg.(
              value & flag & info(["json"], ~doc="Format output as JSON")
            )
          $ pkgTerm
        ),
      ),
      makeProjectCommand(
        ~header=`No,
        ~name="exec-env",
        ~doc="Print exec environment to stdout",
        ~docs=introspectionSection,
        Term.(
          const(execEnv)
          $ Arg.(
              value & flag & info(["json"], ~doc="Format output as JSON")
            )
          $ pkgTerm
        ),
      ),
      makeCommand(
        ~name="cleanup",
        ~doc="Purge unused builds from global cache",
        ~docs="COMMON COMMANDS",
        Term.(
          const(cleanup)
          $ ProjectConfig.multipleProjectConfigsTerm(resolvedPathTerm)
          $ Arg.(
              value
              & flag
              & info(
                  ["dry-run"],
                  ~doc=
                    "Only print directories/files to which should be removed.",
                )
            )
        ),
      ),
      /* LOW LEVEL PLUMBING COMMANDS */
      makeProjectCommand(
        ~name="build-dependencies",
        ~doc="Build dependencies for a specified package",
        ~docs=lowLevelSection,
        Term.(
          const(buildDependencies)
          $ Arg.(
              value
              & flag
              & info(
                  ["all"],
                  ~doc="Build all dependencies (including linked packages)",
                )
            )
          $ modeTerm
          $ pkgTerm
        ),
      ),
      makeProjectCommand(
        ~header=`No,
        ~name="exec-command",
        ~doc="Execute command in a given environment",
        ~docs=lowLevelSection,
        ~stop_on_pos=true,
        Term.(
          const(execCommand)
          $ Arg.(
              value
              & flag
              & info(
                  ["build-context"],
                  ~doc=
                    "Initialize package's build context before executing the command",
                )
            )
          $ Arg.(
              value
              & flag
              & info(["include-build-env"], ~doc="Include build environment")
            )
          $ Arg.(
              value
              & flag
              & info(
                  ["include-current-env"],
                  ~doc="Include current environment",
                )
            )
          $ Arg.(
              value
              & flag
              & info(
                  ["include-esy-introspection-env"],
                  ~doc="Include esy introspection environment",
                )
            )
          $ Arg.(
              value
              & flag
              & info(["include-npm-bin"], ~doc="Include npm bin in PATH")
            )
          $ modeTerm
          $ Arg.(
              value
              & opt(some(depspecConv), None)
              & info(
                  ["envspec"],
                  ~doc=
                    "Define DEPSPEC expression the command execution environment",
                  ~docv="DEPSPEC",
                )
            )
          $ chdirTerm
          $ pkgTerm
          $ Cli.cmdTerm(
              ~doc="Command to execute within the environment.",
              ~docv="COMMAND",
              Cmdliner.Arg.pos_all,
            )
        ),
      ),
      makeProjectCommand(
        ~header=`No,
        ~name="print-env",
        ~doc="Print a configured environment on stdout",
        ~docs=lowLevelSection,
        Term.(
          const(printEnv)
          $ Arg.(
              value & flag & info(["json"], ~doc="Format output as JSON")
            )
          $ Arg.(
              value
              & flag
              & info(["include-build-env"], ~doc="Include build environment")
            )
          $ Arg.(
              value
              & flag
              & info(
                  ["include-current-env"],
                  ~doc="Include current environment",
                )
            )
          $ Arg.(
              value
              & flag
              & info(
                  ["include-esy-introspection-env"],
                  ~doc="Include esy introspection environment",
                )
            )
          $ Arg.(
              value
              & flag
              & info(["include-npm-bin"], ~doc="Include npm bin in PATH")
            )
          $ modeTerm
          $ Arg.(
              value
              & opt(some(depspecConv), None)
              & info(
                  ["envspec"],
                  ~doc=
                    "Define DEPSPEC expression the command execution environment",
                  ~docv="DEPSPEC",
                )
            )
          $ pkgTerm
        ),
      ),
      makeProjectCommand(
        ~name="solve",
        ~doc="Solve dependencies and store the solution",
        ~docs=lowLevelSection,
        Term.(
          const(solve)
          $ Arg.(
              value
              & flag
              & info(
                  ["force"],
                  ~doc=
                    "Do not check if solution exist, run solver and produce new one",
                )
            )
          $ Arg.(
              value
              & opt(some(EsyLib.DumpToFile.conv), None)
              & info(
                  ["dump-cudf-request"],
                  ~doc="File to dump CUDF request ('-' for stdout)",
                  ~docv="FILENAME",
                )
            )
          $ Arg.(
              value
              & opt(some(EsyLib.DumpToFile.conv), None)
              & info(
                  ["dump-cudf-solution"],
                  ~doc="File to dump CUDF solution ('-' for stdout)",
                  ~docv="FILENAME",
                )
            )
        ),
      ),
      makeProjectCommand(
        ~name="fetch",
        ~doc="Fetch dependencies using the stored solution",
        ~docs=lowLevelSection,
        Term.(const(fetch)),
      ),
    ];
  };

  (defaultCommand, commands);
};

let () = {
  EsyLib.System.ensureMinimumFileDescriptors();

  let (defaultCommand, commands) = commandsConfig;

  /*
      Preparse command line arguments to expand syntax:

        esy @projectPath

      into

        esy --project projectPath

     which we can't parse with cmdliner
   */
  let argv = {
    let commandNames = {
      let f = (names, (_term, info)) => {
        let name = Cmdliner.Term.name(info);
        StringSet.add(name, names);
      };
      List.fold_left(~f, ~init=StringSet.empty, commands);
    };

    let argv = Array.to_list(Sys.argv);

    let argv =
      switch (argv) {
      | [] => argv
      | [prg, elem, maybeCommandName, ...rest] when elem.[0] == '@' =>
        let sandbox = String.sub(elem, 1, String.length(elem) - 1);
        if (StringSet.mem(maybeCommandName, commandNames)) {
          [prg, maybeCommandName, "--project", sandbox, ...rest];
        } else {
          [prg, "--project", sandbox, maybeCommandName, ...rest];
        };
      | [prg, elem] when elem.[0] == '@' =>
        let sandbox = String.sub(elem, 1, String.length(elem) - 1);
        [prg, "--project", sandbox];
      | _ => argv
      };

    Array.of_list(argv);
  };

  Cmdliner.Term.(
    exit @@ eval_choice(~main_on_err=true, ~argv, defaultCommand, commands)
  );
};
