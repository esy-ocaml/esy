open EsyPackageConfig;

module Solution = EsyInstall.Solution;
module Package = EsyInstall.Package;
module Installation = EsyInstall.Installation;

type t = {
  cfg: EsyBuildPackage.Config.t,
  spec: EsyInstall.SandboxSpec.t,
  installCfg: EsyInstall.Config.t,
  arch: System.Arch.t,
  platform: System.Platform.t,
  sandboxEnv: SandboxEnv.t,
  solution: Solution.t,
  installation: Installation.t,
  manifests: PackageId.Map.t(BuildManifest.t),
};

let readManifests =
    (cfg, installCfg, solution: Solution.t, installation: Installation.t) => {
  open RunAsync.Syntax;

  let%lwt () = Logs_lwt.debug(m => m("reading manifests: start"));

  let readManifest = ((id, loc)) => {
    let%lwt () =
      Logs_lwt.debug(m =>
        m(
          "reading manifest: %a %a",
          PackageId.pp,
          id,
          Installation.pp_location,
          loc,
        )
      );

    let pkg = Solution.getExn(solution, id);
    let isRoot = Package.compare(Solution.root(solution), pkg) == 0;

    RunAsync.contextf(
      {
        let* (manifest, paths) =
          ReadBuildManifest.ofInstallationLocation(cfg, installCfg, pkg, loc);

        switch (manifest) {
        | Some(manifest) => return((id, paths, Some(manifest)))
        | None =>
          if (isRoot) {
            let manifest =
              BuildManifest.empty(
                ~name=Some(pkg.name),
                ~version=Some(pkg.version),
                (),
              );

            return((id, paths, Some(manifest)));
          } else {
            /* we don't want to track non-esy manifest, hence Path.Set.empty */
            return((
              id,
              Path.Set.empty,
              None,
            ));
          }
        };
      },
      "reading manifest %a",
      PackageId.pp,
      id,
    );
  };

  let* items =
    RunAsync.List.mapAndJoin(
      ~concurrency=100,
      ~f=readManifest,
      Installation.entries(installation),
    );

  let (paths, manifests) = {
    let f = ((paths, manifests), (id, manifestPaths, manifest)) =>
      switch (manifest) {
      | None =>
        let paths = Path.Set.union(paths, manifestPaths);
        (paths, manifests);
      | Some(manifest) =>
        let paths = Path.Set.union(paths, manifestPaths);
        let manifests = PackageId.Map.add(id, manifest, manifests);
        (paths, manifests);
      };

    List.fold_left(~f, ~init=(Path.Set.empty, PackageId.Map.empty), items);
  };

  let%lwt () = Logs_lwt.debug(m => m("reading manifests: done"));

  return((paths, manifests));
};

let make =
    (
      ~sandboxEnv=SandboxEnv.empty,
      cfg,
      spec,
      installCfg,
      solution,
      installation,
    ) => {
  open RunAsync.Syntax;
  let* (paths, manifests) =
    readManifests(spec, installCfg, solution, installation);
  return((
    {
      cfg,
      spec,
      installCfg,
      platform: System.Platform.host,
      arch: System.Arch.host,
      sandboxEnv,
      solution,
      installation,
      manifests,
    },
    paths,
  ));
};

let renderExpression = (sandbox, scope, expr) => {
  open Run.Syntax;
  let* expr = Scope.render(~buildIsInProgress=false, scope, expr);
  return(Scope.SandboxValue.render(sandbox.cfg, expr));
};

module Task = {
  type t = {
    idrepr: BuildId.Repr.t,
    pkg: Package.t,
    scope: Scope.t,
    env: Scope.SandboxEnvironment.t,
    build: list(list(Scope.SandboxValue.t)),
    install: option(list(list(Scope.SandboxValue.t))),
  };

  let plan = (~env=?, t: t) => {
    let rootPath = Scope.rootPath(t.scope);
    let buildPath = Scope.buildPath(t.scope);
    let stagePath = Scope.stagePath(t.scope);
    let installPath = Scope.installPath(t.scope);
    let jbuilderHackEnabled =
      switch (Scope.buildType(t.scope), Scope.sourceType(t.scope)) {
      | (JbuilderLike, Transient) => true
      | (JbuilderLike, _) => false
      | (InSource, _)
      | (OutOfSource, _)
      | (Unsafe, _) => false
      };

    let env = Option.orDefault(~default=t.env, env);
    let depspec =
      Format.asprintf(
        "%a",
        EsyInstall.Solution.DepSpec.pp,
        Scope.depspec(t.scope),
      );

    {
      EsyBuildPackage.Plan.id: BuildId.show(Scope.id(t.scope)),
      name: t.pkg.name,
      version: Version.show(t.pkg.version),
      sourceType: Scope.sourceType(t.scope),
      buildType: Scope.buildType(t.scope),
      build: t.build,
      install: t.install,
      sourcePath: Scope.SandboxPath.toValue(Scope.sourcePath(t.scope)),
      rootPath: Scope.SandboxPath.toValue(rootPath),
      buildPath: Scope.SandboxPath.toValue(buildPath),
      stagePath: Scope.SandboxPath.toValue(stagePath),
      installPath: Scope.SandboxPath.toValue(installPath),
      jbuilderHackEnabled,
      env,
      depspec,
    };
  };

  let to_yojson = t => EsyBuildPackage.Plan.to_yojson(plan(t));

  let toPathWith = (cfg, t, make) =>
    Scope.SandboxPath.toPath(cfg, make(t.scope));

  let sourcePath = (cfg, t) => toPathWith(cfg, t, Scope.sourcePath);
  let buildPath = (cfg, t) => toPathWith(cfg, t, Scope.buildPath);
  let installPath = (cfg, t) => toPathWith(cfg, t, Scope.installPath);
  let logPath = (cfg, t) => toPathWith(cfg, t, Scope.logPath);
  let buildInfoPath = (cfg, t) => toPathWith(cfg, t, Scope.buildInfoPath);

  let pp = (fmt, task) => PackageId.pp(fmt, task.pkg.id);
};

let renderEsyCommands = (~env, ~buildIsInProgress, scope, commands) => {
  open Run.Syntax;
  let envScope = name =>
    switch (Scope.SandboxEnvironment.find(name, env)) {
    | Some(v) => Some(Scope.SandboxValue.show(v))
    | None => None
    };

  let renderArg = v => {
    let* v = Scope.render(~buildIsInProgress, scope, v);
    let v = Scope.SandboxValue.show(v);
    Run.ofStringError(EsyShellExpansion.render(~scope=envScope, v));
  };

  let renderCommand =
    fun
    | Command.Parsed(args) => {
        let f = arg => {
          let* arg = renderArg(arg);
          return(Scope.SandboxValue.v(arg));
        };

        Result.List.map(~f, args);
      }
    | Command.Unparsed(line) => {
        let* line = renderArg(line);
        let* args = ShellSplit.split(line);
        return(List.map(~f=Scope.SandboxValue.v, args));
      };

  switch (Result.List.map(~f=renderCommand, commands)) {
  | Ok(commands) => Ok(commands)
  | Error(err) => Error(err)
  };
};

let renderOpamCommands = (opamEnv, commands) =>
  Run.Syntax.(
    try({
      let commands = OpamFilter.commands(opamEnv, commands);
      let commands =
        List.map(~f=List.map(~f=Scope.SandboxValue.v), commands);
      return(commands);
    }) {
    | Failure(msg) => error(msg)
    }
  );

let renderOpamSubstsAsCommands = (_opamEnv, substs) => {
  open Run.Syntax;
  let commands = {
    let f = path => {
      let path = Path.addExt(".in", path);
      [
        Scope.SandboxValue.v("substs"),
        Scope.SandboxValue.v(Path.show(path)),
      ];
    };

    List.map(~f, substs);
  };

  return(commands);
};

let renderOpamPatchesToCommands = (opamEnv, patches) =>
  Run.Syntax.(
    Run.context(
      {
        let evalFilter =
          fun
          | (path, None) => return((path, true))
          | (path, Some(filter)) => {
              let* filter =
                try(return(OpamFilter.eval_to_bool(opamEnv, filter))) {
                | Failure(msg) => error(msg)
                };
              return((path, filter));
            };

        let* filtered = Result.List.map(~f=evalFilter, patches);

        let toCommand = ((path, _)) => {
          let cmd = ["patch", "--strip", "1", "--input", Path.show(path)];
          List.map(~f=Scope.SandboxValue.v, cmd);
        };

        return(
          filtered
          |> List.filter(~f=((_, v)) => v)
          |> List.map(~f=toCommand),
        );
      },
      "processing patch field",
    )
  );

module Reason = {
  [@deriving ord]
  type t =
    | ForBuild
    | ForScope;

  let (+) = (a, b) =>
    switch (a, b) {
    | (ForBuild, _)
    | (_, ForBuild) => ForBuild
    | (ForScope, ForScope) => ForScope
    };
};

let makeScope =
    (~cache=?, ~envspec=?, ~forceImmutable, buildspec, mode, sandbox, id) => {
  open Run.Syntax;

  let updateSeen = (seen, id) =>
    switch (List.find_opt(~f=p => PackageId.compare(p, id) == 0, seen)) {
    | Some(_) =>
      errorf(
        "@[<h>found circular dependency on: %a@]",
        PackageId.ppNoHash,
        id,
      )
    | None => return([id, ...seen])
    };

  let cache =
    switch (cache) {
    | None => Hashtbl.create(100)
    | Some(cache) => cache
    };

  let rec visit = (envspec, seen, id: PackageId.t) =>
    switch (Hashtbl.find_opt(cache, id)) {
    | Some(None) => return(None)
    | Some(Some(res)) =>
      let* _: list(PackageId.t) = updateSeen(seen, id);
      return(Some(res));
    | None =>
      let* res =
        switch (PackageId.Map.find_opt(id, sandbox.manifests)) {
        | Some(build) =>
          let* seen = updateSeen(seen, id);
          Run.contextf(
            {
              let* (scope, idrepr, directDependencies) =
                visit'(envspec, seen, id, build);
              return(Some((scope, build, idrepr, directDependencies)));
            },
            "processing %a",
            PackageId.ppNoHash,
            id,
          );
        | None => return(None)
        };

      Hashtbl.replace(cache, id, res);
      return(res);
    }
  and visit' = (envspec, seen, id, buildManifest) => {
    module IdS = PackageId.Set;
    let pkg = Solution.getExn(sandbox.solution, id);
    let location = Installation.findExn(id, sandbox.installation);

    let mode = BuildSpec.mode(mode, pkg);
    let depspec = BuildSpec.depspec(buildspec, mode, pkg);
    let buildCommands = BuildSpec.buildCommands(mode, pkg, buildManifest);

    let matchedForBuild =
      EsyInstall.Solution.eval(sandbox.solution, depspec, pkg.Package.id);

    let matchedForScope =
      switch (envspec) {
      | None => matchedForBuild
      | Some(envspec) =>
        EsyInstall.Solution.eval(sandbox.solution, envspec, pkg.Package.id)
      };

    let annotateWithReason = pkgid =>
      if (IdS.mem(pkgid, matchedForBuild)) {
        (Reason.ForBuild, pkgid);
      } else {
        (Reason.ForScope, pkgid);
      };

    let* dependencies = {
      module Seen =
        Set.Make({
          [@deriving ord]
          type t = (Reason.t, PackageId.t);
        });

      let collectAllDependencies = initDependencies => {
        let queue = Queue.create();
        let enqueue = (direct, dependencies) => {
          let f = id => Queue.add((direct, id), queue);
          List.iter(~f, dependencies);
        };

        let rec process = ((seen, reasons, dependencies)) =>
          switch (Queue.pop(queue)) {
          | exception Queue.Empty => (seen, reasons, dependencies)
          | (direct, (reason, id)) =>
            if (Seen.mem((reason, id), seen)) {
              process((seen, reasons, dependencies));
            } else {
              let node = Solution.getExn(sandbox.solution, id);
              let seen = Seen.add((reason, id), seen);
              let dependencies = [(direct, node), ...dependencies];
              let reasons = {
                let f = (
                  fun
                  | None => Some(reason)
                  | Some(prevreason) => Some(Reason.(reason + prevreason))
                );

                PackageId.Map.update(id, f, reasons);
              };

              let next =
                List.map(
                  ~f=
                    depid => {
                      let (depreason, depid) = annotateWithReason(depid);
                      (depreason, depid);
                    },
                  Solution.traverse(node),
                );

              enqueue(false, next);
              process((seen, reasons, dependencies));
            }
          };

        let (_, reasons, dependencies) = {
          enqueue(true, initDependencies);
          process((Seen.empty, PackageId.Map.empty, []));
        };

        let (_seen, dependencies) = {
          let f = ((seen, res), (direct, pkg)) =>
            if (IdS.mem(pkg.Package.id, seen)) {
              (seen, res);
            } else {
              let seen = IdS.add(pkg.id, seen);
              let reason = PackageId.Map.find(pkg.Package.id, reasons);
              (seen, [(direct, reason, pkg), ...res]);
            };

          List.fold_left(~f, ~init=(IdS.empty, []), dependencies);
        };

        dependencies;
      };

      let collect = (dependencies, (direct, reason, pkg)) =>
        switch%bind (visit(None, seen, pkg.Package.id)) {
        | Some((scope, _build, _idrepr, _directDependencies)) =>
          let _pkgid = Scope.pkg(scope).id;
          return([(direct, reason, scope), ...dependencies]);
        | None => return(dependencies)
        };

      let lineage = {
        let dependencies = {
          open PackageId.Set;
          let set = union(matchedForBuild, matchedForScope);
          let set = remove(pkg.Package.id, set);
          List.map(~f=annotateWithReason, elements(set));
        };
        collectAllDependencies(dependencies);
      };

      Result.List.foldLeft(~f=collect, ~init=[], lineage);
    };

    let sourceType =
      switch (pkg.source) {
      | Install(_) =>
        let hasTransientDeps = {
          let f = ((_direct, reason, scope)) =>
            switch (reason) {
            | Reason.ForBuild =>
              switch (Scope.sourceType(scope)) {
              | SourceType.Transient
              | SourceType.ImmutableWithTransientDependencies => true
              | SourceType.Immutable => false
              }
            | Reason.ForScope => false
            };

          List.exists(~f, dependencies);
        };

        let sourceType =
          if (hasTransientDeps) {SourceType.ImmutableWithTransientDependencies} else {
            SourceType.Immutable
          };

        sourceType;
      | Link(_) => SourceType.Transient
      };

    let sourceType =
      if (forceImmutable) {SourceType.Immutable} else {sourceType};

    let name = PackageId.name(id);
    let version = PackageId.version(id);

    let (id, idrepr) = {
      let dependencies = {
        let f =
          fun
          | (true, Reason.ForBuild, dep) => Some(Scope.id(dep))
          | (true, Reason.ForScope, _) => None
          | (false, _, _) => None;

        dependencies |> List.map(~f) |> List.filterNone;
      };

      BuildId.make(
        ~ocamlPkgName=sandbox.cfg.ocamlPkgName,
        ~sandboxEnv=sandbox.sandboxEnv,
        ~packageId=pkg.id,
        ~platform=sandbox.platform,
        ~arch=sandbox.arch,
        ~build=buildManifest,
        ~mode,
        ~dependencies,
        ~buildCommands,
        (),
      );
    };

    let sourcePath = Scope.SandboxPath.ofPath(sandbox.cfg, location);

    let sandboxEnv = {
      let f = (env, {BuildEnv.name, value}) =>
        // TODO: what should that do?
        switch (value) {
        | Set(value) => [
            (
              name,
              Scope.SandboxEnvironment.Bindings.value(
                name,
                Scope.SandboxValue.v(value),
              ),
            ),
          ]
        | Unset => env |> List.filter(~f=((key, _)) => name != key)
        };
      StringMap.values(sandbox.sandboxEnv)
      |> List.fold_left(~f, ~init=[])
      |> List.rev_map(~f=snd);
    };

    let scope =
      Scope.make(
        ~platform=sandbox.platform,
        ~sandboxEnv,
        ~id,
        ~name,
        ~version,
        ~sourceType,
        ~sourcePath,
        ~mode,
        ~depspec,
        ~globalPathVariable=sandbox.cfg.globalPathVariable,
        pkg,
        buildManifest,
      );

    let scope = {
      let (_seen, scope) = {
        let f = ((seen, scope), (direct, _reason, dep)) => {
          let id = Scope.id(dep);
          if (BuildId.Set.mem(id, seen)) {
            (seen, scope);
          } else {
            (BuildId.Set.add(id, seen), Scope.add(~direct, ~dep, scope));
          };
        };

        List.fold_left(~f, ~init=(BuildId.Set.empty, scope), dependencies);
      };

      if (IdS.mem(pkg.id, matchedForScope)) {
        Scope.add(~direct=true, ~dep=scope, scope);
      } else {
        scope;
      };
    };

    let directDependencies =
      PackageId.Set.(elements(remove(pkg.Package.id, matchedForBuild)));

    return((scope, idrepr, directDependencies));
  };

  visit(envspec, [], id);
};

module Plan = {
  type t = {
    buildspec: BuildSpec.t,
    mode: BuildSpec.mode,
    tasks: PackageId.Map.t(option(Task.t)),
  };

  let spec = plan => plan.buildspec;

  let get = (plan, id) =>
    switch (PackageId.Map.find_opt(id, plan.tasks)) {
    | None => None
    | Some(None) => None
    | Some(Some(task)) => Some(task)
    };

  let findBy = (plan, pred) => {
    let f = ((_id, node)) => pred(node);
    let bindings = PackageId.Map.bindings(plan.tasks);
    switch (List.find_opt(~f, bindings)) {
    | None => None
    | Some((_id, task)) => task
    };
  };

  let getByName = (plan, name) =>
    findBy(
      plan,
      fun
      | None => false
      | Some(task) => String.compare(task.Task.pkg.Package.name, name) == 0,
    );

  let getByNameVersion = (plan: t, name, version) => {
    let compare = [%derive.ord: (string, Version.t)];
    findBy(
      plan,
      fun
      | None => false
      | Some(task) =>
        compare(
          (task.Task.pkg.name, task.Task.pkg.version),
          (name, version),
        )
        == 0,
    );
  };

  let all = plan => {
    let f = tasks =>
      fun
      | (_, Some(task)) => [task, ...tasks]
      | (_, None) => tasks;

    List.fold_left(~f, ~init=[], PackageId.Map.bindings(plan.tasks));
  };

  let mode = plan => plan.mode;
};

let makePlan = (~forceImmutable=false, buildspec, mode, sandbox) => {
  open Run.Syntax;

  let cache = Hashtbl.create(100);

  let makeTask = pkg =>
    switch%bind (
      makeScope(~cache, ~forceImmutable, buildspec, mode, sandbox, pkg.id)
    ) {
    | None => return(None)
    | Some((scope, build, idrepr, _dependencies)) =>
      let* env = {
        let* bindings =
          Scope.env(~buildIsInProgress=true, ~includeBuildEnv=true, scope);
        Run.context(
          Run.ofStringError(
            Scope.SandboxEnvironment.Bindings.eval(bindings),
          ),
          "evaluating environment",
        );
      };

      let opamEnv = Scope.toOpamEnv(~buildIsInProgress=true, scope);

      let* buildCommands = {
        let commands = BuildSpec.buildCommands(mode, pkg, build);

        Run.context(
          switch (commands) {
          | BuildManifest.EsyCommands(commands) =>
            let* commands =
              renderEsyCommands(
                ~buildIsInProgress=true,
                ~env,
                scope,
                commands,
              );
            let* applySubstsCommands =
              renderOpamSubstsAsCommands(opamEnv, build.substs);
            let* applyPatchesCommands =
              renderOpamPatchesToCommands(opamEnv, build.patches);
            return(applySubstsCommands @ applyPatchesCommands @ commands);
          | OpamCommands(commands) =>
            let* commands = renderOpamCommands(opamEnv, commands);
            let* applySubstsCommands =
              renderOpamSubstsAsCommands(opamEnv, build.substs);
            let* applyPatchesCommands =
              renderOpamPatchesToCommands(opamEnv, build.patches);
            return(applySubstsCommands @ applyPatchesCommands @ commands);
          | NoCommands => return([])
          },
          "processing build commands",
        );
      };

      let* installCommands =
        Run.context(
          switch (build.BuildManifest.install) {
          | EsyCommands(commands) =>
            let* cmds =
              renderEsyCommands(
                ~buildIsInProgress=true,
                ~env,
                scope,
                commands,
              );
            return(Some(cmds));
          | OpamCommands(commands) =>
            let* cmds = renderOpamCommands(opamEnv, commands);
            return(Some(cmds));
          | NoCommands => return(None)
          },
          "processing esy.install",
        );

      let task = {
        Task.idrepr,
        pkg,
        scope,
        build: buildCommands,
        install: installCommands,
        env,
      };

      return(Some(task));
    };

  let* tasks = {
    let root = Solution.root(sandbox.solution);
    let rec visit = tasks =>
      fun
      | [] => return(tasks)
      | [id, ...ids] =>
        switch (PackageId.Map.find_opt(id, tasks)) {
        | Some(_) => visit(tasks, ids)
        | None =>
          let pkg = Solution.getExn(sandbox.solution, id);
          let* task =
            Run.contextf(
              makeTask(pkg),
              "creating task for %a",
              Package.pp,
              pkg,
            );

          let tasks = PackageId.Map.add(id, task, tasks);
          let ids = {
            let dependencies = {
              let depspec = BuildSpec.depspec(buildspec, mode, pkg);
              Solution.dependenciesByDepSpec(sandbox.solution, depspec, pkg);
            };

            List.map(~f=Package.id, dependencies) @ ids;
          };

          visit(tasks, ids);
        };

    visit(PackageId.Map.empty, [root.id]);
  };

  return({Plan.mode, tasks, buildspec});
};

let task = (buildspec, mode, sandbox, id) => {
  open RunAsync.Syntax;
  let* tasks = RunAsync.ofRun(makePlan(buildspec, mode, sandbox));
  switch (Plan.get(tasks, id)) {
  | None => errorf("no build found for %a", PackageId.pp, id)
  | Some(task) => return(task)
  };
};

let buildShell = (buildspec, mode, sandbox, id) => {
  open RunAsync.Syntax;
  let* task = task(buildspec, mode, sandbox, id);
  let plan = Task.plan(task);
  EsyBuildPackageApi.buildShell(sandbox.cfg, plan);
};

module EsyIntrospectionEnv = {
  let rootPackageConfigPath = "ESY__ROOT_PACKAGE_CONFIG_PATH";
};

let augmentEnvWithOptions = (envspec: EnvSpec.t, sandbox, scope) => {
  open Run.Syntax;

  let {
    EnvSpec.augmentDeps,
    buildIsInProgress,
    includeCurrentEnv,
    includeBuildEnv,
    includeEsyIntrospectionEnv,
    includeNpmBin,
  } = envspec;

  module Env = Scope.SandboxEnvironment.Bindings;
  module Val = Scope.SandboxValue;

  let* env = {
    let scope =
      if (includeCurrentEnv) {
        scope |> Scope.exposeUserEnvWith(Env.value, "SHELL");
      } else {
        scope;
      };

    Scope.env(~includeBuildEnv, ~buildIsInProgress, scope);
  };

  let env =
    if (includeNpmBin) {
      let npmBin = Path.show(EsyInstall.SandboxSpec.binPath(sandbox.spec));
      [Env.prefixValue("PATH", Val.v(npmBin)), ...env];
    } else {
      env;
    };

  let env =
    if (includeCurrentEnv) {
      Env.current @ env;
    } else {
      env;
    };

  let env =
    if (includeEsyIntrospectionEnv) {
      switch (EsyInstall.SandboxSpec.manifestPath(sandbox.spec)) {
      | None => env
      | Some(path) => [
          Env.value(
            EsyIntrospectionEnv.rootPackageConfigPath,
            Val.v(Path.show(path)),
          ),
          ...env,
        ]
      };
    } else {
      env;
    };

  let env =
    /* if envspec's DEPSPEC expression was provided we need to filter out env
     * bindings according to it. */
    switch (augmentDeps) {
    | None => env
    | Some(depspec) =>
      let matched =
        EsyInstall.Solution.collect(
          sandbox.solution,
          depspec,
          Scope.pkg(scope).id,
        );

      let matched =
        matched
        |> PackageId.Set.elements
        |> List.map(~f=PackageId.show)
        |> StringSet.of_list;

      let f = binding =>
        switch (Environment.Binding.origin(binding)) {
        | None => true
        | Some(pkgid) => StringSet.mem(pkgid, matched)
        };

      List.filter(~f, env);
    };

  return((env, scope));
};

let configure = (~forceImmutable=false, envspec, buildspec, mode, sandbox, id) => {
  open Run.Syntax;
  let cache = Hashtbl.create(100);

  let* scope = {
    open EnvSpec;
    let scope =
      makeScope(
        ~cache,
        ~forceImmutable,
        ~envspec=?envspec.augmentDeps,
        buildspec,
        mode,
        sandbox,
        id,
      );

    switch%bind (scope) {
    | None => errorf("no build found for %a", PackageId.pp, id)
    | Some((scope, _, _, _)) => return(scope)
    };
  };

  augmentEnvWithOptions(envspec, sandbox, scope);
};

let env = (~forceImmutable=?, envspec, buildspec, mode, sandbox, id) => {
  open Run.Syntax;
  let%map (env, _scope) =
    configure(~forceImmutable?, envspec, buildspec, mode, sandbox, id);
  env;
};

let exec =
    (
      ~changeDirectoryToPackageRoot=false,
      envspec,
      buildspec,
      mode,
      sandbox,
      id,
      cmd,
    ) => {
  open RunAsync.Syntax;
  let* (env, scope) =
    RunAsync.ofRun(
      {
        open Run.Syntax;
        let* (env, scope) = configure(envspec, buildspec, mode, sandbox, id);
        let* env =
          Run.ofStringError(Scope.SandboxEnvironment.Bindings.eval(env));
        return((env, scope));
      },
    );

  let* cmd =
    RunAsync.ofRun(
      {
        open Run.Syntax;

        let expand = v => {
          let* v =
            Scope.render(
              ~env,
              ~buildIsInProgress=envspec.EnvSpec.buildIsInProgress,
              scope,
              v,
            );
          return(Scope.SandboxValue.render(sandbox.cfg, v));
        };

        let (tool, args) = Cmd.getToolAndArgs(cmd);
        let* tool = expand(tool);
        let* args = Result.List.map(~f=expand, args);
        return(Cmd.ofToolAndArgs((tool, args)));
      },
    );

  if (envspec.EnvSpec.buildIsInProgress) {
    let* task = task(buildspec, mode, sandbox, id);
    let plan = Task.plan(~env, task);
    EsyBuildPackageApi.buildExec(sandbox.cfg, plan, cmd);
  } else {
    let waitForProcess = process => {
      let%lwt status = process#status;
      return(status);
    };

    let cwd =
      changeDirectoryToPackageRoot
        ? Some(
            Scope.(
              rootPath(scope)
              |> SandboxPath.toValue
              |> SandboxValue.render(sandbox.cfg)
            ),
          )
        : None;

    let env = Scope.SandboxEnvironment.render(sandbox.cfg, env);

    /* TODO: make sure we resolve 'esy' to the current executable, needed nested
     * invokations */
    ChildProcess.withProcess(
      ~env=CustomEnv(env),
      ~resolveProgramInEnv=true,
      ~cwd?,
      ~stderr=`FD_copy(Unix.stderr),
      ~stdout=`FD_copy(Unix.stdout),
      ~stdin=`FD_copy(Unix.stdin),
      cmd,
      waitForProcess,
    );
  };
};

let findMaxModifyTime = path => {
  open RunAsync.Syntax;
  let skipTraverse = path =>
    switch (Path.basename(path)) {
    | "node_modules"
    | ".git"
    | ".hg"
    | ".svn"
    | ".merlin"
    | "esy.lock"
    | "_esy"
    | "_release"
    | "_build"
    | "_install" => true
    | _ =>
      switch (Path.getExt(path)) {
      /* dune can touch this */
      | ".install" => true
      | _ => false
      }
    };

  let reduce = ((prevpath, prevmtime), filepath, stat) => {
    let mtime = stat.Unix.st_mtime;
    if (mtime > prevmtime) {
      (filepath, mtime);
    } else {
      (prevpath, prevmtime);
    };
  };

  let rec f = (value, filepath, stat) =>
    switch (stat.Unix.st_kind) {
    | Unix.S_LNK =>
      let* targetpath = Fs.readlink(filepath);
      let targetpath =
        Path.(normalize(append(parent(filepath), targetpath)));
      /* check first if link itself has modified mtime, if not - traverse it */
      let value = reduce(value, filepath, stat);
      switch%lwt (Fs.lstat(targetpath)) {
      | Ok(targetstat) => f(value, targetpath, targetstat)
      | Error(_) => return(value)
      };
    | _ =>
      let value = reduce(value, filepath, stat);
      return(value);
    };

  let label = Printf.sprintf("computing mtime for %s", Path.show(path));
  Perf.measureLwt(
    ~label,
    () => {
      let value = (path, 0.0);
      let* (path, mtime) = Fs.fold(~skipTraverse, ~f, ~init=value, path);
      return((path, BuildInfo.ModTime.v(mtime)));
    },
  );
};

module Changes = {
  type t =
    | Yes
    | No;

  let (+) = (a, b) =>
    switch (a, b) {
    | (No, No) => No
    | _ => Yes
    };

  let pp = fmt =>
    fun
    | Yes => Fmt.unit("yes", fmt, ())
    | No => Fmt.unit("no", fmt, ());
};

let isBuilt = (sandbox, task) =>
  Fs.exists(Task.installPath(sandbox.cfg, task));

let makeSymlinksToStore = (sandbox, task) => {
  open RunAsync.Syntax;
  let addSuffix = p =>
    switch (Scope.mode(task.Task.scope)) {
    | BuildDev => p
    | Build => Path.v(Path.show(p) ++ "-release")
    };
  let* () =
    Fs.symlink(
      ~force=true,
      ~src=Task.buildPath(sandbox.cfg, task),
      addSuffix(EsyInstall.SandboxSpec.buildPath(sandbox.spec)),
    );
  let* () =
    Fs.symlink(
      ~force=true,
      ~src=Task.installPath(sandbox.cfg, task),
      addSuffix(EsyInstall.SandboxSpec.installPath(sandbox.spec)),
    );
  return();
};

let buildTask =
    (~quiet=?, ~buildOnly=?, ~logPath=?, ~disableSandbox=?, sandbox, task) => {
  open RunAsync.Syntax;
  let%lwt () = Logs_lwt.debug(m => m("build %a", Task.pp, task));
  let plan = Task.plan(task);
  let label = Fmt.strf("build %a", Task.pp, task);
  let* () =
    Perf.measureLwt(~label, () =>
      EsyBuildPackageApi.build(
        ~quiet?,
        ~buildOnly?,
        ~logPath?,
        ~disableSandbox?,
        sandbox.cfg,
        plan,
      )
    );
  let* () =
    switch (
      Solution.isRoot(sandbox.solution, task.pkg),
      System.Platform.host,
    ) {
    | (_, System.Platform.Windows) => return()
    | (true, _) => makeSymlinksToStore(sandbox, task)
    | (false, _) => return()
    };
  return();
};

let buildOnly =
    (
      ~force,
      ~quiet=?,
      ~buildOnly=?,
      ~logPath=?,
      ~disableSandbox=?,
      sandbox,
      plan,
      id,
    ) =>
  RunAsync.Syntax.(
    switch (Plan.get(plan, id)) {
    | Some(task) =>
      if (!force) {
        if%bind (isBuilt(sandbox, task)) {
          return();
        } else {
          buildTask(
            ~quiet?,
            ~buildOnly?,
            ~logPath?,
            ~disableSandbox?,
            sandbox,
            task,
          );
        };
      } else {
        buildTask(
          ~quiet?,
          ~buildOnly?,
          ~logPath?,
          ~disableSandbox?,
          sandbox,
          task,
        );
      }
    | None => RunAsync.return()
    }
  );

let build' =
    (~skipStalenessCheck, ~concurrency, ~buildLinked, sandbox, plan, ids) => {
  open RunAsync.Syntax;
  let%lwt () =
    Logs_lwt.debug(m => m("buildDependencies ~concurrency:%i", concurrency));

  let findMaxModifyTimeMem = {
    let mem = Memoize.make();
    path => Memoize.compute(mem, path, () => findMaxModifyTime(path));
  };

  let checkFreshModifyTime = (infoPath, sourcePath) => {
    open RunAsync.Syntax;

    let prevmtime =
      Lwt.catch(
        () =>
          switch%bind (BuildInfo.ofFile(infoPath)) {
          | Some(info) => return(info.BuildInfo.sourceModTime)
          | None => return(None)
          },
        _exn => return(None),
      );

    let* (mpath, mtime) = findMaxModifyTimeMem(sourcePath);
    switch%bind (prevmtime) {
    | None =>
      let%lwt () =
        Logs_lwt.debug(m => m("no mtime info found: %a", Path.pp, mpath));
      return((Changes.Yes, mtime));
    | Some(prevmtime) =>
      if (!BuildInfo.ModTime.equal(mtime, prevmtime)) {
        let%lwt () =
          Logs_lwt.debug(m =>
            m(
              "path changed: %a %a (prev %a)",
              Path.pp,
              mpath,
              BuildInfo.ModTime.pp,
              mtime,
              BuildInfo.ModTime.pp,
              prevmtime,
            )
          );
        return((Changes.Yes, mtime));
      } else {
        return((Changes.No, mtime));
      }
    };
  };

  let queue = LwtTaskQueue.create(~concurrency, ());

  let run = (~quiet, task, ()) => {
    let start = Unix.gettimeofday();
    let%lwt () =
      if (!quiet) {
        Logs_lwt.app(m => m("building %a", Task.pp, task));
      } else {
        Lwt.return();
      };

    let logPath = Task.logPath(sandbox.cfg, task);
    let* () = buildTask(~logPath, sandbox, task);
    let%lwt () =
      if (!quiet) {
        Logs_lwt.app(m => m("building %a: done", Task.pp, task));
      } else {
        Lwt.return();
      };

    let stop = Unix.gettimeofday();
    return(stop -. start);
  };

  let runIfNeeded = (changesInDependencies, task) => {
    let infoPath = Task.buildInfoPath(sandbox.cfg, task);
    let sourcePath = Task.sourcePath(sandbox.cfg, task);
    let* isBuilt = isBuilt(sandbox, task);

    let runAndRecordSourceModTime = sourceModTime => {
      let* timeSpent = LwtTaskQueue.submit(queue, run(~quiet=false, task));
      let* () =
        BuildInfo.toFile(
          infoPath,
          {BuildInfo.idInfo: task.idrepr, timeSpent, sourceModTime},
        );
      return();
    };

    switch (Scope.sourceType(task.scope)) {
    | SourceType.Transient =>
      if (skipStalenessCheck) {
        let* () = runAndRecordSourceModTime(None);
        return(Changes.Yes);
      } else {
        let* (changesInSources, mtime) =
          checkFreshModifyTime(infoPath, sourcePath);
        switch (isBuilt, Changes.(changesInDependencies + changesInSources)) {
        | (true, Changes.No) =>
          let%lwt () =
            Logs_lwt.debug(m =>
              m(
                "building %a: skipping (changesInDependencies: %a, changesInSources: %a)",
                Task.pp,
                task,
                Changes.pp,
                changesInDependencies,
                Changes.pp,
                changesInSources,
              )
            );
          return(Changes.No);
        | (true, Changes.Yes)
        | (false, _) =>
          let* () = runAndRecordSourceModTime(Some(mtime));
          return(Changes.Yes);
        };
      }
    | SourceType.ImmutableWithTransientDependencies =>
      switch (isBuilt, changesInDependencies) {
      | (true, Changes.No) =>
        let%lwt () =
          Logs_lwt.debug(m =>
            m(
              "building %a: skipping (changesInDependencies: %a)",
              Task.pp,
              task,
              Changes.pp,
              changesInDependencies,
            )
          );
        return(Changes.No);
      | (true, Changes.Yes)
      | (false, _) =>
        let* () = runAndRecordSourceModTime(None);
        return(Changes.Yes);
      }
    | SourceType.Immutable =>
      if (isBuilt) {
        return(Changes.No);
      } else {
        let* () = runAndRecordSourceModTime(None);
        return(Changes.No);
      }
    };
  };

  let tasksInProcess = Hashtbl.create(100);

  let rec process = pkg => {
    let id = pkg.Package.id;
    switch (Hashtbl.find_opt(tasksInProcess, id)) {
    | None =>
      let running =
        switch (Plan.get(plan, id)) {
        | Some(task) =>
          let dependencies = {
            let depspec = Scope.depspec(task.scope);
            Solution.dependenciesByDepSpec(
              sandbox.solution,
              depspec,
              task.pkg,
            );
          };

          let* changes = processMany(dependencies);
          switch (buildLinked, task.Task.pkg.source) {
          | (false, Link(_)) => return(changes)
          | (_, _) =>
            RunAsync.contextf(
              runIfNeeded(changes, task),
              "building %a",
              PackageId.ppNoHash,
              id,
            )
          };
        | None => RunAsync.return(Changes.No)
        };

      Hashtbl.replace(tasksInProcess, id, running);
      running;
    | Some(running) => running
    };
  }
  and processMany = dependencies => {
    let* changes = RunAsync.List.mapAndJoin(~f=process, dependencies);
    let changes = List.fold_left(~f=Changes.(+), ~init=Changes.No, changes);
    return(changes);
  };

  let* pkgs =
    RunAsync.ofRun(
      {
        open Run.Syntax;
        let f = id =>
          switch (Solution.get(sandbox.solution, id)) {
          | None => Run.errorf("no such package %a", PackageId.pp, id)
          | Some(pkg) => return(pkg)
          };

        Result.List.map(~f, ids);
      },
    );

  let* _: Changes.t = processMany(pkgs);
  return();
};

let build =
    (~skipStalenessCheck, ~concurrency=1, ~buildLinked, sandbox, plan, ids) =>
  Perf.measureLwt(~label="build", () =>
    build'(
      ~skipStalenessCheck,
      ~concurrency,
      ~buildLinked,
      sandbox,
      plan,
      ids,
    )
  );

let exportBuild = (cfg, ~outputPrefixPath, buildPath) => {
  open RunAsync.Syntax;
  let buildId = Path.basename(buildPath);
  let%lwt () = Logs_lwt.app(m => m("Exporting %s", buildId));
  let outputPath =
    Path.(outputPrefixPath / Printf.sprintf("%s.tar.gz", buildId));
  let* (origPrefix, destPrefix) = {
    let* prevStorePrefix =
      Fs.readFile(Path.(buildPath / "_esy" / "storePrefix"));
    let nextStorePrefix =
      switch (System.Platform.host) {
      | Windows =>
        /* Keep the slashes segments in the path.  It's important for doing
         * replacement of double backslashes in artifacts.  */
        String.split_on_char('\\', prevStorePrefix)
        |> List.map(~f=seg => String.make(String.length(seg), '_'))
        |> String.concat("\\")
      | _ => String.make(String.length(prevStorePrefix), '_')
      };
    return((Path.v(prevStorePrefix), Path.v(nextStorePrefix)));
  };

  let* stagePath = {
    let path = Path.(cfg.EsyBuildPackage.Config.storePath / "s" / buildId);
    let* () = Fs.rmPath(path);
    let* () = Fs.copyPath(~src=buildPath, ~dst=path);
    return(path);
  };

  let* () = RewritePrefix.rewritePrefix(~origPrefix, ~destPrefix, stagePath);
  let* () = Fs.createDir(Path.parent(outputPath));
  let* () =
    Tarball.create(
      ~filename=outputPath,
      ~outpath=buildId,
      Path.parent(stagePath),
    );

  let%lwt () = Logs_lwt.app(m => m("Exporting %s: done", buildId));
  /* `Fs.rmPath` needs the same fix we made for `Bos.OS.Path.delete`
   * readonly files need to have their readonly bit off just before
   * deleting. (https://github.com/esy/esy/pull/1122)
   * Temporarily commenting `Fs.rmPath` and using the Bos
   * equivalent as a stopgap.
   */
  /* let* () = Fs.rmPath(stagePath); */
  let%lwt () =
    switch (Bos.OS.Path.delete(~must_exist=false, ~recurse=true, stagePath)) {
    | Ok () => Lwt.return()
    | Error(e) =>
      switch (e) {
      | `Msg(message) => Logs_lwt.debug(m => m("%s", message))
      }
    };
  return();
};

let importBuild = (storePath, buildPath) => {
  open RunAsync.Syntax;
  let (buildId, kind) =
    if (Path.hasExt("tar.gz", buildPath)) {
      (buildPath |> Path.remExt |> Path.remExt |> Path.basename, `Archive);
    } else {
      (buildPath |> Path.basename, `Dir);
    };

  let%lwt () = Logs_lwt.app(m => m("Import %s", buildId));
  let outputPath = Path.(storePath / Store.installTree / buildId);
  if%bind (Fs.exists(outputPath)) {
    let%lwt () =
      Logs_lwt.app(m =>
        m("Import %s: already in store, skipping...", buildId)
      );
    return();
  } else {
    let importFromDir = buildPath => {
      let* origPrefix = {
        let* v = Fs.readFile(Path.(buildPath / "_esy" / "storePrefix"));
        return(Path.v(v));
      };

      let* () =
        RewritePrefix.rewritePrefix(
          ~origPrefix,
          ~destPrefix=storePath,
          buildPath,
        );

      let* () = Fs.rename(~skipIfExists=true, ~src=buildPath, outputPath);
      let%lwt () = Logs_lwt.app(m => m("Import %s: done", buildId));
      return();
    };

    switch (kind) {
    | `Dir =>
      let* stagePath = {
        let path = Path.(storePath / "s" / buildId);
        let* () = Fs.rmPath(path);
        let* () = Fs.copyPath(~src=buildPath, ~dst=path);
        return(path);
      };

      importFromDir(stagePath);
    | `Archive =>
      let stagePath = Path.(storePath / Store.stageTree / buildId);
      let* () = {
        let cmd =
          Cmd.(
            v("tar")
            % "-C"
            % p(Path.parent(stagePath))
            % "-xz"
            % "-f"
            % p(buildPath)
          );
        ChildProcess.run(cmd);
      };

      importFromDir(stagePath);
    };
  };
};
