open EsyPackageConfig;

module EsyBash = EsyLib.EsyBash;
module Fs = EsyLib.Fs;
module Path = EsyLib.Path;
module Option = EsyLib.Option;
module Result = EsyLib.Result;
module System = EsyLib.System;
open Run;

type t = {
  plan: Plan.t,
  storePath: Path.t,
  sourcePath: Path.t,
  rootPath: Path.t,
  installPath: Path.t,
  stagePath: Path.t,
  buildPath: Path.t,
  prefixPath: Path.t,
  lockPath: Path.t,
  env: Bos.OS.Env.t,
  files: list(file),
  build: list(Cmd.t),
  install: option(list(Cmd.t)),
  sandbox: Sandbox.sandbox,
}
and file = {
  path: EsyLib.Path.t,
  content: string,
};

type build = t;

let isRoot = (build: t) =>
  Config.Value.compare(build.plan.sourcePath, Config.Value.project) == 0;

let regex = (base, segments) => {
  let pat = String.concat(Path.dirSep, [Path.show(base), ...segments]);
  Sandbox.Regex(pat);
};

let relocateSourcePath = (sourcePath, rootPath) => {
  let%bind () = rm(rootPath);
  let%bind () = mkdir(rootPath);
  let%bind () = {
    let ignore = [
      ".git",
      ".hg",
      ".svn",
      "node_modules",
      "_esy",
      "_build",
      "_install",
      "_release",
      "_esybuild",
      "_esyinstall",
    ];
    copyContents(~ignore, ~from=sourcePath, rootPath);
  };
  ok;
};

/*

  A special lifecycle designed to be compatible with jbuilder's use of _build
  subdirectory as a build dir.

 */
module JbuilderHack = {
  let prepareImpl = (build: build) => {
    let savedBuild = build.buildPath / "_build";
    let currentBuild = build.sourcePath / "_build";
    let backupBuild = build.sourcePath / "_build.prev";

    let%bind () =
      if%bind (exists(currentBuild)) {
        mv(currentBuild, backupBuild);
      } else {
        ok;
      };
    let%bind () = mkdir(savedBuild);
    let%bind () = mv(savedBuild, currentBuild);
    ok;
  };

  let commitImpl = (build: build) => {
    let savedBuild = build.buildPath / "_build";
    let currentBuild = build.sourcePath / "_build";
    let backupBuild = build.sourcePath / "_build.prev";

    let%bind () =
      if%bind (exists(currentBuild)) {
        mv(currentBuild, savedBuild);
      } else {
        ok;
      };
    let%bind () =
      if%bind (exists(backupBuild)) {
        mv(backupBuild, currentBuild);
      } else {
        ok;
      };
    ok;
  };

  let prepare = (build: build) =>
    if (build.plan.jbuilderHackEnabled) {
      if (isRoot(build)) {
        let duneBuildDir = build.sourcePath / "_build";
        let%bind () =
          switch (lstat(duneBuildDir)) {
          | Ok({Unix.st_kind: S_DIR, _}) => ok
          | Ok(_) => rm(duneBuildDir)
          | Error(_) => ok
          };
        ok;
      } else {
        prepareImpl(build);
      };
    } else {
      ok;
    };

  let finalize = (build: build) =>
    if (build.plan.jbuilderHackEnabled) {
      if (isRoot(build)) {
        ok;
      } else {
        commitImpl(build);
      };
    } else {
      ok;
    };
};

let configureBuild = (~cfg: Config.t, plan: Plan.t) => {
  let%bind env = {
    let f = (k, v) =>
      fun
      | Ok(result) => {
          let v = Config.Value.render(cfg, v);
          Ok(Astring.String.Map.add(k, v, result));
        }
      | error => error;
    Astring.String.Map.fold(f, plan.env, Ok(Astring.String.Map.empty));
  };

  let%bind files = {
    let f = ({Plan.path, content}) => {
      open Config.Value;
      let path = Path.v(render(cfg, path));
      let content = render(cfg, content);

      Ok({path, content});
    };
    Result.List.map(~f, plan.files);
  };

  let renderCommands = (~cfg, cmds) => {
    let f = cmd => {
      let cmd = List.map(Config.Value.render(cfg), cmd);
      return(Cmd.of_list(cmd));
    };
    Result.List.map(~f, cmds);
  };
  let%bind build = renderCommands(~cfg, plan.build);
  let%bind install =
    switch (plan.install) {
    | Some(cmds) =>
      let%bind cmds = renderCommands(~cfg, cmds);
      return(Some(cmds));
    | None => return(None)
    };

  let storePath =
    switch (plan.sourceType) {
    | Immutable => cfg.storePath
    | ImmutableWithTransientDependencies
    | Transient => cfg.localStorePath
    };

  let p = path => Path.v(Config.Value.render(cfg, path));
  let prefixPath = p(plan.prefixPath);
  let sourcePath = p(plan.sourcePath);
  let installPath = p(plan.installPath);
  let buildPath = p(plan.buildPath);
  let stagePath = p(plan.stagePath);
  let rootPath = p(plan.rootPath);
  let lockPath =
    Path.(storePath / EsyLib.Store.buildTree / plan.id |> addExt(".lock"));

  let%bind sandbox = {
    let%bind config = {
      let%bind tempPath = {
        let v = Path.v(Bos.OS.Env.opt_var("TMPDIR", ~absent="/tmp"));
        let%bind v = realpath(v);
        Ok(Path.show(v));
      };
      let allowWrite = [
        regex(sourcePath, [".*", "\\.merlin"]),
        regex(sourcePath, ["\\.merlin"]),
        regex(sourcePath, [".*\\.install"]),
        regex(sourcePath, ["dune-project"]),
        Subpath(Path.show(buildPath)),
        Subpath(Path.show(stagePath)),
        Subpath("/private/tmp"),
        Subpath("/tmp"),
        Subpath(tempPath),
      ];
      let allowWrite =
        if (plan.buildType == BuildType.Unsafe) {
          [Sandbox.Subpath(Path.show(sourcePath)), ...allowWrite];
        } else {
          allowWrite;
        };
      let allowWrite =
        if (plan.jbuilderHackEnabled) {
          Sandbox.[
            Subpath(Path.show(sourcePath / "_build")),
            regex(sourcePath, [".*", "[^/]*\\.install"]),
            regex(sourcePath, ["[^/]*\\.install"]),
            regex(sourcePath, [".*", "[^/]*\\.opam"]),
            regex(sourcePath, ["[^/]*\\.opam"]),
            regex(sourcePath, [".*", "jbuild-ignore"]),
          ]
          @ allowWrite;
        } else {
          allowWrite;
        };
      Ok({Sandbox.allowWrite: allowWrite});
    };
    Sandbox.init(config, ~noSandbox=cfg.disableSandbox);
  };

  return({
    plan,
    env,
    files,
    build,
    install,
    sourcePath,
    rootPath,
    storePath,
    installPath,
    prefixPath,
    stagePath,
    buildPath,
    lockPath,
    sandbox,
  });
};

let install = (~enableLinkingOptimization, ~prefixPath, installFilename) => {
  let label = Fmt.(strf("esy-installer: %a", Path.pp, installFilename));
  EsyLib.Perf.measure(
    ~label,
    () => {
      Logs.app(m =>
        m("# esy-build-package: installing using built-in installer")
      );
      let res =
        Install.install(
          ~enableLinkingOptimization,
          ~prefixPath,
          installFilename,
        );
      Run.coerceFromClosed(res);
    },
  );
};

let withLock = (lockPath: Path.t, f) => {
  let lockPath = Path.show(lockPath);
  let fd =
    UnixLabels.(
      openfile(
        ~mode=[O_WRONLY, O_CREAT, O_TRUNC, O_SYNC],
        ~perm=0o640,
        lockPath,
      )
    );
  let release = () => {
    UnixLabels.(lockf(fd, ~mode=F_ULOCK, ~len=0));
    Unix.close(fd);
  };
  UnixLabels.(lockf(fd, ~mode=F_TLOCK, ~len=0));
  let res =
    try({
      let res = f();
      release();
      res;
    }) {
    | e =>
      release();
      raise(e);
    };
  res;
};

let commitBuildToStore = (config: Config.t, build: build) => {
  let%bind () =
    write(
      ~data=Path.show(config.storePath),
      Path.(build.stagePath / "_esy" / "storePrefix"),
    );
  let%bind () =
    if (Path.compare(build.stagePath, build.installPath) == 0) {
      Logs.app(m =>
        m("# esy-build-package: stage path and install path are the same")
      );
      return();
    } else {
      Logs.app(m =>
        m(
          "# esy-build-package: rewriting prefix: %a -> %a",
          Path.pp,
          build.stagePath,
          Path.pp,
          build.installPath,
        )
      );
      let env = EsyLib.EsyBash.currentEnvWithMingwInPath;
      let%bind cmd =
        EsyLib.NodeResolution.resolve("./esyRewritePrefixCommand.exe");
      let%bind () =
        Bos.OS.Cmd.run(
          ~env,
          Cmd.(
            v(p(cmd))
            % "--orig-prefix"
            % p(build.stagePath)
            % "--dest-prefix"
            % p(build.installPath)
            % p(build.stagePath)
          ),
        );
      Logs.app(m =>
        m(
          "# esy-build-package: committing: %a -> %a",
          Path.pp,
          build.stagePath,
          Path.pp,
          build.installPath,
        )
      );
      let%bind () = mv(build.stagePath, build.installPath);
      return();
    };
  ok;
};

let makePrefixOfBuild = (~cfg as _: Config.t, build) => {
  Logs.debug(m => m("preparing prefix %s", build.plan.id));

  let%bind prefixExists = exists(build.prefixPath);
  if (!prefixExists) {
    let emitFile = ({path, content}) => {
      let%bind () = mkdir(Path.parent(path));
      let%bind () = write(~data=content, path);

      ok;
    };

    let%bind () = Result.List.iter(~f=emitFile, build.files);
    return();
  } else {
    return();
  };
};

let makePrefix = (~cfg: Config.t, plan: Plan.t) => {
  let%bind build = configureBuild(~cfg, plan);
  makePrefixOfBuild(~cfg, build);
};

let withBuild = (~commit=false, ~cfg: Config.t, plan: Plan.t, f) => {
  let%bind build = configureBuild(~cfg, plan);

  let initStoreAt = (path: Path.t) => {
    let%bind () = mkdir(Path.(path / "i"));
    let%bind () = mkdir(Path.(path / "b"));
    let%bind () = mkdir(Path.(path / "s"));
    Ok();
  };

  let%bind () = initStoreAt(cfg.storePath);
  let%bind () = initStoreAt(cfg.localStorePath);

  let perform = () => {
    let%bind () = rm(build.installPath);
    let%bind () = rm(build.stagePath);
    /* remove buildPath only if we build into a global store, otherwise we keep
     * buildPath and thus keep incremental builds */
    let%bind () =
      switch (build.plan.sourceType) {
      | Immutable => rm(build.buildPath)
      | ImmutableWithTransientDependencies
      | Transient => return()
      };
    let%bind () = mkdir(build.stagePath);
    let%bind () = mkdir(build.stagePath / "bin");
    let%bind () = mkdir(build.stagePath / "lib");
    let%bind () = mkdir(build.stagePath / "etc");
    let%bind () = mkdir(build.stagePath / "sbin");
    let%bind () = mkdir(build.stagePath / "man");
    let%bind () = mkdir(build.stagePath / "share");
    let%bind () = mkdir(build.stagePath / "toplevel");
    let%bind () = mkdir(build.stagePath / "doc");
    let%bind () = mkdir(build.stagePath / "_esy");
    let%bind () = JbuilderHack.prepare(build);
    let%bind () = mkdir(build.buildPath);
    let%bind () = mkdir(build.buildPath / "_esy");

    let%bind () =
      if (Path.compare(build.sourcePath, build.rootPath) == 0) {
        ok;
      } else {
        relocateSourcePath(build.sourcePath, build.rootPath);
      };

    let%bind () = makePrefixOfBuild(~cfg, build);

    let%bind () =
      switch (withCwd(build.rootPath, ~f=() => f(build))) {
      | Ok () =>
        let%bind () =
          if (commit) {
            commitBuildToStore(cfg, build);
          } else {
            ok;
          };
        let%bind () = JbuilderHack.finalize(build);
        ok;
      | error =>
        let%bind () = JbuilderHack.finalize(build);
        error;
      };

    ok;
  };

  switch (build.plan.sourceType) {
  | Transient
  | ImmutableWithTransientDependencies => withLock(build.lockPath, perform)
  | Immutable => perform()
  };
};

let filterPathSegments = (paths: list(string)) => {
  let f = path =>
    if (String.length(path) < 1) {
      false;
    } else if (path.[0] == '/' && Sys.win32) {
      true;
          /* On Windows, we let Cygwin resolve paths like `/usr/bin`.
           * These would fail the empty check, but we still want to include them */
    } else {
      switch (empty(Path.v(path))) {
      | Ok(empty) => !empty
      | Error(_) => false /* skip dirs which we can't check */
      };
    };
  List.filter(f, paths);
};

let getEnvAndPath = build => {
  let path =
    switch (Astring.String.Map.find("PATH", build.env)) {
    | Some(path) =>
      String.split_on_char(System.Environment.sep().[0], path)
      |> filterPathSegments
    | None => []
    };

  let env =
    switch (Bos.OS.Env.var("TERM")) {
    | Some(term) => Astring.String.Map.add("TERM", term, build.env)
    | None => build.env
    };

  let env =
    switch (path) {
    | [] => env
    | v =>
      let updatedPath = String.concat(System.Environment.sep(), v);
      Astring.String.Map.add("PATH", updatedPath, env);
    };

  (env, path);
};

let runCommand = (build, cmd) => {
  let (env, path) = getEnvAndPath(build);
  let%bind ((), (_runInfo, runStatus)) = {
    let%bind cmd = EsyLib.Cmd.ofBosCmd(cmd);
    let%bind cmd = EsyLib.Cmd.resolveInvocation(path, cmd);
    let cmd = EsyLib.Cmd.toBosCmd(cmd);
    let%bind exec = Sandbox.exec(~env, build.sandbox, cmd);
    Bos.OS.Cmd.(in_null |> exec(~err=Bos.OS.Cmd.err_run_out) |> out_stdout);
  };
  switch (runStatus) {
  | `Exited(0) => Ok()
  | status => Error(`CommandError((cmd, status)))
  };
};

let runCommandInteractive = (build, cmd) => {
  let (env, path) = getEnvAndPath(build);
  let%bind ((), (_runInfo, runStatus)) = {
    let%bind cmd = EsyLib.Cmd.ofBosCmd(cmd);
    let%bind cmd = EsyLib.Cmd.resolveInvocation(path, cmd);
    let cmd = EsyLib.Cmd.toBosCmd(cmd);
    let%bind exec = Sandbox.exec(~env, build.sandbox, cmd);
    Bos.OS.Cmd.(in_stdin |> exec(~err=Bos.OS.Cmd.err_stderr) |> out_stdout);
  };
  switch (runStatus) {
  | `Exited(0) => Ok()
  | status => Error(`CommandError((cmd, status)))
  };
};

let build = (~buildOnly=true, ~cfg: Config.t, plan: Plan.t) => {
  let%bind build = configureBuild(~cfg, plan);
  Logs.debug(m => m("start %s", build.plan.id));
  Logs.debug(m => m("building"));
  Logs.app(m =>
    m(
      "# esy-build-package: building: %s@%s",
      build.plan.name,
      build.plan.version,
    )
  );
  Logs.app(m => m("# esy-build-package: pwd: %a", Fpath.pp, build.rootPath));

  let runBuildAndInstall = (build: build) => {
    let enableLinkingOptimization =
      switch (build.plan.sourceType) {
      | Transient => true
      | ImmutableWithTransientDependencies => true
      | Immutable => false
      };
    let runEsyInstaller = installFilenames => {
      let findInstallFilenames = () => {
        let%bind items = Run.ls(build.rootPath);
        return(
          items
          |> List.filter(name => Path.hasExt(".install", name))
          |> List.map(name => Path.basename(name)),
        );
      };
      let findInstallFilenameByName = filenames => {
        let name = PackageName.withoutScope(build.plan.name);
        let f = filename =>
          String.compare(Path.remExtOfFilename(filename), name) == 0;
        List.find_opt(f, filenames);
      };
      switch (installFilenames) {
      /* the case where esy-installer is called implicitly, ignore the case
       * we have no *.install files */
      | None =>
        switch%bind (findInstallFilenames()) {
        | [] => ok
        | [filename] =>
          install(
            ~enableLinkingOptimization,
            ~prefixPath=build.stagePath,
            Path.(build.rootPath / filename),
          )
        | filenames =>
          switch (findInstallFilenameByName(filenames)) {
          | Some(filename) =>
            install(
              ~enableLinkingOptimization,
              ~prefixPath=build.stagePath,
              Path.(build.rootPath / filename),
            )
          | None =>
            error({|multiple *.install files found, specify "esy.install"|})
          }
        }
      /* the case where esy-installer is called explicitly with 0 args, fail
       * on all but a single *.install file found. */
      | Some([]) =>
        switch%bind (findInstallFilenames()) {
        | [] => error("no *.install files found")
        | [filename] =>
          install(
            ~enableLinkingOptimization,
            ~prefixPath=build.stagePath,
            Path.(build.rootPath / filename),
          )
        | filenames =>
          switch (findInstallFilenameByName(filenames)) {
          | Some(filename) =>
            install(
              ~enableLinkingOptimization,
              ~prefixPath=build.stagePath,
              Path.(build.rootPath / filename),
            )
          | None =>
            error({|multiple *.install files found, specify "esy.install"|})
          }
        }
      | Some(installFilenames) =>
        let f = ((), installFilename) =>
          install(
            ~enableLinkingOptimization,
            ~prefixPath=build.stagePath,
            Path.(build.rootPath /\/ Path.v(installFilename)),
          );
        EsyLib.Result.List.foldLeft(~f, ~init=(), installFilenames);
      };
    };

    let runCommand = cmd => {
      ();
      switch (Cmd.to_list(cmd)) {
      | [] => error("empty command")
      | ["esy-installer", ...installFilenames] =>
        runEsyInstaller(Some(installFilenames))
      | _ => runCommand(build, cmd)
      };
    };

    let runCommands = cmds => {
      let rec aux = cmds =>
        switch (cmds) {
        | [] => Ok()
        | [cmd, ...cmds] =>
          Logs.app(m =>
            m("# esy-build-package: running: %s", Cmd.to_string(cmd))
          );
          let%bind () = runCommand(cmd);
          aux(cmds);
        };
      aux(cmds);
    };

    let runBuild = () => runCommands(build.build);

    let runInstall = () =>
      switch (build.install) {
      | None => return()
      | Some([]) => runEsyInstaller(None)
      | Some(commands) => runCommands(commands)
      };

    switch (runBuild()) {
    | Ok () =>
      if (!buildOnly) {
        runInstall();
      } else {
        let%bind () = rm(build.installPath);
        let%bind () = rm(build.stagePath);
        ok;
      }
    | Error(msg) =>
      let%bind () = rm(build.installPath);
      let%bind () = rm(build.stagePath);
      Error(msg);
    };
  };
  withBuild(~commit=!buildOnly, ~cfg, plan, runBuildAndInstall);
};
