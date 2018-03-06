open EsyBuildPackage.Std;

module Run = EsyBuildPackage.Run;

module File = Bos.OS.File;

module Dir = Bos.OS.Dir;

type verb =
  | Normal
  | Quiet
  | Verbose;

type commonOpts = {
  buildPath: option(Fpath.t),
  prefixPath: option(Fpath.t),
  sandboxPath: option(Fpath.t),
  logLevel: option(Logs.level),
};

let setupLog = (style_renderer, level) => {
  let style_renderer = Option.orDefault(`None, style_renderer);
  Fmt_tty.setup_std_outputs(~style_renderer, ());
  Logs.set_level(level);
  Logs.set_reporter(Logs_fmt.reporter());
  level;
};

let createConfig = (copts: commonOpts) => {
  open Run;
  let {prefixPath, sandboxPath, _} = copts;
  let%bind fastreplacestringCmd = {
    let program = Sys.argv[0];
    let%bind program = realpath(v(program));
    let basedir = Fpath.parent(program);
    let resolution =
      EsyBuildPackage.NodeResolution.resolve(
        "fastreplacestring/.bin/fastreplacestring.exe",
        basedir,
      );
    switch%bind (Run.coerceFrmMsgOnly(resolution)) {
    | Some(path) => Ok(Fpath.to_string(path))
    | None => Ok("fastreplacestring.exe")
    };
  };
  EsyBuildPackage.Config.create(
    ~prefixPath,
    ~sandboxPath,
    ~fastreplacestringCmd,
    (),
  );
};

let build = (~buildOnly=false, ~force=false, copts: commonOpts) => {
  open Run;
  let {buildPath, _} = copts;
  let buildPath = Option.orDefault(v("build.json"), buildPath);
  let%bind config = createConfig(copts);
  let%bind task = EsyBuildPackage.BuildTask.ofFile(config, buildPath);
  let%bind () =
    EsyBuildPackage.Builder.build(~buildOnly, ~force, config, task);
  Ok();
};

let shell = (copts: commonOpts) => {
  open Run;
  let {buildPath, _} = copts;
  let buildPath = Option.orDefault(v("build.json"), buildPath);
  let%bind config = createConfig(copts);
  let runShell = (_run, runInteractive, ()) => {
    let%bind rcFilename =
      putTempFile(
        {|
        export PS1="[build $cur__name] % ";

        echo ""
        echo "  esy build shell for $cur__name@$cur__version package"
        echo ""
        |},
      );
    let cmd =
      Bos.Cmd.of_list([
        "bash",
        "--noprofile",
        "--rcfile",
        Fpath.to_string(rcFilename),
      ]);
    runInteractive(cmd);
  };
  let%bind task = EsyBuildPackage.BuildTask.ofFile(config, buildPath);
  let%bind () = EsyBuildPackage.Builder.withBuildEnv(config, task, runShell);
  ok;
};

let exec = (copts, command) => {
  open Run;
  let {buildPath, _} = copts;
  let buildPath = Option.orDefault(v("build.json"), buildPath);
  let%bind config = createConfig(copts);
  let runCommand = (_run, runInteractive, ()) => {
    let cmd = Bos.Cmd.of_list(command);
    runInteractive(cmd);
  };
  let%bind task = EsyBuildPackage.BuildTask.ofFile(config, buildPath);
  let%bind () =
    EsyBuildPackage.Builder.withBuildEnv(config, task, runCommand);
  ok;
};

let runToCompletion = (~forceExitOnError=false, run) =>
  switch (run) {
  | Error(`Msg(msg)) => `Error((false, msg))
  | Error(`CommandError(cmd, status)) =>
    let exitCode =
      switch (status) {
      | `Exited(n) => n
      | `Signaled(n) => n
      };
    if (forceExitOnError) {
      exit(exitCode);
    } else {
      let msg =
        Printf.sprintf(
          "command failed:\n%s\nwith exit code: %d",
          Bos.Cmd.to_string(cmd),
          exitCode,
        );
      `Error((false, msg));
    };
  | _ => `Ok()
  };

let help = (_copts, man_format, cmds, topic) =>
  switch (topic) {
  | None => `Help((`Pager, None)) /* help about the program. */
  | Some(topic) =>
    let topics = ["topics", "patterns", "environment", ...cmds];
    let (conv, _) = Cmdliner.Arg.enum(List.rev_map(s => (s, s), topics));
    switch (conv(topic)) {
    | `Error(e) => `Error((false, e))
    | `Ok(t) when t == "topics" =>
      List.iter(print_endline, topics);
      `Ok();
    | `Ok(t) when List.mem(t, cmds) => `Help((man_format, Some(t)))
    | `Ok(_) =>
      let page = (
        (topic, 7, "", "", ""),
        [`S(topic), `P("Say something")],
      );
      `Ok(Cmdliner.Manpage.print(man_format, Format.std_formatter, page));
    };
  };

let () = {
  open Cmdliner;
  /* Help sections common to all commands */
  let help_secs = [
    `S(Manpage.s_common_options),
    `P("These options are common to all commands."),
    `S("MORE HELP"),
    `P("Use `$(mname) $(i,COMMAND) --help' for help on a single command."),
    `Noblank,
    `P("Use `$(mname) help patterns' for help on patch matching."),
    `Noblank,
    `P("Use `$(mname) help environment' for help on environment variables."),
    `S(Manpage.s_bugs),
    `P("Check bug reports at https://github.com/esy/esy."),
  ];
  /* Options common to all commands */
  let commonOpts = (prefixPath, sandboxPath, buildPath, logLevel) => {
    prefixPath,
    sandboxPath,
    buildPath,
    logLevel,
  };
  let path = {
    let parse = Fpath.of_string;
    let print = Fpath.pp;
    Arg.conv(~docv="PATH", (parse, print));
  };
  let commonOptsT = {
    let docs = Manpage.s_common_options;
    let prefixPath = {
      let doc = "Specifies esy prefix path.";
      let env = Arg.env_var("ESY__PREFIX", ~doc);
      Arg.(
        value
        & opt(some(path), None)
        & info(["prefix-path", "P"], ~env, ~docs, ~docv="PATH", ~doc)
      );
    };
    let sandboxPath = {
      let doc = "Specifies esy sandbox path.";
      let env = Arg.env_var("ESY__SANDBOX", ~doc);
      Arg.(
        value
        & opt(some(path), None)
        & info(["sandbox-path", "S"], ~env, ~docs, ~docv="PATH", ~doc)
      );
    };
    let buildPath = {
      let doc = "Specifies path to build task.";
      let env = Arg.env_var("ESY__BUILD_SPEC", ~doc);
      Arg.(
        value
        & opt(some(path), None)
        & info(["build", "B"], ~env, ~docs, ~docv="PATH", ~doc)
      );
    };
    let setupLogT =
      Term.(
        const(setupLog)
        $ Fmt_cli.style_renderer()
        $ Logs_cli.level(~env=Arg.env_var("ESY__LOG"), ())
      );
    Term.(
      const(commonOpts) $ prefixPath $ sandboxPath $ buildPath $ setupLogT
    );
  };
  /* Command terms */
  let default_cmd = {
    let doc = "esy package builder";
    let sdocs = Manpage.s_common_options;
    let exits = Term.default_exits;
    let man = help_secs;
    let cmd = opts => runToCompletion(build(opts));
    (
      Term.(ret(const(cmd) $ commonOptsT)),
      Term.info(
        "esy-build-package",
        ~version="v0.1.0",
        ~doc,
        ~sdocs,
        ~exits,
        ~man,
      ),
    );
  };
  let build_cmd = {
    let doc = "build package";
    let sdocs = Manpage.s_common_options;
    let exits = Term.default_exits;
    let man = help_secs;
    let cmd = (opts, buildOnly, force) =>
      runToCompletion(build(~buildOnly, ~force, opts));
    let forceT = {
      let doc = "Force build without running any staleness checks.";
      Arg.(value & flag & info(["f", "force"], ~doc));
    };
    let buildOnlyT = {
      let doc = "Only run build commands (skipping install commands).";
      Arg.(value & flag & info(["build-only"], ~doc));
    };
    (
      Term.(ret(const(cmd) $ commonOptsT $ buildOnlyT $ forceT)),
      Term.info("build", ~doc, ~sdocs, ~exits, ~man),
    );
  };
  let shell_cmd = {
    let doc = "shell into build environment";
    let sdocs = Manpage.s_common_options;
    let exits = Term.default_exits;
    let man = help_secs;
    let cmd = opts => runToCompletion(shell(opts));
    (
      Term.(ret(const(cmd) $ commonOptsT)),
      Term.info("shell", ~doc, ~sdocs, ~exits, ~man),
    );
  };
  let exec_cmd = {
    let doc = "execute command inside build environment";
    let sdocs = Manpage.s_common_options;
    let exits = Term.default_exits;
    let man = help_secs;
    let command_t =
      Arg.(non_empty & pos_all(string, []) & info([], ~docv="COMMAND"));
    let cmd = (opts, command) =>
      runToCompletion(~forceExitOnError=true, exec(opts, command));
    (
      Term.(ret(const(cmd) $ commonOptsT $ command_t)),
      Term.info("exec", ~doc, ~sdocs, ~exits, ~man),
    );
  };
  let help_cmd = {
    let topic = {
      let doc = "The topic to get help on. `topics' lists the topics.";
      Arg.(
        value & pos(0, some(string), None) & info([], ~docv="TOPIC", ~doc)
      );
    };
    let doc = "display help about esy-build-package and its commands";
    let man = [
      `S(Manpage.s_description),
      `P(
        "Prints help about esy-build-package commands and other subjects...",
      ),
      `Blocks(help_secs),
    ];
    (
      Term.(
        ret(
          const(help)
          $ commonOptsT
          $ Arg.man_format
          $ Term.choice_names
          $ topic,
        )
      ),
      Term.info("help", ~doc, ~exits=Term.default_exits, ~man),
    );
  };
  let cmds = [build_cmd, shell_cmd, exec_cmd, help_cmd];
  Term.(exit @@ eval_choice(default_cmd, cmds));
};
