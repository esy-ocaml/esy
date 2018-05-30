module Path = EsyLib.Path;
module Config = Shared.Config;

module Api = {
  let (/+) = Filename.concat;

  let solve = (config: Config.t) => {
    let json =
      Yojson.Basic.from_file(
        Path.(config.basePath / "package.json" |> to_string),
      );
    let env = Solve.solve(config, `PackageJson(json));
    let json = Shared.Env.to_yojson(Shared.Types.Source.to_yojson, env);
    let chan =
      open_out(Path.(config.basePath / "esyi.lock.json" |> to_string));
    Yojson.Safe.pretty_to_channel(chan, json);
    close_out(chan);
  };

  let fetch = (config: Config.t) => {
    let json =
      Yojson.Safe.from_file(
        Path.(config.basePath / "esyi.lock.json" |> to_string),
      );
    let env =
      switch (Shared.Env.of_yojson(Shared.Types.Source.of_yojson, json)) {
      | Error(_a) => failwith("Bad lockfile")
      | Ok(a) => a
      };
    Shared.Files.removeDeep(
      Path.(config.basePath / "node_modules" |> to_string),
    );
    Fetch.fetch(config, env);
  };
};

module CommandLineInterface = {
  open Cmdliner;

  let exits = Term.default_exits;
  let docs = Manpage.s_common_options;
  let sdocs = Manpage.s_common_options;

  let cwd = Path.v(Sys.getcwd());
  let version = "0.1.0";

  let pathConv = {
    let parse = Path.of_string;
    let print = Path.pp;
    Arg.conv(~docv="PATH", (parse, print));
  };

  let sandboxPathArg = {
    let doc = "Specifies esy sandbox path.";
    let env = Arg.env_var("ESYI__SANDBOX", ~doc);
    Arg.(
      value
      & opt(some(pathConv), None)
      & info(["sandbox-path", "S"], ~env, ~docs, ~doc)
    );
  };

  let cachePathArg = {
    let doc = "Specifies cache directory..";
    let env = Arg.env_var("ESYI__CACHE", ~doc);
    Arg.(
      value
      & opt(some(pathConv), None)
      & info(["cache-path"], ~env, ~docs, ~doc)
    );
  };

  let npmRegistryArg = {
    let doc = "Specifies npm registry to use.";
    let env = Arg.env_var("NPM_CONFIG_REGISTRY", ~doc);
    Arg.(
      value
      & opt(some(string), None)
      & info(["npm-registry"], ~env, ~docs, ~doc)
    );
  };

  let cfgTerm = {
    let parse = (cachePath, sandboxPath, npmRegistry) => {
      let sandboxPath =
        switch (sandboxPath) {
        | Some(sandboxPath) => sandboxPath
        | None => cwd
        };
      Shared.Config.make(~cachePath?, ~npmRegistry?, sandboxPath);
    };
    Term.(const(parse) $ cachePathArg $ sandboxPathArg $ npmRegistryArg);
  };

  let defaultCommand = {
    let doc = "Dependency installer";
    let info = Term.info("esyi", ~version, ~doc, ~sdocs, ~exits);
    let cmd = cfg => {
      Api.solve(cfg);
      Api.fetch(cfg);
      `Ok();
    };
    (Term.(ret(const(cmd) $ cfgTerm)), info);
  };

  let installCommand = {
    let doc = "Solve & fetch dependencies";
    let info = Term.info("install", ~version, ~doc, ~sdocs, ~exits);
    let cmd = cfg => {
      Api.solve(cfg);
      Api.fetch(cfg);
      `Ok();
    };
    (Term.(ret(const(cmd) $ cfgTerm)), info);
  };

  let solveCommand = {
    let doc = "Solve dependencies and store the solution as a lockfile";
    let info = Term.info("solve", ~version, ~doc, ~sdocs, ~exits);
    let cmd = cfg => {
      Api.solve(cfg);
      `Ok();
    };
    (Term.(ret(const(cmd) $ cfgTerm)), info);
  };

  let fetchCommand = {
    let doc = "Fetch dependencies using the solution in a lockfile";
    let info = Term.info("fetch", ~version, ~doc, ~sdocs, ~exits);
    let cmd = cfg => {
      Api.fetch(cfg);
      `Ok();
    };
    (Term.(ret(const(cmd) $ cfgTerm)), info);
  };

  let commands = [installCommand, solveCommand, fetchCommand];

  let run = () => {
    Printexc.record_backtrace(true);
    Term.(exit(eval_choice(~argv=Sys.argv, defaultCommand, commands)));
  };
};

let () = CommandLineInterface.run();
