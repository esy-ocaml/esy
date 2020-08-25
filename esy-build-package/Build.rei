/*

   This module provides functions to build tasks and interact with build task
   environment. This implements the core of esy-build-package command.

 */

type t =
  pri {
    plan: Plan.t,
    storePath: EsyLib.Path.t,
    sourcePath: EsyLib.Path.t,
    rootPath: EsyLib.Path.t,
    installPath: EsyLib.Path.t,
    stagePath: EsyLib.Path.t,
    buildPath: EsyLib.Path.t,
    prefixPath: EsyLib.Path.t,
    lockPath: EsyLib.Path.t,
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

/**

  Create a per-package prefix directory for storing build configs, ld.conf, etc.
  */
let makePrefix: (~cfg: Config.t, Plan.t) => Run.t(unit, 'b);

/**

  Build task.

  if [buildOnly] (set to true by default) is set to true then only build
  commands of a task are executed, other install commands are executed too.

 */
let build: (~buildOnly: bool=?, ~cfg: Config.t, Plan.t) => Run.t(unit, 'b);

/**

  Run computation with initialized build env.

  Current working dir is changed to a task's build path.

 */
let withBuild:
  (~commit: bool=?, ~cfg: Config.t, Plan.t, t => Run.t(unit, 'b)) =>
  Run.t(unit, 'b);

/**

	Run command in the build environment.

 */
let runCommand: (t, Cmd.t) => Run.t(unit, _);

/**

	Run command interactively in the build environment.

 */
let runCommandInteractive: (t, Cmd.t) => Run.t(unit, _);
