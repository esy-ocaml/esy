module Package = EsyInstall.Package;
module SandboxPath: (module type of EsyBuildPackage.Config.Path);
module SandboxValue: (module type of EsyBuildPackage.Config.Value);

type t =
  | Host(config)
and config = {
  path: SandboxValue.t,
  destdir: SandboxValue.t,
  stdlib: SandboxValue.t,
  ldconf: SandboxValue.t,
  commands: list((string, SandboxValue.t)),
};

let isCompiler: Package.t => bool;

let commands: SandboxPath.t => list((string, SandboxValue.t));

let path: SandboxPath.t => SandboxPath.t;

let renderConfig: (~prefix: SandboxPath.t, t) => EsyBuildPackage.Plan.file;
