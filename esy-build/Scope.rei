open EsyPackageConfig;

module SandboxPath: (module type of EsyBuildPackage.Config.Path);
module SandboxValue: (module type of EsyBuildPackage.Config.Value);
module SandboxEnvironment: (module type of EsyBuildPackage.Config.Environment);

type t;

/** An initial scope for the package. */

let make:
  (
    ~platform: System.Platform.t,
    ~sandboxEnv: SandboxEnvironment.Bindings.t,
    ~id: BuildId.t,
    ~name: string,
    ~version: Version.t,
    ~mode: BuildSpec.mode,
    ~depspec: EsyInstall.Solution.DepSpec.t,
    ~sourceType: SourceType.t,
    ~sourcePath: SandboxPath.t,
    ~globalPathVariable: option(string),
    EsyInstall.Package.t,
    BuildManifest.t
  ) =>
  t;

/** Add new pkg */

let add: (~direct: bool, ~dep: t, t) => t;

let pkg: t => EsyInstall.Package.t;
let id: t => BuildId.t;
let mode: t => BuildSpec.mode;
let depspec: t => EsyInstall.Solution.DepSpec.t;
let name: t => string;
let version: t => Version.t;
let sourceType: t => SourceType.t;
let buildType: t => BuildType.t;
let storePath: t => SandboxPath.t;
let rootPath: t => SandboxPath.t;
let sourcePath: t => SandboxPath.t;
let buildPath: t => SandboxPath.t;
let buildInfoPath: t => SandboxPath.t;
let stagePath: t => SandboxPath.t;
let installPath: t => SandboxPath.t;
let prefixPath: t => SandboxPath.t;
let logPath: t => SandboxPath.t;

let pp: Fmt.t(t);

let env:
  (~includeBuildEnv: bool, ~buildIsInProgress: bool, t) =>
  Run.t(SandboxEnvironment.Bindings.t);

let render:
  (
    ~env: SandboxEnvironment.t=?,
    ~environmentVariableName: string=?,
    ~buildIsInProgress: bool,
    t,
    string
  ) =>
  Run.t(SandboxValue.t);

let toOpamEnv: (~buildIsInProgress: bool, t) => OpamFilter.env;

let exposeUserEnvWith:
  (
    (string, SandboxValue.t) => Environment.Binding.t(SandboxValue.t),
    string,
    t
  ) =>
  t;

let findlibConf: t => list(FindlibConf.t);
