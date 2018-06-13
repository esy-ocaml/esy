module PackageName: {
  type t;

  let toNpm: t => string;
  let ofNpm: string => Run.t(t);
  let ofNpmExn: string => t;

  let toString: t => string;
  let ofString: string => t;

  let compare: (t, t) => int;
  let equal: (t, t) => bool;
}

type manifest = {
  name: PackageName.t,
  version: OpamVersion.Version.t,
  fileName: string,
  build: list(list(string)),
  install: list(list(string)),
  patches: list(string), /* these should be absolute */
  files: list((Path.t, string)), /* relname, sourcetext */
  dependencies: PackageInfo.Dependencies.t,
  buildDependencies: PackageInfo.Dependencies.t,
  devDependencies: PackageInfo.Dependencies.t,
  peerDependencies: PackageInfo.Dependencies.t,
  optDependencies: PackageInfo.Dependencies.t,
  available: bool,
  /* TODO optDependencies (depopts) */
  source: PackageInfo.Source.t,
  exportedEnv: PackageJson.ExportedEnv.t,
};

module ThinManifest : {
  type t = {
    name: PackageName.t,
    opamFile: Path.t,
    urlFile: Path.t,
    version: OpamVersion.Version.t,
  };
};

let name : manifest => string;
let version : manifest => OpamVersion.Version.t;
let source : manifest => PackageInfo.Source.t;
let dependencies : manifest => PackageInfo.DependenciesInfo.t;

let parseManifest : ((PackageName.t, OpamVersion.Version.t), OpamParserTypes.opamfile) => manifest;
let parseUrlFile : OpamParserTypes.opamfile => PackageInfo.SourceSpec.t;

let toPackageJson : (manifest, PackageInfo.Version.t) => PackageInfo.OpamInfo.t;
