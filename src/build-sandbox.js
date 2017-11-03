/**
 * @flow
 */

import type {
  BuildSpec,
  BuildSandbox,
  BuildEnvironment,
  EnvironmentVarExport,
  EsySpec,
} from './types';

import * as JSON5 from 'json5';
import * as path from 'path';
import invariant from 'invariant';
import outdent from 'outdent';

import * as fs from './lib/fs';
import {computeHash, resolve, normalizePackageName} from './util';
import * as Env from './environment';

export type PackageJson = {
  name: string,
  version: string,
  dependencies?: PackageJsonVersionSpec,
  peerDependencies?: PackageJsonVersionSpec,
  devDependencies?: PackageJsonVersionSpec,
  optionalDependencies?: PackageJsonVersionSpec,

  // This is specific to npm, make sure we get rid of that if we want to port to
  // other package installers.
  //
  // npm puts a resolved name there, for example for packages installed from
  // github — it would be a URL to git repo and a sha1 hash of the tree.
  _resolved?: string,

  esy: EsySpec,
};

export type PackageJsonVersionSpec = {
  [name: string]: string,
};

type SandboxCrawlContext = {
  env: BuildEnvironment,
  sandboxPath: string,
  dependencyTrace: Array<string>,
  crawlBuild: (sourcePath: string, context: SandboxCrawlContext) => Promise<BuildSpec>,
  resolve: (moduleName: string, baseDirectory: string) => Promise<string>,
  options: Options,
};

export type Options = {
  forRelease?: boolean,
};

export async function fromDirectory(
  sandboxPath: string,
  options: Options = {},
): Promise<BuildSandbox> {
  // Caching module resolution actually speed ups sandbox crawling a lot.
  const resolutionCache: Map<string, Promise<string>> = new Map();

  function resolveCached(packageName, baseDir): Promise<string> {
    const key = `${baseDir}__${packageName}`;
    let resolution = resolutionCache.get(key);
    if (resolution == null) {
      resolution = resolve(packageName, baseDir);
      resolutionCache.set(key, resolution);
    }
    return resolution;
  }

  const buildCache: Map<string, Promise<BuildSpec>> = new Map();

  function crawlBuildCached(sourcePath, context): Promise<BuildSpec> {
    let build = buildCache.get(sourcePath);
    if (build == null) {
      build = crawlBuild(sourcePath, context);
      buildCache.set(sourcePath, build);
    }
    return build;
  }

  const env = getEnvironment();

  const crawlContext = {
    env,
    sandboxPath,
    resolve: resolveCached,
    crawlBuild: crawlBuildCached,
    dependencyTrace: [],
    options,
  };

  const root = await crawlBuild(sandboxPath, crawlContext);

  return {env, root};
}

async function crawlDependencies(
  baseDir: string,
  dependencySpecs: string[],
  context: SandboxCrawlContext,
): Promise<{dependencies: Map<string, BuildSpec>, errors: Array<{message: string}>}> {
  const dependencies = new Map();
  const errors = [];
  const missingPackages = [];

  for (const spec of dependencySpecs) {
    const {name} = parseDependencySpec(spec);

    if (context.dependencyTrace.indexOf(name) > -1) {
      errors.push({
        message: formatCircularDependenciesError(name, context),
      });
      continue;
    }

    let dependencyPackageJsonPath = '/does/not/exists';
    try {
      dependencyPackageJsonPath = await context.resolve(`${name}/package.json`, baseDir);
    } catch (_err) {
      missingPackages.push(name);
      continue;
    }

    const build = await context.crawlBuild(
      path.dirname(dependencyPackageJsonPath),
      context,
    );

    errors.push(...build.errors);
    dependencies.set(build.id, build);
  }

  if (missingPackages.length > 0) {
    errors.push({
      message: formatMissingPackagesError(missingPackages, context),
    });
  }

  return {dependencies, errors};
}

async function crawlBuild(
  sourcePath: string,
  context: SandboxCrawlContext,
): Promise<BuildSpec> {
  const packageJson = await readManifest(sourcePath);
  const isRootBuild = context.sandboxPath === sourcePath;

  let buildCommand = normalizeCommand(packageJson.esy.build);
  let installCommand = normalizeCommand(packageJson.esy.install);

  const dependencySpecs = objectToDependencySpecs(
    packageJson.dependencies,
    packageJson.peerDependencies,
  );
  const {dependencies, errors} = await crawlDependencies(sourcePath, dependencySpecs, {
    ...context,
    dependencyTrace: context.dependencyTrace.concat(packageJson.name),
  });

  const nextErrors = [...errors];
  const isInstalled = packageJson._resolved != null;
  const realSourcePath = await fs.realpath(sourcePath);
  const source = packageJson._resolved || `local:${realSourcePath}`;
  const nextSourcePath = path.relative(context.sandboxPath, sourcePath);
  const id = calculateBuildId(context.env, packageJson, source, dependencies);

  const spec: BuildSpec = {
    id,
    name: packageJson.name,
    version: packageJson.version,
    exportedEnv: packageJson.esy.exportedEnv,
    buildCommand,
    installCommand,
    shouldBePersisted:
      !(isRootBuild || !isInstalled) || Boolean(context.options.forRelease),
    sourceType: isRootBuild ? 'root' : !isInstalled ? 'transient' : 'immutable',
    buildType:
      packageJson.esy.buildsInSource === '_build'
        ? '_build'
        : Boolean(packageJson.esy.buildsInSource) ? 'in-source' : 'out-of-source',
    sourcePath: nextSourcePath,
    packageJson,
    dependencies,
    errors: nextErrors,
  };

  return spec;
}

function getEnvironment(): BuildEnvironment {
  return Env.fromEntries([
    {
      name: 'PATH',
      value: '$PATH:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin',
      exclusive: false,
      builtIn: true,
      exported: true,
    },
    {
      name: 'SHELL',
      value: 'env -i /bin/bash --norc --noprofile',
      exclusive: false,
      builtIn: true,
      exported: true,
    },
  ]);
}

export async function readManifest(packagePath: string): Promise<PackageJson> {
  const manifestNames = ['esy.json', 'package.json'];

  for (const manifestName of manifestNames) {
    const manifestPath = path.join(packagePath, manifestName);
    if (!await fs.exists(manifestPath)) {
      continue;
    }

    const parse = manifestName === 'esy.json' ? JSON5.parse : JSON.parse;
    const packageJson = await fs.readJson(manifestPath, parse);
    if (packageJson.esy == null) {
      packageJson.esy = {};
    }
    if (packageJson.esy.build == null) {
      packageJson.esy.build = null;
    }
    if (packageJson.esy.install == null) {
      packageJson.esy.install = null;
    }
    if (packageJson.esy.exportedEnv == null) {
      packageJson.esy.exportedEnv = {};
    }
    if (packageJson.esy.buildsInSource == null) {
      packageJson.esy.buildsInSource = false;
    }
    return packageJson;
  }

  invariant(
    false,
    'Unable to find manifest in %s: tried %s',
    packagePath,
    manifestNames.join(', '),
  );
}

function calculateBuildId(
  env: BuildEnvironment,
  packageJson: PackageJson,
  source: string,
  dependencies: Map<string, BuildSpec>,
): string {
  const {name, version, esy} = packageJson;
  const h = hash({
    env,
    source,
    packageJson: {
      name,
      version,
      esy,
    },
    dependencies: Array.from(dependencies.values(), dep => dep.id),
  });
  if (process.env.NODE_ENV === 'test') {
    return `${normalizePackageName(name)}-${version || '0.0.0'}`;
  } else {
    return `${normalizePackageName(name)}-${version || '0.0.0'}-${h.slice(0, 8)}`;
  }
}

function hash(value: mixed) {
  if (typeof value === 'object') {
    if (value === null) {
      return hash('null');
    } else if (!Array.isArray(value)) {
      const v = value;
      const keys = Object.keys(v);
      keys.sort();
      return hash(keys.map(k => [k, v[k]]));
    } else {
      return hash(JSON.stringify(value.map(hash)));
    }
  } else if (value === undefined) {
    return hash('undefined');
  } else {
    return computeHash(JSON.stringify(value));
  }
}

function parseDependencySpec(spec: string): {name: string, versionSpec: string} {
  if (spec.startsWith('@')) {
    const [_, name, versionSpec] = spec.split('@', 3);
    return {name: '@' + name, versionSpec};
  } else {
    const [name, versionSpec] = spec.split('@');
    return {name, versionSpec};
  }
}

function objectToDependencySpecs(...objs) {
  const dependencySpecList = [];
  for (const obj of objs) {
    if (obj == null) {
      continue;
    }
    for (const name in obj) {
      const versionSpec = obj[name];
      const dependencySpec = `${name}@${versionSpec}`;
      if (dependencySpecList.indexOf(dependencySpec) === -1) {
        dependencySpecList.push(dependencySpec);
      }
    }
  }
  return dependencySpecList;
}

function formatCircularDependenciesError(dependency, context) {
  return outdent`
    Circular dependency "${dependency}" found
      At ${context.dependencyTrace.join(' -> ')}
  `;
}

function formatMissingPackagesError(missingPackages, context) {
  const packagesToReport = missingPackages.slice(0, 3);
  const packagesMessage = packagesToReport.map(p => `"${p}"`).join(', ');
  const extraPackagesMessage =
    missingPackages.length > packagesToReport.length
      ? ` (and ${missingPackages.length - packagesToReport.length} more)`
      : '';
  return outdent`
    Cannot resolve ${packagesMessage}${extraPackagesMessage} packages
      At ${context.dependencyTrace.join(' -> ')}
      Did you forget to run "esy install" command?
  `;
}

function normalizeCommand(
  command: null | string | Array<string | Array<string>>,
): Array<string | Array<string>> {
  if (command == null) {
    return [];
  } else if (!Array.isArray(command)) {
    return [command];
  } else {
    return command;
  }
}
