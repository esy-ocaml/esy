/**
 * @flow
 */

import type {Config, Sandbox, BuildSpec} from '../types';
import invariant from 'invariant';
import semver from 'semver';

import * as EsyOpam from '@esy-ocaml/esy-opam';
import {LOCKFILE_FILENAME} from '@esy-ocaml/esy-install/src/constants';
import PackageResolver from '@esy-ocaml/esy-install/src/package-resolver';
import Lockfile from '@esy-ocaml/esy-install/src/lockfile';
import {stringify as lockStringify} from '@esy-ocaml/esy-install/src/lockfile';
import YarnConfig from '@esy-ocaml/esy-install/src/config';
import * as fetcher from '@esy-ocaml/esy-install/src/package-fetcher';
import type {Manifest} from '@esy-ocaml/esy-install/src/types';

import * as fs from '../lib/fs';
import * as path from '../lib/path';
import * as M from '../package-manifest';
import * as Crawl from './crawl';

async function createResolver(config, sandboxPath, requests: Array<string>) {
  const yarnRequests = requests.map(pattern => ({
    pattern,
    registry: 'npm',
    optional: false,
  }));

  const lockfile = await Lockfile.fromDirectory(sandboxPath);

  const yarnConfig = new YarnConfig(config.reporter);
  await yarnConfig.init();

  const packageResolver = new PackageResolver(yarnConfig, lockfile);
  await packageResolver.init(yarnRequests);

  // write lockfile
  const lockfileObject = lockfile.getLockfile(packageResolver.patterns);
  const lockfileFilename = path.join(sandboxPath, LOCKFILE_FILENAME);
  const lockSource = lockStringify(lockfileObject, false, true);
  await fs.writeFile(lockfileFilename, lockSource);

  lockfile.cache = lockfileObject;

  const manifests: Array<Manifest> = await fetcher.fetch(
    packageResolver.getManifests(),
    yarnConfig,
  );

  const manifestLocByResolution: Map<string, Manifest> = new Map();
  const manifestByName: Map<string, Map<string, Manifest>> = new Map();

  for (const manifest of manifests) {
    if (manifest._remote != null && manifest._remote.resolved != null) {
      manifestLocByResolution.set(manifest._remote.resolved, manifest);
      const manifestByVersion = manifestByName.get(manifest.name);
      if (manifestByVersion == null) {
        manifestByName.set(manifest.name, new Map([[manifest.version, manifest]]));
      } else {
        manifestByVersion.set(manifest.version, manifest);
      }
    }
  }

  const resolveCacheLocation = async dep => {
    if (dep.type === 'peer') {
      // peer dep resolutions aren't stored in a lockfile so we resolve them
      // against installed packages here
      const versionMap = manifestByName.get(dep.name);
      if (versionMap == null) {
        return null;
      }
      const versions = Array.from(versionMap.keys());
      versions.sort((a, b) => -1 * EsyOpam.versionCompare(a, b));
      for (const v of versions) {
        if (semver.satisfies(v, dep.spec)) {
          const manifest = versionMap.get(v);
          if (
            manifest != null &&
            manifest._loc != null &&
            manifest._remote != null &&
            manifest._remote.resolved != null
          ) {
            return {
              resolved: manifest._remote.resolved,
              sourcePath: path.dirname(manifest._loc),
            };
          }
        }
      }
      return null;
    } else {
      const lockedManifest = lockfile.getLocked(dep.pattern);
      if (lockedManifest == null || lockedManifest.resolved == null) {
        return null;
      }
      const manifest = manifestLocByResolution.get(lockedManifest.resolved);
      if (
        manifest != null &&
        manifest._loc != null &&
        manifest._remote != null &&
        manifest._remote.resolved != null
      ) {
        return {
          resolved: manifest._remote.resolved,
          sourcePath: path.dirname(manifest._loc),
        };
      } else {
        return null;
      }
    }
  };

  const resolver = async dep => {
    const res = await resolveCacheLocation(dep);
    if (res == null) {
      return null;
    } else {
      const {sourcePath, resolved} = res;
      const {manifest} = await M.read(sourcePath);
      // This is what esy-install do. We probably need to consolidate this in a
      // single place.
      manifest._resolved = resolved;
      return {manifest, sourcePath};
    }
  };

  return resolver;
}

export async function create(
  request: Array<string>,
  config: Config<*>,
): Promise<Sandbox> {
  const sandboxPath = config.getSandboxPath(request);
  await fs.mkdirp(sandboxPath);
  const resolve = await createResolver(config, sandboxPath, request);
  const env = Crawl.getDefaultEnvironment();

  const resolutionCache = new Map();

  function resolveManifestCached(spec, baseDir) {
    let resolution = resolutionCache.get(spec.pattern);
    if (resolution == null) {
      resolution = resolve(spec);
      resolutionCache.set(spec.pattern, resolution);
    }
    return resolution;
  }

  const buildCache: Map<string, Promise<BuildSpec>> = new Map();
  function crawlBuildCached(context: Crawl.SandboxCrawlContext): Promise<BuildSpec> {
    const key = context.sourcePath;
    let build = buildCache.get(key);
    if (build == null) {
      build = Crawl.crawlBuild(context);
      buildCache.set(key, build);
    }
    return build;
  }

  const manifest = M.normalizeManifest({
    name: '__sandbox__',
    version: '0.0.0',
  });

  const crawlContext: Crawl.SandboxCrawlContext = {
    manifest,
    sourcePath: sandboxPath,

    env,
    sandboxPath,
    resolveManifest: resolveManifestCached,
    crawlBuild: crawlBuildCached,
    dependencyTrace: [],
    options: {forRelease: true},
  };

  const dependenciesReqs = request.map(pattern => {
    const {name, spec} = Crawl.parseDependencyPattern(pattern);
    return {type: 'regular', name, spec, pattern};
  });
  const {dependencies} = await Crawl.crawlDependencies(dependenciesReqs, crawlContext);
  const root: BuildSpec = {
    id: '__sandbox__',
    name: '__sandbox__',
    version: '0.0.0',
    buildCommand: [],
    installCommand: [],
    exportedEnv: {},
    sourcePath: '',
    sourceType: 'root',
    buildType: 'out-of-source',
    shouldBePersisted: false,
    dependencies,
    // TODO:
    errors: [],
  };

  return {env, root, devDependencies: new Map()};
}
