/**
 * @flow
 */

import type {Reporter, BuildSpec, Config, BuildPlatform, StoreTree, Store} from './types';
import * as os from 'os';
import invariant from 'invariant';
import {STORE_BUILD_TREE, STORE_INSTALL_TREE, STORE_STAGE_TREE} from './constants';
import * as S from './store';
import * as path from './lib/path';
import * as crypto from './lib/crypto';
import {sync as resolve} from 'resolve';

const isTestEnvironment = process.env.NODE_ENV === 'test';

// Pretent there's a single CPU in test environment
const NUM_CPUS = isTestEnvironment ? 1 : os.cpus().length;

export const FASTREPLACESTRING_COMMAND = resolve(
  'fastreplacestring/.bin/fastreplacestring.exe',
  {basedir: __dirname},
);

function _create<Path: path.Path>(
  sandboxPath: Path,
  store: Store<Path>,
  localStore: Store<Path>,
  importPaths: Array<path.AbsolutePath>,
  buildPlatform,
  reporter: Reporter,
): Config<Path> {
  const genStorePath = (tree: StoreTree, build: BuildSpec, segments: string[]) => {
    const path: Path =
      build.sourceType === 'immutable'
        ? store.getPath(tree, build, ...segments)
        : localStore.getPath(tree, build, ...segments);
    return path;
  };

  function getSourcePath(build: BuildSpec, ...segments): Path {
    const {sourcePath} = build;
    if (sourcePath.charAt(0) === '/') {
      return (path.join(sourcePath, ...segments): any);
    } else {
      return (path.join(sandboxPath, sourcePath, ...segments): any);
    }
  }

  const buildConfig: Config<Path> = {
    reporter,
    sandboxPath,
    store,
    localStore,
    buildPlatform,
    importPaths,
    buildConcurrency: NUM_CPUS,

    getSourcePath,

    getRootPath: (build: BuildSpec, ...segments) => {
      if (build.buildType === 'in-source') {
        return genStorePath(STORE_BUILD_TREE, build, segments);
      } else if (build.buildType === '_build') {
        if (build.sourceType === 'immutable') {
          return genStorePath(STORE_BUILD_TREE, build, segments);
        } else if (build.sourceType === 'transient') {
          return getSourcePath(build, ...segments);
        } else if (build.sourceType === 'root') {
          return getSourcePath(build, ...segments);
        }
      } else if (build.buildType === 'out-of-source') {
        return getSourcePath(build, ...segments);
      }
      invariant(false, 'Impossible happened');
    },
    getBuildPath: (build: BuildSpec, ...segments) =>
      genStorePath(STORE_BUILD_TREE, build, segments),
    getLogPath: (build: BuildSpec) => {
      const buildPath = genStorePath(STORE_BUILD_TREE, build, []);
      return path.join(path.dirname(buildPath), `${build.id}.log`);
    },
    getStagePath: (build: BuildSpec, ...segments) =>
      genStorePath(STORE_STAGE_TREE, build, segments),
    getInstallPath: (build: BuildSpec, ...segments) =>
      genStorePath(STORE_INSTALL_TREE, build, segments),

    prettifyPath: (p: string) => {
      if (p.indexOf(store.path) === 0) {
        const relative = p.slice(store.path.length);
        return path.join(store.prettyPath, relative);
      } else {
        return p;
      }
    },

    getSandboxPath: (requests: Array<string>) => {
      // TODO: normalize requests?
      requests = requests.slice(0);
      requests.sort();
      const id = crypto.hash(requests.join(' '));
      // TODO: how to get prefix? is it availble in any config?
      const prefixPath = path.dirname(store.path);
      return path.join(prefixPath, 'sandbox', id);
    },
  };
  return buildConfig;
}

export function create(params: {
  reporter: Reporter,
  storePath: string,
  sandboxPath: string,
  buildPlatform: BuildPlatform,
  importPaths?: Array<string>,
}): Config<path.AbstractPath> {
  const {reporter, storePath, sandboxPath, buildPlatform, importPaths = []} = params;
  const store = S.forAbstractPath(storePath);
  const localStore = S.forAbstractPath(
    path.join(sandboxPath, 'node_modules', '.cache', '_esy', 'store'),
  );
  return _create(
    path.abstract(sandboxPath),
    store,
    localStore,
    importPaths.map(path.absolute),
    buildPlatform,
    reporter,
  );
}

export function createForPrefix(params: {
  reporter: Reporter,
  prefixPath: string,
  sandboxPath: string,
  buildPlatform: BuildPlatform,
  importPaths?: Array<string>,
}): Config<path.AbsolutePath> {
  const {reporter, prefixPath, sandboxPath, buildPlatform, importPaths = []} = params;
  const store = S.forPrefixPath(prefixPath);
  const localStore = S.forAbsolutePath(
    path.join(sandboxPath, 'node_modules', '.cache', '_esy', 'store'),
  );
  return _create(
    path.absolute(sandboxPath),
    store,
    localStore,
    importPaths.map(path.absolute),
    buildPlatform,
    reporter,
  );
}
