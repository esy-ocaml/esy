/**
 * @flow
 */

import type {
  BuildSandbox,
  BuildSpec,
  Config,
  BuildTask,
  BuildEnvironment,
  BuildPlatform,
} from './types';

import {substituteVariables} from 'var-expansion';

import {doubleQuote} from './lib/shell';
import * as path from './lib/path';
import {normalizePackageName, mergeIntoMap, mapValuesMap} from './util';
import * as Graph from './graph';
import * as Env from './environment';

type BuildTaskParams = {
  env?: BuildEnvironment,
};

/**
 * Produce a task graph from a build spec graph.
 */
export function fromBuildSpec(
  rootBuild: BuildSpec,
  config: Config<path.Path>,
  params?: BuildTaskParams = {},
): BuildTask {
  const {task} = Graph.topologicalFold(
    rootBuild,
    (dependencies, allDependencies, spec) => {
      const scopes = computeScopes(dependencies, allDependencies, spec);
      const task = createTask(scopes);
      return {spec, scopes, task};
    },
  );

  function computeScopes(dependencies, allDependencies, spec) {
    // scope which is used to eval exported variables
    const evalScope = getEvalScope(spec, dependencies, config);
    // global env vars exported from a spec
    const globalScope = new Map();
    // local env vars exported from a spec
    const localScope = new Map();
    for (const name in spec.exportedEnv) {
      const envConfig = spec.exportedEnv[name];
      const value = renderWithScope(envConfig.val, evalScope).rendered;
      const item = {
        name,
        value,
        spec,
        builtIn: false,
        exported: true,
        exclusive: Boolean(envConfig.exclusive),
      };
      if (envConfig.scope === 'global') {
        globalScope.set(name, item);
      } else {
        localScope.set(name, item);
      }
    }
    const scopes = {
      spec,
      localScope,
      globalScope,
      dependencies,
      allDependencies,
    };
    return scopes;
  }

  function createTask(scopes): BuildTask {
    const env = new Map();
    const ocamlfindDest = config.getInstallPath(scopes.spec, 'lib');

    const OCAMLPATH = [];
    const PATH = [];
    const MAN_PATH = [];

    for (const dep of scopes.allDependencies.values()) {
      OCAMLPATH.push(config.getFinalInstallPath(dep.spec, 'lib'));
      PATH.push(config.getFinalInstallPath(dep.spec, 'bin'));
      MAN_PATH.push(config.getFinalInstallPath(dep.spec, 'man'));
    }

    // In ideal world we wouldn't need it as the whole toolchain should be
    // sandboxed. This isn't the case unfortunately.
    PATH.push('$PATH');
    MAN_PATH.push('$MAN_PATH');

    evalIntoEnv(env, [
      {
        name: 'OCAMLPATH',
        value: OCAMLPATH.join(getPathsDelimiter('OCAMLPATH', config.buildPlatform)),
        exported: true,
        exclusive: true,
      },
      {
        name: 'OCAMLFIND_DESTDIR',
        value: ocamlfindDest,
        exported: true,
        exclusive: true,
      },
      {
        name: 'OCAMLFIND_LDCONF',
        value: 'ignore',
        exported: true,
        exclusive: true,
      },
      {
        name: 'OCAMLFIND_COMMANDS',
        // eslint-disable-next-line max-len
        value:
          'ocamlc=ocamlc.opt ocamldep=ocamldep.opt ocamldoc=ocamldoc.opt ocamllex=ocamllex.opt ocamlopt=ocamlopt.opt',
        exported: true,
        exclusive: true,
      },
      {
        name: 'PATH',
        value: PATH.join(getPathsDelimiter('PATH', config.buildPlatform)),
        exported: true,
      },
      {
        name: 'MAN_PATH',
        value: MAN_PATH.join(getPathsDelimiter('MAN_PATH', config.buildPlatform)),
        exported: true,
      },
    ]);

    const errors = [];

    // $cur__name, $cur__version and so on...
    mergeIntoMap(env, getBuiltInScope(scopes.spec, config, true));

    // direct deps' local scopes
    for (const dep of scopes.dependencies.values()) {
      mergeIntoMap(env, dep.scopes.localScope);
    }
    // build's own local scope
    mergeIntoMap(env, scopes.localScope);
    // all deps' global scopes merged
    mergeIntoMap(
      env,
      Env.merge(
        Array.from(scopes.allDependencies.values())
          .map(dep => dep.scopes.globalScope)
          .concat(scopes.globalScope),
        evalIntoEnv,
      ),
    );

    if (params != null && params.env != null) {
      evalIntoEnv(env, Array.from(params.env.values()));
    }

    const scope = new Map();
    mergeIntoMap(scope, getEvalScope(scopes.spec, scopes.dependencies, config));
    mergeIntoMap(scope, env);

    const buildCommand = scopes.spec.buildCommand.map(command =>
      renderCommand(command, scope),
    );
    const installCommand = scopes.spec.installCommand.map(command =>
      renderCommand(command, scope),
    );

    return {
      id: scopes.spec.id,
      spec: scopes.spec,
      buildCommand,
      installCommand,
      env,
      scope,
      dependencies: mapValuesMap(scopes.dependencies, dep => dep.task),
      errors,
    };
  }

  function renderCommand(command: Array<string> | string, scope) {
    if (Array.isArray(command)) {
      return {
        command: command.join(' '),
        renderedCommand: command
          .map(command => quoteArgIfNeeded(expandWithScope(command, scope).rendered))
          .join(' '),
      };
    } else {
      return {
        command,
        renderedCommand: expandWithScope(command, scope).rendered,
      };
    }
  }

  return task;
}

function builtInEntry({
  name,
  value,
  spec,
  exclusive = true,
  exported = false,
}: {
  name: string,
  value: string,
  spec?: BuildSpec,
  exclusive?: boolean,
  exported?: boolean,
}) {
  return [name, {name, value, spec, builtIn: true, exclusive, exported}];
}

function builtInEntries(...values) {
  return new Map(values.map(builtInEntry));
}

function getBuiltInScope(
  spec: BuildSpec,
  config: Config<path.Path>,
  currentlyBuilding?: boolean,
): BuildEnvironment {
  const prefix = currentlyBuilding ? 'cur' : normalizePackageName(spec.name);
  const getInstallPath = currentlyBuilding
    ? config.getInstallPath
    : config.getFinalInstallPath;
  return builtInEntries(
    {
      name: `${prefix}__name`,
      value: spec.name,
      spec,
    },
    {
      name: `${prefix}__version`,
      value: spec.version,
      spec,
    },
    {
      name: `${prefix}__root`,
      value:
        currentlyBuilding && config.requiresRootRelocation(spec)
          ? config.getBuildPath(spec)
          : config.getRootPath(spec),
      spec,
    },
    {
      name: `${prefix}__depends`,
      value: Array.from(spec.dependencies.values(), dep => dep.name).join(' '),
      spec,
    },
    {
      name: `${prefix}__target_dir`,
      value: config.getBuildPath(spec),
      spec,
    },
    {
      name: `${prefix}__install`,
      value: getInstallPath(spec),
      spec,
    },
    {
      name: `${prefix}__bin`,
      value: getInstallPath(spec, 'bin'),
      spec,
    },
    {
      name: `${prefix}__sbin`,
      value: getInstallPath(spec, 'sbin'),
      spec,
    },
    {
      name: `${prefix}__lib`,
      value: getInstallPath(spec, 'lib'),
      spec,
    },
    {
      name: `${prefix}__man`,
      value: getInstallPath(spec, 'man'),
      spec,
    },
    {
      name: `${prefix}__doc`,
      value: getInstallPath(spec, 'doc'),
      spec,
    },
    {
      name: `${prefix}__stublibs`,
      value: getInstallPath(spec, 'stublibs'),
      spec,
    },
    {
      name: `${prefix}__toplevel`,
      value: getInstallPath(spec, 'toplevel'),
      spec,
    },
    {
      name: `${prefix}__share`,
      value: getInstallPath(spec, 'share'),
      spec,
    },
    {
      name: `${prefix}__etc`,
      value: getInstallPath(spec, 'etc'),
      spec,
    },
  );
}

function evalIntoEnv<V: {name: string, value: string}>(
  scope: BuildEnvironment,
  items: Array<V>,
) {
  const update = new Map();
  for (const item of items) {
    const nextItem = {
      exported: true,
      exclusive: false,
      builtIn: false,
      ...item,
      value: renderWithScope(item.value, scope).rendered,
    };
    update.set(item.name, nextItem);
  }
  mergeIntoMap(scope, update);
  return scope;
}

function getEvalScope(spec: BuildSpec, dependencies, config): BuildEnvironment {
  const evalScope = new Map();
  for (const dep of dependencies.values()) {
    mergeIntoMap(evalScope, getBuiltInScope(dep.spec, config));
    mergeIntoMap(evalScope, dep.scopes.localScope);
  }
  mergeIntoMap(evalScope, getBuiltInScope(spec, config));
  return evalScope;
}

const FIND_VAR_RE = /\$([a-zA-Z0-9_]+)/g;

export function renderWithScope<T: {value: string}>(
  value: string,
  scope: Map<string, T>,
): {rendered: string} {
  const rendered = value.replace(FIND_VAR_RE, (_, name) => {
    const value = scope.get(name);
    if (value == null) {
      return `\$${name}`;
    } else {
      return value.value;
    }
  });
  return {rendered};
}

export function quoteArgIfNeeded(arg: string): string {
  if (arg.indexOf(' ') === -1 && arg.indexOf("'") === -1 && arg.indexOf('"') === -1) {
    return arg;
  } else {
    return doubleQuote(arg);
  }
}

export function expandWithScope<T: {value: string}>(
  value: string,
  scope: Map<string, T>,
): {rendered: string} {
  const {value: rendered} = substituteVariables(value, {
    env: name => {
      const item = scope.get(name);
      return item != null ? item.value : undefined;
    },
  });
  return {rendered: rendered != null ? rendered : value};
}

export function fromBuildSandbox<Path: path.Path>(
  sandbox: BuildSandbox,
  config: Config<Path>,
  params?: BuildTaskParams,
): BuildTask {
  const env = new Map();
  if (sandbox.env) {
    mergeIntoMap(env, sandbox.env);
  }
  if (params != null && params.env != null) {
    mergeIntoMap(env, params.env);
  }
  return fromBuildSpec(sandbox.root, config, {...params, env});
}

/**
 * Logic to determine how file paths inside of env vars should be delimited.
 * For example, what separates file paths in the `PATH` env variable, or
 * `OCAMLPATH` variable? In an ideal world, the logic would be very simple:
 * `linux`/`darwin`/`cygwin` always uses `:`, and Windows/MinGW always uses
 * `;`, however there's some unfortunate edge cases to deal with - `esy` can
 * take care of all of that for you.
 */
function getPathsDelimiter(envVarName: string, buildPlatform: BuildPlatform) {
  // Error as a courtesy. This means something went wrong in the esy code, not
  // consumer code. Should be fixed ASAP.
  if (envVarName === '' || envVarName.charAt(0) === '$') {
    throw new Error('Invalidly formed environment variable:' + envVarName);
  }
  if (buildPlatform === null || buildPlatform === undefined) {
    throw new Error('Build platform not specified');
  }
  // Comprehensive pattern matching would be nice to have here!
  return envVarName === 'OCAMLPATH' && buildPlatform === 'cygwin'
    ? ';'
    : buildPlatform === 'cygwin' ||
      buildPlatform === 'linux' ||
      buildPlatform === 'darwin'
      ? ':'
      : ';';
}
