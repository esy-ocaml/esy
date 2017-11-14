/**
 * @flow
 */

require('babel-polyfill');

import type {Config, Sandbox, BuildTask, BuildPlatform} from '../types';
import type {Options as SandboxOptions} from '../sandbox/project-sandbox';
import ConsoleReporter from '@esy-ocaml/esy-install/src/reporters/console/console-reporter';

import loudRejection from 'loud-rejection';
import userHome from 'user-home';
import * as path from '../lib/path';
import chalk from 'chalk';
import parse from 'cli-argparse';

const pkg = require('../../package.json');
// for deterministic test output
if (process.env.NODE_ENV === 'test') {
  pkg.version = '0.0.0';
}

function getSandboxPath() {
  if (process.env.ESY__SANDBOX != null) {
    return process.env.ESY__SANDBOX;
  } else {
    // TODO: Need to change this to climb to closest package.json.
    return process.cwd();
  }
}

function getPrefixPath() {
  if (process.env.ESY__PREFIX != null) {
    return process.env.ESY__PREFIX;
  } else {
    return path.join(userHome, '.esy');
  }
}

function getReadOnlyStorePath() {
  if (process.env.ESY__READ_ONLY_STORE_PATH != null) {
    return process.env.ESY__READ_ONLY_STORE_PATH.split(':');
  } else {
    return [];
  }
}

function getBuildPlatform() {
  if (process.platform === 'darwin') {
    return 'darwin';
  } else if (process.platform === 'linux') {
    return 'linux';
  } else {
    // think cygwin/wsl
    return 'linux';
  }
}

export async function getSandbox(
  ctx: CommandContext,
  options?: SandboxOptions,
): Promise<Sandbox> {
  const ProjectSandbox = require('../sandbox/project-sandbox');
  const sandbox = await ProjectSandbox.create(ctx.sandboxPath, options);
  if (sandbox.root.errors.length > 0) {
    sandbox.root.errors.forEach(error => {
      console.log(formatError(error.message));
    });
    process.exit(1);
  }
  return sandbox;
}

export async function getBuildConfig(
  ctx: CommandContext,
): Promise<Config<path.AbsolutePath>> {
  const {createForPrefix} = require('../config');

  return createForPrefix({
    reporter: ctx.reporter,
    prefixPath: ctx.prefixPath,
    sandboxPath: ctx.sandboxPath,
    buildPlatform: ctx.buildPlatform,
    readOnlyStorePath: ctx.readOnlyStorePath,
  });
}

function formatError(message: string, stack?: string) {
  let result = message;
  if (stack != null) {
    result += `\n${stack}`;
  }
  return result;
}

function error(error?: Error | string) {
  if (error != null) {
    const message = String(error.message ? error.message : error);
    const stack = error.stack ? String(error.stack) : undefined;
    console.log(formatError(message, stack));
  }
  process.exit(1);
}

export function indent(string: string, indent: string) {
  return string
    .split('\n')
    .map(line => indent + line)
    .join('\n');
}

export type CommandContext = {
  prefixPath: string,
  sandboxPath: string,
  readOnlyStorePath: Array<string>,
  buildPlatform: BuildPlatform,
  executeCommand: (commandName: string, args: string[]) => Promise<void>,
  version: string,
  reporter: ConsoleReporter,
  error(message?: string): any,
};

export type CommandInvocation = {
  commandName: string,
  args: Array<string>,
  options: {
    options: {[name: string]: string},
    flags: {[name: string]: boolean},
  },
};

type Command = {
  default: (CommandContext, CommandInvocation) => any,
  options?: Object,
};

const commandsByName: {[name: string]: () => Command} = {
  build: () => require('./esyBuild'),
  'build-ls': () => require('./esyBuildLs'),
  release: () => require('./esyRelease'),
  config: () => require('./esyConfig'),
  install: () => require('./esyInstall'),
  add: () => require('./esyAdd'),
  x: () => require('./esyX'),
  'build-eject': () => require('./esyBuildEject'),
  'build-shell': () => require('./esyBuildShell'),
  'import-opam': () => require('./esyImportOpam'),
  'print-env': () => require('./esyPrintEnv'),
  'install-cache': () => require('./esyInstallCache'),
};

async function main() {
  const commandName = process.argv[2];

  const isTTY = (process.stdout: any).isTTY;
  const reporter = new ConsoleReporter({
    emoji: isTTY,
    verbose: false,
    noProgress: !isTTY,
    isSilent: process.env.ESY__SILENT === '1',
  });
  const error = (error?: Error | string) => {
    if (error != null) {
      const message = String(error.message ? error.message : error);
      const stack = error.stack ? String(error.stack) : undefined;
      reporter.error(formatError(message, stack));
    }
    reporter.close();
    process.exit(1);
  };

  if (commandName == null) {
    error('no command provided');
  }

  const command = commandsByName[commandName];
  if (command != null) {
    reporter.header(commandName, pkg);

    async function executeCommand(commandName, initialArgs) {
      const command = commandsByName[commandName];
      const commandImpl = command();
      const options = parse(initialArgs, {
        ...commandImpl.options,
        strict: true,
      });
      const args = [];
      for (const opt of options.unparsed) {
        if (opt.startsWith('-')) {
          error(`unknown option ${opt}`);
        }
        args.push(opt);
      }
      await commandImpl.default(commandCtx, {commandName, args, options});
    }

    const commandCtx: CommandContext = {
      version: pkg.version,
      prefixPath: getPrefixPath(),
      readOnlyStorePath: getReadOnlyStorePath(),
      sandboxPath: getSandboxPath(),
      buildPlatform: getBuildPlatform(),
      executeCommand,
      error,
      reporter,
    };

    await executeCommand(commandName, process.argv.slice(3));
  } else {
    error(`unknown command: ${commandName}`);
  }
}

main().catch(error);
loudRejection();
