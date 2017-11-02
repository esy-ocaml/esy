/**
 * Utilities for programmatic Makefile genetation.
 *
 * @flow
 */

import outdent from 'outdent';
import {doubleQuote} from './lib/shell';

export type Env = {
  [name: string]: null | string | QuotedString,
};

export type MakefileItemDependency = string | MakefileItem;

export type MakefileGroupItem = {
  type: 'group',
  dependencies: $ReadOnlyArray<MakefileItem>,
};

export type MakefileRuleItem = {
  type: 'rule',
  target: string,
  command?: ?string | Array<void | null | string | Env>,
  phony?: boolean,
  dependencies?: $ReadOnlyArray<string | MakefileItem>,
  env?: Env,
  exportEnv?: Array<string>,
  shell?: string,
};

export type MakefileDefineItem = {
  type: 'define',
  name: string,
  value: string | Array<void | null | string | Env>,
};

export type MakefileRawItem = {|
  type: 'raw',
  value: string,
|};

export type MakefileItem =
  | MakefileRuleItem
  | MakefileDefineItem
  | MakefileRawItem
  | MakefileGroupItem;

export function renderMakefile(entries: Array<MakefileItem>) {
  const seen = new Set();
  const queue: Array<MakefileItem> = entries.slice(0);
  const rendered = [];

  while (queue.length > 0) {
    const item = queue.shift();
    if (seen.has(item)) {
      continue;
    }
    seen.add(item);
    rendered.push(renderMakefileItem(item));

    if ((item.type === 'rule' || item.type === 'group') && item.dependencies != null) {
      for (const dep of item.dependencies) {
        if (seen.has(dep) || typeof dep === 'string') {
          continue;
        }
        queue.push(dep);
      }
    }
  }

  return rendered.join('\n\n');
}

function renderMakefileItem(item: MakefileItem): ?string {
  switch (item.type) {
    case 'rule':
      return renderMakefileRuleItem(item);
    case 'define':
      return rendereMakefileDefineItem(item);
    case 'raw':
      return renderMakefileRawItem(item);
    case 'group':
      return null;
    default:
      throw new Error(`Unknown item: ${JSON.stringify(item)}`);
  }
}

function rendereMakefileDefineItem({name, value}: MakefileDefineItem) {
  return `define ${name}\n${escapeEnvVar(renderCommand(value))}\nendef`;
}

function renderMakefileRawItem({value}: MakefileRawItem) {
  return value;
}

function renderMakefileRuleItem(rule: MakefileRuleItem) {
  const {target, dependencies = [], command, phony, env, exportEnv, shell} = rule;
  const header = `${target}: ${renderRuleDependencies(dependencies)}`;

  let prelude = '';
  if (exportEnv) {
    exportEnv.forEach(name => {
      prelude += `export ${name}\n`;
    });
  }

  if (phony) {
    prelude += `.PHONY: ${target}\n`;
  }

  if (shell != null) {
    prelude += `${target}: SHELL=${shell}\n`;
  }

  if (command != null) {
    const recipe = escapeEnvVar(renderCommand(command));
    if (env) {
      const envString = renderEnv(env);
      return `${prelude}${header}\n${envString}\\\n${recipe}`;
    } else {
      return `${prelude}${header}\n${recipe}`;
    }
  } else {
    return prelude + header;
  }
}

function renderRuleDependencies(
  dependencies: $ReadOnlyArray<MakefileItemDependency>,
): string {
  const rendered: Array<string> = [];
  for (const dep of dependencies) {
    if (typeof dep === 'string') {
      rendered.push(dep);
    } else if (dep.type === 'rule') {
      rendered.push(dep.target);
    }
  }
  return rendered.join(' ');
}

function renderEnv(env) {
  const lines = [];
  for (const k in env) {
    const v = env[k];
    if (v == null) {
      continue;
    } else if (typeof v === 'string') {
      lines.push(`\texport ${k}=${doubleQuote(v)};`);
    } else {
      lines.push(`\texport ${k}=${v.value};`);
    }
  }
  return lines.join('\\\n');
}

function renderCommand(command) {
  if (Array.isArray(command)) {
    return command
      .filter(item => item != null)
      .map(item => (typeof item === 'string' ? renderCommand(item) : renderEnv(item)))
      .join('\\\n');
  } else {
    return command
      .split('\n')
      .map(line => `\t${line};`)
      .join('\\\n');
  }
}

function escapeEnvVar(command) {
  return command.replace(/\$([^\(])/g, '$$$$$1');
}

function escapeName(name) {
  return name.replace(/[^a-zA-Z0-9]/g, '_').replace(/_+/g, '_');
}

type QuotedString = {type: 'quoted', value: string};

export function quoted(value: string) {
  return {type: 'quoted', value};
}

export function createRule(
  config: $Diff<MakefileRuleItem, {type: 'rule'}>,
): MakefileRuleItem {
  return {type: 'rule', ...config};
}

export function createDefine(
  config: $Diff<MakefileDefineItem, {type: 'define'}>,
): MakefileDefineItem {
  return {type: 'define', ...config};
}

export function createRaw(value: string): MakefileRawItem {
  return {type: 'raw', value};
}

export function createGroup(...dependencies: Array<MakefileItem>): MakefileGroupItem {
  return {type: 'group', dependencies};
}
