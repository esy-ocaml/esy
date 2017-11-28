/**
 * @flow
 */

jest.setTimeout(200000);

import * as path from 'path';
import * as fs from '../src/lib/fs';
import * as child from '../src/lib/child_process.js';
import outdent from 'outdent';

const DEBUG_TEST_LOC = '/tmp/esydbg';

export const esyRoot = path.dirname(__dirname);
// We use version of esy executable w/o lock so we can run in parallel. We make
// sure we use isolated sources for tests so this is ok.
export const esyBin = path.join(esyRoot, 'bin', '_esy');
export const testUtilsBash = require.resolve('./testlib.sh');

let tempDirectoriesCreatedDuringTestRun = [];

export async function cleanUp() {
  if (tempDirectoriesCreatedDuringTestRun.length > 0) {
    await Promise.all(tempDirectoriesCreatedDuringTestRun.map(p => fs.rmdir(p)));
    tempDirectoriesCreatedDuringTestRun = [];
  }
}

export function spawn(command: string, args: string[], options: any = {}) {
  if (process.env.DEBUG != null) {
    console.log(outdent`
      CWD ${options.cwd || process.cwd()}
      EXECUTE ${command} ${args.join(' ')}
    `);
  }
  return child.spawn(command, args, options);
}

export function run(command: string, ...args: string[]) {
  return spawn(command, args);
}

export function runIn(project: string, command: string, ...args: string[]) {
  return spawn(command, args, {cwd: project});
}

export async function mkdtemp() {
  // We should be using `os.tmpdir()` instead but it's too long so we cannot
  // relocate binaries there.
  const root = '/tmp/';
  const dir = await fs._mkdtemp(root);
  tempDirectoriesCreatedDuringTestRun.push(dir);
  return dir;
}

export function mkdtempSync() {
  // We should be using `os.tmpdir()` instead but it's too long so we cannot
  // relocate binaries there.
  const root = '/tmp/';
  const dir = fs._mkdtempSync(root);
  tempDirectoriesCreatedDuringTestRun.push(dir);
  return dir;
}

/**
 * Initialize fixture.
 */
export function initFixtureSync(fixturePath: string) {
  let root;
  if (process.env.DEBUG != null) {
    console.log(outdent`

      Test Debug Notice!
      ------------------

      Test is being executed in DEBUG mode. The location for tests release & installation
      is set to /tmp/esydbg.

      Make sure you run only a single test case at a time with DEBUG as /tmp/esydbg is going
      to be removed before the test run. After test is done with either status, you can go
      into /tmp/esydbg and inspect its contents.

      Note thet if test fails during 'npm install' phase then npm will do a rollback and
      /tmp/esydbg/npm directory will become empty.

    `);
    fs.rmdirSync(DEBUG_TEST_LOC);
    root = DEBUG_TEST_LOC;
  } else {
    root = mkdtempSync();
  }
  const project = path.join(root, 'project');
  const npmPrefix = path.join(root, 'npm');
  const esyPrefix = path.join(root, 'esy');
  const localEsyPrefix = path.join(project, 'node_modules', '.cache', '_esy');

  fs.copydirSync(fixturePath, project);

  // Patch package.json to include dependency on esy.
  const packageJsonFilename = path.join(project, 'package.json');
  const packageJson = fs.readJsonSync(packageJsonFilename);
  packageJson.devDependencies = packageJson.devDependencies || {};
  packageJson.devDependencies.esy = esyRoot;
  fs.writeFileSync(packageJsonFilename, JSON.stringify(packageJson, null, 2), 'utf8');

  const env = {
    ...process.env,
    ESY__PREFIX: esyPrefix,
  };

  const shellInProject = async (script: string) => {
    script = `
      source "${testUtilsBash}"

      function esy () {
        "${esyBin}" "$@"
      }

      function npmGlobal () {
        npm --prefix "${npmPrefix}" "$@"
      }

      export ESY__COMMAND="${require.resolve('../bin/_esy')}"
      export PATH="${npmPrefix}/bin:$PATH"

      set -u
      set -o pipefail

      run esy config ls

      ${script}
    `;
    const options = {env: {...env}, cwd: project};
    console.log(await spawn('/bin/bash', ['-c', script], options));
  };

  return {
    description: packageJson.description || packageJson.name,
    root,
    project,
    npmPrefix,
    esyPrefix,
    localEsyPrefix,
    shellInProject,
  };
}

export function defineTestCaseWithShell(fixturePath: string, shellScript: string) {
  const fixture = initFixtureSync(fixturePath);

  test(`build ${fixture.description}`, async function() {
    await fixture.shellInProject(shellScript);
  });

  afterAll(cleanUp);
}
