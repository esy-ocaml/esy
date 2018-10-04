// @flow

const crypto = require('crypto');
const fs = require('fs-extra');
const os = require('os');
const path = require('path');
const childProcess = require('child_process');
const rmSync = require('rimraf').sync;
const isCi = require("is-ci");

const isWindows = process.platform === 'win32';
const ocamlVersion = '4.6.6';

const esyCommand =
  process.platform === 'win32'
    ? require.resolve('../_build/default/esy/bin/esyCommand.exe')
    : require.resolve('../bin/esy');

function getTempDir() {
  // The appveyor temp folder has some permission issues - 
  // so in that environment, we'll run these tests from a root folder.
  const appVeyorTempFolder = "C:/esy-ci-temp";
  return isWindows ? (isCi ? appVeyorTempFolder : os.tmpdir()) : '/tmp';
}

const esyPrefixPath =
  process.env.TEST_ESY_PREFIX != null
    ? process.env.TEST_ESY_PREFIX
    : path.join(getTempDir(), crypto.randomBytes(20).toString('hex'));

function mkdir(path) {
  try {
    fs.mkdirpSync(path);
  } catch (e) {
    // doesn't matter if it exists
  }
}

function mkdirTemp() {
  const p = path.join(getTempDir(), crypto.randomBytes(20).toString('hex'));
  fs.mkdirpSync(p);
  return p;
}

/*::

 type TestSandbox = {
   path: string,
   cd: (where: string) => void,
   exec: (...args: string[]) => void,
   esy: (...args: string[]) => void,
   dispose: () => void,
 };

*/

function createSandbox() /* : TestSandbox */ {
  const sandboxPath = mkdirTemp();

  let cwd = sandboxPath;

  function exec(...args /* : Array<string> */) {
    const normalizedArgs = args.map(arg => arg.split("\\").join("/"));
    const cmd = normalizedArgs.join(" ");
    console.log(`EXEC: ${cmd}`);
    childProcess.execSync(cmd, {
      cwd: cwd,
      env: {...process.env, ESY__PREFIX: esyPrefixPath},
      stdio: 'inherit',
    });
  }

  function cd(where) {
    cwd = path.resolve(cwd, where);
    console.log(`CWD: ${cwd}`);
  }

  return {
    path: sandboxPath,
    exec: exec,
    cd,
    esy(...args /* : Array<string> */) {
      return exec(esyCommand, ...args);
    },
    dispose: () => {
      rmSync(sandboxPath);
    },
  };
}

function buildOCaml() {
  const sandbox = createSandbox();

  fs.writeFileSync(
    path.join(sandbox.path, 'package.json'),
    JSON.stringify({
      name: 'root-project',
      version: '1.0.0',
      dependencies: {
        ocaml: ocamlVersion,
      },
      esy: {
        build: [],
        install: [],
      },
    }),
  );

  console.log(`*** building OCaml toolchain at ${sandbox.path} ***`);

  sandbox.esy('install');
  sandbox.esy('build');
  sandbox.dispose();
}

function setup(_globalConfig /* : any */) {
  buildOCaml();
}

module.exports.setup = setup;
module.exports.esyPrefixPath = esyPrefixPath;
module.exports.esyCommand = esyCommand;
module.exports.isWindows = isWindows;
module.exports.mkdirTemp = mkdirTemp;
module.exports.ocamlVersion = ocamlVersion;
module.exports.createSandbox = createSandbox;
