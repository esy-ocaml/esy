// @flow

const path = require('path');
const fs = require('fs-extra');

const helpers = require('./test/helpers');
const {packageJson, dir} = helpers;

helpers.skipSuiteOnWindows('#301');

function makeFixture(p) {
  return [
    packageJson({
      name: 'simple-project',
      version: '1.0.0',
      dependencies: {
        dep: 'path:./dep',
      },
      devDependencies: {
        devDep: 'path:./devDep',
      },
      esy: {
        buildEnv: {
          root__build: 'root__build__value',
        },
        exportedEnv: {
          root__local: {val: 'root__local__value'},
          root__global: {val: 'root__global__value', scope: 'global'},
        },
      },
    }),
    dir(
      'dep',
      packageJson({
        name: 'dep',
        version: '1.0.0',
        esy: {
          install: [
            'cp #{self.root / self.name}.js #{self.bin / self.name}.js',
            helpers.buildCommand(p, '#{self.bin / self.name}.js'),
          ],
          exportedEnv: {
            dep__local: {val: 'dep__local__value'},
            dep__global: {val: 'dep__global__value', scope: 'global'},
          },
        },
        dependencies: {
          depOfDep: 'path:../depOfDep',
        },
      }),

      helpers.dummyExecutable('dep'),
    ),
    dir(
      'depOfDep',
      packageJson({
        name: 'depOfDep',
        version: '1.0.0',
        esy: {
          exportedEnv: {
            depOfDep__local: {val: 'depOfDep__local__value'},
            depOfDep__global: {val: 'depOfDep__global__value', scope: 'global'},
          },
        },
      }),
    ),
    dir(
      'devDep',
      packageJson({
        name: 'devDep',
        version: '1.0.0',
        esy: {
          install: [
            'cp #{self.root / self.name}.js #{self.bin / self.name}.js',
            helpers.buildCommand(p, '#{self.bin / self.name}.js'),
          ],
          exportedEnv: {
            devDep__local: {val: 'devDep__local__value'},
            devDep__global: {val: 'devDep__global__value', scope: 'global'},
          },
        },
      }),
      helpers.dummyExecutable('devDep'),
    ),
  ];
}

describe('esy command-env', () => {
  it('generates env as a bash source', async () => {
    const p = await helpers.createTestSandbox();
    await p.fixture(...makeFixture(p));
    await p.esy('install');
    await p.esy('build');

    const env = (await p.esy('command-env')).stdout;

    await fs.writeFile(path.join(p.projectPath, 'command-env'), env);

    await expect(
      helpers.promiseExec('. ./command-env && dep.cmd', {
        cwd: p.projectPath,
      }),
    ).resolves.toEqual({stdout: '__dep__\n', stderr: ''});

    await expect(
      helpers.promiseExec('. ./command-env && devDep.cmd', {
        cwd: p.projectPath,
      }),
    ).resolves.toEqual({stdout: '__devDep__\n', stderr: ''});
  });

  it('generates env as JSON', async () => {
    const p = await helpers.createTestSandbox();
    await p.fixture(...makeFixture(p));
    await p.esy('install');
    await p.esy('build');

    const env = JSON.parse((await p.esy('command-env --json')).stdout);

    expect(env.cur__version).toBe('1.0.0');
    expect(env.cur__name).toBe('simple-project');
    expect(env.cur__toplevel).toBeTruthy();
    expect(env.cur__target_dir).toBeTruthy();
    expect(env.cur__stublibs).toBeTruthy();
    expect(env.cur__share).toBeTruthy();
    expect(env.cur__sbin).toBeTruthy();
    expect(env.cur__root).toBeTruthy();

    expect(env.cur__original_root).toBeTruthy();
    expect(env.cur__original_root).toBe(p.projectPath);

    expect(env.cur__man).toBeTruthy();
    expect(env.cur__lib).toBeTruthy();
    expect(env.cur__install).toBeTruthy();
    expect(env.cur__etc).toBeTruthy();
    expect(env.cur__doc).toBeTruthy();
    expect(env.cur__bin).toBeTruthy();
    expect(env.SHELL).toBeTruthy();
    expect(env.PATH).toBeTruthy();
    expect(env.OCAMLFIND_CONF).toBeTruthy();
    expect(env.MAN_PATH).toBeTruthy();
    expect(env.CAML_LD_LIBRARY_PATH).toBeTruthy();

    expect(env.DUNE_BUILD_DIR).toBeTruthy();
    expect(env.DUNE_BUILD_DIR).toBe(env.cur__target_dir);

    // build env
    expect(env.root__build).toBe('root__build__value');

    // exported env isn't present in the build env of the same package
    expect(env.root__local).toBe(undefined);
    expect(env.root__global).toBe(undefined);

    // deps are present in command env
    expect(env.dep__local).toBe('dep__local__value');
    expect(env.dep__global).toBe('dep__global__value');

    // but only direct deps contribute local exports
    expect(env.depOfDep__local).toBe(undefined);
    expect(env.depOfDep__global).toBe('depOfDep__global__value');

    // dev deps are present in command env
    expect(env.devDep__local).toBe('devDep__local__value');
    expect(env.devDep__global).toBe('devDep__global__value');
  });
});
