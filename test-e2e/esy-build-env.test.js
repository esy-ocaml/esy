// @flow

const os = require('os');
const path = require('path');
const fs = require('fs-extra');

const helpers = require('./test/helpers');
const {promiseExec, packageJson, dir, file, dummyExecutable, buildCommand} = helpers;

helpers.skipSuiteOnWindows('#301');

describe(`'esy build-env' command`, () => {
  function createPackage(p, {name, dependencies}: {name: string, dependencies?: Object}) {
    return dir(
      name,
      packageJson({
        name,
        version: '1.0.0',
        dependencies,
        esy: {
          install: [
            'cp #{self.root / self.name}.js #{self.bin / self.name}.js',
            buildCommand(p, '#{self.bin / self.name}.js'),
          ],
          buildEnv: {
            [`${name}__buildvar`]: `${name}__buildvar__value`,
          },
          exportedEnv: {
            [`${name}__local`]: {val: `${name}__local__value`},
            [`${name}__global`]: {val: `${name}__global__value`, scope: 'global'},
          },
        },
      }),

      dummyExecutable(name),
    );
  }

  async function createTestSandbox() {
    const p = await helpers.createTestSandbox();
    await p.fixture(
      packageJson({
        name: 'simple-project',
        version: '1.0.0',
        dependencies: {
          dep: 'path:./dep',
          linkedDep: '*',
        },
        devDependencies: {
          devDep: 'path:./devDep',
        },
        resolutions: {
          linkedDep: 'link:./linkedDep',
        },
        esy: {
          buildEnv: {
            root__build: 'root__build__value',
            emptyVariable: '',
          },
          exportedEnv: {
            root__local: {val: 'root__local__value'},
            root__global: {val: 'root__global__value', scope: 'global'},
          },
        },
      }),
      createPackage(p, {name: 'dep', dependencies: {depOfDep: 'path:../depOfDep'}}),
      createPackage(p, {name: 'depOfDep'}),
      createPackage(p, {name: 'linkedDep'}),
      createPackage(p, {name: 'devDep'}),
    );
    return p;
  }

  it('generates an environment as bash source', async () => {
    const p = await createTestSandbox();

    await p.esy('install');
    await p.esy('build');

    const env = (await p.esy('build-env')).stdout;

    await fs.writeFile(path.join(p.projectPath, 'build-env'), env);

    await expect(
      promiseExec('. ./build-env && dep.cmd', {
        cwd: p.projectPath,
      }),
    ).resolves.toEqual({stdout: '__dep__\n', stderr: ''});

    await expect(
      promiseExec('. ./build-env && devDep.cmd', {
        cwd: p.projectPath,
      }),
    ).resolves.toMatchObject({
      stdout: '__devDep__' + os.EOL,
      stderr: '',
    });
  });

  it('generates an environment as bash source (--release)', async () => {
    const p = await createTestSandbox();

    await p.esy('install');
    await p.esy('build');

    const env = (await p.esy('build-env --release')).stdout;

    await fs.writeFile(path.join(p.projectPath, 'build-env'), env);

    await expect(
      promiseExec('. ./build-env && dep.cmd', {
        cwd: p.projectPath,
      }),
    ).resolves.toEqual({stdout: '__dep__\n', stderr: ''});

    await expect(
      promiseExec('. ./build-env && devDep.cmd', {
        cwd: p.projectPath,
      }),
    ).rejects.toMatchObject({
      code: 127
    });
  });

  it('generates an environment in JSON', async () => {
    const p = await createTestSandbox();

    await p.esy('install');

    const env = JSON.parse((await p.esy('build-env --json')).stdout);

    expect(env.cur__name).toBe('simple-project');
    expect(env.cur__version).toBe('1.0.0');
    expect(env.cur__dev).toBe('true');
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
    expect(env.OCAMLPATH).toBeTruthy();
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

    // deps are present in build env
    expect(env.dep__local).toBe('dep__local__value');
    expect(env.dep__global).toBe('dep__global__value');

    // but only direct deps contribute local exports
    expect(env.depOfDep__local).toBe(undefined);
    expect(env.depOfDep__global).toBe('depOfDep__global__value');

    // dev deps are not present in build env
    expect(env.devDep__local).toBe('devDep__local__value');
    expect(env.devDep__global).toBe('devDep__global__value');

    // check if empty variables are defined
    expect(env.emptyVariable).toBe('');
  });

  it('generates an environment in JSON (--release)', async () => {
    const p = await createTestSandbox();

    await p.esy('install');

    const env = JSON.parse((await p.esy('build-env --release --json')).stdout);

    expect(env.cur__name).toBe('simple-project');
    expect(env.cur__version).toBe('1.0.0');
    expect(env.cur__dev).toBe('false');
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

    // deps are present in build env
    expect(env.dep__local).toBe('dep__local__value');
    expect(env.dep__global).toBe('dep__global__value');

    // but only direct deps contribute local exports
    expect(env.depOfDep__local).toBe(undefined);
    expect(env.depOfDep__global).toBe('depOfDep__global__value');

    // dev deps are not present in build env
    expect(env.devDep__local).toBe(undefined);
    expect(env.devDep__global).toBe(undefined);
  });

  it('allows to query build env for a dep (by name)', async () => {
    const p = await createTestSandbox();

    await p.esy('install');

    const env = JSON.parse((await p.esy('build-env --json -p dep')).stdout);
    expect(env.cur__name).toBe('dep');
    expect(env.cur__dev).toBe('false');
  });

  it('allows to query build env for a linked dep (by name)', async () => {
    const p = await createTestSandbox();

    await p.esy('install');

    const env = JSON.parse((await p.esy('build-env --json -p linkedDep')).stdout);
    expect(env.cur__name).toBe('linkedDep');
    expect(env.cur__dev).toBe('false');
  });

  it('allows to query build env for a dep (by name, version)', async () => {
    const p = await createTestSandbox();

    await p.esy('install');

    const env = JSON.parse((await p.esy('build-env --json -p dep@path:dep')).stdout);
    expect(env.cur__name).toBe('dep');
  });

  it('expands a variable to a global path for the sandbox', async () => {
    // Append the current path so we can access tools like `cp`
    process.env.TEST_PATH = "/some/path:" + process.env.PATH;

    const p = await createTestSandbox();

    await p.esy('install');

    const env = JSON.parse((await p.esy('build-env --global-path TEST_PATH --release --json')).stdout);

    expect(env.PATH).toContain("/some/path");
  });
});
