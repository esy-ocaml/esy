// @flow

const os = require('os');
const path = require('path');
const helpers = require('../test/helpers');
const {test, isWindows, isMacos, isLinux} = helpers;

helpers.skipSuiteOnWindows('Needs investigation');

function makePackage(
  p,
  {
    name,
    dependencies = {},
    devDependencies = {},
  }: {
    name: string,
    dependencies?: {[name: string]: string},
    devDependencies?: {[name: string]: string},
  },
  ...items
) {
  return helpers.dir(
    name,
    helpers.packageJson({
      name: name,
      version: '1.0.0',
      license: 'MIT',
      esy: {
        buildsInSource: true,
        build: [helpers.buildCommand(p, '#{self.root / self.name}.js')],
        install: [
          `cp #{self.root / self.name}.cmd #{self.bin / self.name}.cmd`,
          `cp #{self.root / self.name}.js #{self.bin / self.name}.js`,
        ],
      },
      dependencies,
      devDependencies,
    }),
    helpers.dummyExecutable(name),
    ...items,
  );
}

describe(`Project with "devDependencies"`, () => {
  async function createTestSandbox() {
    const p = await helpers.createTestSandbox();
    await p.fixture(
      helpers.packageJson({
        name: 'withDevDep',
        version: '1.0.0',
        esy: {
          build: 'true',
        },
        dependencies: {
          dep: 'path:./dep',
        },
        devDependencies: {
          devDep: 'path:./devDep',
        },
      }),
      makePackage(p, {
        name: 'dep',
        devDependencies: {devDepOfDep: '*'},
      }),
      makePackage(p, {
        name: 'devDep',
        dependencies: {
          depOfDevDep: 'path:../depOfDevDep',
        },
      }),
      makePackage(p, {
        name: 'depOfDevDep',
      }),
    );
    await p.esy('install');
    await p.esy('build');
    return p;
  }

  it('package "dep" should be visible in all envs', async () => {
    const p = await createTestSandbox();
    const expecting = expect.stringMatching('__dep__');

    {
      const {stdout} = await p.esy('dep.cmd');
      expect(stdout.trim()).toEqual(expecting);
    }

    {
      const {stdout} = await p.esy('b dep.cmd');
      expect(stdout.trim()).toEqual(expecting);
    }

    {
      const {stdout} = await p.esy('x dep.cmd');
      expect(stdout.trim()).toEqual(expecting);
    }
  });

  it(`package "dev-dep" is visible in command env / test env and via 'esy b CMD'`, async () => {
    const p = await createTestSandbox();

    {
      const {stdout} = await p.esy('devDep.cmd');
      expect(stdout.trim()).toEqual('__devDep__');
    }

    {
      const {stdout} = await p.esy('x devDep.cmd');
      expect(stdout.trim()).toEqual('__devDep__');
    }

    {
      const {stdout} = await p.esy('b devDep.cmd');
      expect(stdout.trim()).toEqual('__devDep__');
    }
  });

  test('build-env', async function() {
    const p = await createTestSandbox();
    const id = JSON.parse((await p.esy('build-plan')).stdout).id;
    const depId = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
    const devdepId = JSON.parse((await p.esy('build-plan -p devDep')).stdout).id;
    const depofdevdepId = JSON.parse((await p.esy('build-plan -p depOfDevDep')).stdout)
      .id;

    const {stdout} = await p.esy('build-env --json');
    const env = JSON.parse(stdout);
    expect(env).toMatchObject({
      cur__dev: 'true',
      cur__version: '1.0.0',
      cur__toplevel: `${p.projectPath}/_esy/default/store/i/${id}/toplevel`,
      cur__target_dir: `${p.projectPath}/_esy/default/store/b/${id}`,
      cur__stublibs: `${p.projectPath}/_esy/default/store/i/${id}/stublibs`,
      cur__share: `${p.projectPath}/_esy/default/store/i/${id}/share`,
      cur__sbin: `${p.projectPath}/_esy/default/store/i/${id}/sbin`,
      cur__root: `${p.projectPath}`,
      cur__original_root: `${p.projectPath}`,
      cur__name: `withDevDep`,
      cur__man: `${p.projectPath}/_esy/default/store/i/${id}/man`,
      cur__lib: `${p.projectPath}/_esy/default/store/i/${id}/lib`,
      cur__install: `${p.projectPath}/_esy/default/store/i/${id}`,
      cur__etc: `${p.projectPath}/_esy/default/store/i/${id}/etc`,
      cur__doc: `${p.projectPath}/_esy/default/store/i/${id}/doc`,
      cur__bin: `${p.projectPath}/_esy/default/store/i/${id}/bin`,
      PATH: [
        `${p.esyStorePath}/i/${depId}/bin`,
        `${p.esyStorePath}/i/${devdepId}/bin`,
        `${p.esyStorePath}/i/${depofdevdepId}/bin`,
        ``,
        `/usr/local/bin`,
        `/usr/bin`,
        `/bin`,
        `/usr/sbin`,
        `/sbin`,
      ].join(path.delimiter),
      OCAMLFIND_CONF: `${p.projectPath}/_esy/default/store/p/${id}/etc/findlib.conf`,
      DUNE_BUILD_DIR: `${p.projectPath}/_esy/default/store/b/${id}`,
    });
  });

  test('build-env --release', async function() {
    const p = await createTestSandbox();
    const id = JSON.parse((await p.esy('build-plan --release')).stdout).id;
    const depId = JSON.parse((await p.esy('build-plan --release -p dep')).stdout).id;

    const {stdout} = await p.esy('build-env --json --release');
    const env = JSON.parse(stdout);
    expect(env).toMatchObject({
      cur__dev: 'false',
      cur__version: '1.0.0',
      cur__toplevel: `${p.projectPath}/_esy/default/store/i/${id}/toplevel`,
      cur__target_dir: `${p.projectPath}/_esy/default/store/b/${id}`,
      cur__stublibs: `${p.projectPath}/_esy/default/store/i/${id}/stublibs`,
      cur__share: `${p.projectPath}/_esy/default/store/i/${id}/share`,
      cur__sbin: `${p.projectPath}/_esy/default/store/i/${id}/sbin`,
      cur__root: `${p.projectPath}`,
      cur__original_root: `${p.projectPath}`,
      cur__name: `withDevDep`,
      cur__man: `${p.projectPath}/_esy/default/store/i/${id}/man`,
      cur__lib: `${p.projectPath}/_esy/default/store/i/${id}/lib`,
      cur__install: `${p.projectPath}/_esy/default/store/i/${id}`,
      cur__etc: `${p.projectPath}/_esy/default/store/i/${id}/etc`,
      cur__doc: `${p.projectPath}/_esy/default/store/i/${id}/doc`,
      cur__bin: `${p.projectPath}/_esy/default/store/i/${id}/bin`,
      PATH: [
        `${p.esyStorePath}/i/${depId}/bin`,
        ``,
        `/usr/local/bin`,
        `/usr/bin`,
        `/bin`,
        `/usr/sbin`,
        `/sbin`,
      ].join(path.delimiter),
      OCAMLFIND_CONF: `${p.projectPath}/_esy/default/store/p/${id}/etc/findlib.conf`,
      DUNE_BUILD_DIR: `${p.projectPath}/_esy/default/store/b/${id}`,
    });
  });

  test('command-env', async function() {
    const p = await createTestSandbox();
    const id = JSON.parse((await p.esy('build-plan')).stdout).id;
    const depId = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
    const devDepId = JSON.parse((await p.esy('build-plan -p devDep')).stdout).id;
    const depOfDevDepId = JSON.parse((await p.esy('build-plan -p depOfDevDep')).stdout)
      .id;

    const {stdout} = await p.esy('command-env --json');
    const env = JSON.parse(stdout);

    const PATH = env.PATH.split(path.delimiter);

    expect(PATH).toContain(`${p.esyStorePath}/i/${depId}/bin`);
    expect(PATH).toContain(`${p.esyStorePath}/i/${devDepId}/bin`);
    expect(PATH).toContain(`${p.esyStorePath}/i/${depOfDevDepId}/bin`);

    expect(env).toMatchObject({
      cur__version: '1.0.0',
      cur__toplevel: `${p.projectPath}/_esy/default/store/i/${id}/toplevel`,
      cur__target_dir: `${p.projectPath}/_esy/default/store/b/${id}`,
      cur__stublibs: `${p.projectPath}/_esy/default/store/i/${id}/stublibs`,
      cur__share: `${p.projectPath}/_esy/default/store/i/${id}/share`,
      cur__sbin: `${p.projectPath}/_esy/default/store/i/${id}/sbin`,
      cur__root: `${p.projectPath}`,
      cur__original_root: `${p.projectPath}`,
      cur__name: `withDevDep`,
      cur__man: `${p.projectPath}/_esy/default/store/i/${id}/man`,
      cur__lib: `${p.projectPath}/_esy/default/store/i/${id}/lib`,
      cur__install: `${p.projectPath}/_esy/default/store/i/${id}`,
      cur__etc: `${p.projectPath}/_esy/default/store/i/${id}/etc`,
      cur__doc: `${p.projectPath}/_esy/default/store/i/${id}/doc`,
      cur__bin: `${p.projectPath}/_esy/default/store/i/${id}/bin`,
      OCAMLFIND_CONF: `${p.projectPath}/_esy/default/store/p/${id}/etc/findlib.conf`,
      DUNE_BUILD_DIR: `${p.projectPath}/_esy/default/store/b/${id}`,
    });
  });

  test.enableIf(isMacos || isLinux)(
    'macos || linux: build-env snapshot',
    async function() {
      const p = await createTestSandbox();
      const id = JSON.parse((await p.esy('build-plan')).stdout).id;
      const depid = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
      const devdepid = JSON.parse((await p.esy('build-plan -p devDep')).stdout).id;
      const depofdevdepid = JSON.parse((await p.esy('build-plan -p depOfDevDep')).stdout)
        .id;
      const {stdout} = await p.esy('build-env');
      expect(
        p.normalizePathsForSnapshot(stdout, {id, depid, devdepid, depofdevdepid}),
      ).toMatchSnapshot();
    },
  );

  test('build-env dep', async function() {
    const p = await createTestSandbox();
    const depId = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;

    const {stdout} = await p.esy('build-env --json -p dep');
    const env = JSON.parse(stdout);
    expect(env).toMatchObject({
      cur__version: '1.0.0',
      cur__toplevel: `${p.esyStorePath}/s/${depId}/toplevel`,
      cur__target_dir: `${p.esyPrefixPath}/3/b/${depId}`,
      cur__stublibs: `${p.esyStorePath}/s/${depId}/stublibs`,
      cur__share: `${p.esyStorePath}/s/${depId}/share`,
      cur__sbin: `${p.esyStorePath}/s/${depId}/sbin`,
      cur__name: `dep`,
      cur__man: `${p.esyStorePath}/s/${depId}/man`,
      cur__lib: `${p.esyStorePath}/s/${depId}/lib`,
      cur__install: `${p.esyStorePath}/s/${depId}`,
      cur__etc: `${p.esyStorePath}/s/${depId}/etc`,
      cur__doc: `${p.esyStorePath}/s/${depId}/doc`,
      cur__bin: `${p.esyStorePath}/s/${depId}/bin`,
      PATH: [``, `/usr/local/bin`, `/usr/bin`, `/bin`, `/usr/sbin`, `/sbin`].join(
        path.delimiter,
      ),
    });
  });

  test.enableIf(isMacos || isLinux)(
    'macos || linux: build-env dep snapshot',
    async function() {
      const p = await createTestSandbox();
      const id = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
      const {stdout} = await p.esy('build-env -p dep');
      expect(p.normalizePathsForSnapshot(stdout, {id})).toMatchSnapshot();
    },
  );

  test('build-env devDep', async function() {
    const p = await createTestSandbox();
    const devDepId = JSON.parse((await p.esy('build-plan -p devDep')).stdout).id;
    const depOfDevDepId = JSON.parse((await p.esy('build-plan -p depOfDevDep')).stdout)
      .id;

    const {stdout} = await p.esy('build-env --json -p devDep');
    const env = JSON.parse(stdout);
    expect(env).toMatchObject({
      cur__version: '1.0.0',
      cur__toplevel: `${p.esyStorePath}/s/${devDepId}/toplevel`,
      cur__target_dir: `${p.esyPrefixPath}/3/b/${devDepId}`,
      cur__stublibs: `${p.esyStorePath}/s/${devDepId}/stublibs`,
      cur__share: `${p.esyStorePath}/s/${devDepId}/share`,
      cur__sbin: `${p.esyStorePath}/s/${devDepId}/sbin`,
      cur__name: `devDep`,
      cur__man: `${p.esyStorePath}/s/${devDepId}/man`,
      cur__lib: `${p.esyStorePath}/s/${devDepId}/lib`,
      cur__install: `${p.esyStorePath}/s/${devDepId}`,
      cur__etc: `${p.esyStorePath}/s/${devDepId}/etc`,
      cur__doc: `${p.esyStorePath}/s/${devDepId}/doc`,
      cur__bin: `${p.esyStorePath}/s/${devDepId}/bin`,
      PATH: [
        `${p.esyStorePath}/i/${depOfDevDepId}/bin`,
        ``,
        `/usr/local/bin`,
        `/usr/bin`,
        `/bin`,
        `/usr/sbin`,
        `/sbin`,
      ].join(path.delimiter),
    });
  });

  test.enableIf(isMacos || isLinux)(
    'macos || linux: build-env devDep snapshot',
    async function() {
      const p = await createTestSandbox();
      const id = JSON.parse((await p.esy('build-plan -p devDep')).stdout).id;
      const depOfDevDepId = JSON.parse((await p.esy('build-plan -p depOfDevDep')).stdout)
        .id;
      const {stdout} = await p.esy('build-env -p devDep');
      expect(p.normalizePathsForSnapshot(stdout, {id, depOfDevDepId})).toMatchSnapshot();
    },
  );

  test('exec-env', async function() {
    const p = await createTestSandbox();
    const id = JSON.parse((await p.esy('build-plan')).stdout).id;
    const depId = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
    const {stdout} = await p.esy('exec-env --json');
    const envpath = JSON.parse(stdout).PATH.split(path.delimiter);
    expect(
      envpath.includes(`${p.projectPath}/_esy/default/store/i/${id}/bin`),
    ).toBeTruthy();
    expect(envpath.includes(`${p.esyStorePath}/i/${depId}/bin`)).toBeTruthy();
  });

  test('command-env', async function() {
    const p = await createTestSandbox();
    const id = JSON.parse((await p.esy('build-plan')).stdout).id;
    const depId = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
    const devDepId = JSON.parse((await p.esy('build-plan -p devDep')).stdout).id;
    const {stdout} = await p.esy('command-env --json');
    const env = JSON.parse(stdout);
    expect(env).toMatchObject({
      cur__version: '1.0.0',
      cur__toplevel: `${p.projectPath}/_esy/default/store/i/${id}/toplevel`,
      cur__target_dir: `${p.projectPath}/_esy/default/store/b/${id}`,
      cur__stublibs: `${p.projectPath}/_esy/default/store/i/${id}/stublibs`,
      cur__share: `${p.projectPath}/_esy/default/store/i/${id}/share`,
      cur__sbin: `${p.projectPath}/_esy/default/store/i/${id}/sbin`,
      cur__root: `${p.projectPath}`,
      cur__original_root: `${p.projectPath}`,
      cur__name: `withDevDep`,
      cur__man: `${p.projectPath}/_esy/default/store/i/${id}/man`,
      cur__lib: `${p.projectPath}/_esy/default/store/i/${id}/lib`,
      cur__install: `${p.projectPath}/_esy/default/store/i/${id}`,
      cur__etc: `${p.projectPath}/_esy/default/store/i/${id}/etc`,
      cur__doc: `${p.projectPath}/_esy/default/store/i/${id}/doc`,
      cur__bin: `${p.projectPath}/_esy/default/store/i/${id}/bin`,
      DUNE_BUILD_DIR: `${p.projectPath}/_esy/default/store/b/${id}`,
    });
    const envpath = env.PATH.split(path.delimiter);
    expect(envpath.includes(`${p.esyStorePath}/i/${depId}/bin`)).toBeTruthy();
    expect(envpath.includes(`${p.esyStorePath}/i/${devDepId}/bin`)).toBeTruthy();
  });
});

describe('Project with "devDependencies" (with "buildDev" config at the root)', () => {
  async function createTestSandbox() {
    const p = await helpers.createTestSandbox();
    const name = 'withDevDep';
    await p.fixture(
      helpers.packageJson({
        name: name,
        version: '1.0.0',
        esy: {
          build: [
            'cp #{self.name}.js #{self.target_dir / self.name}.js',
            helpers.buildCommand(p, '#{self.target_dir / self.name}.js'),
          ],
          // run commands from "devDependencies" here
          buildDev: [
            'devDep.cmd',
            'cp #{self.name}-dev.js #{self.target_dir / self.name}.js',
            helpers.buildCommand(p, '#{self.target_dir / self.name}.js'),
          ],
          install: [
            `cp #{self.target_dir / self.name}.cmd #{self.bin / self.name}.cmd`,
            `cp #{self.target_dir / self.name}.js #{self.bin / self.name}.js`,
          ],
        },
        dependencies: {
          dep: 'path:./dep',
        },
        devDependencies: {
          devDep: 'path:./devDep',
        },
      }),
      helpers.dummyExecutable(name),
      helpers.dummyExecutable(`${name}-dev`),
      makePackage(p, {
        name: 'dep',
        devDependencies: {devDepOfDep: '*'},
      }),
      makePackage(p, {
        name: 'devDep',
        dependencies: {
          depOfDevDep: 'path:../depOfDevDep',
        },
      }),
      makePackage(p, {
        name: 'depOfDevDep',
      }),
    );
    await p.esy('install');
    return p;
  }

  it(`can be built with 'esy build'`, async () => {
    const p = await createTestSandbox();

    {
      const {stdout} = await p.esy('build');
      expect(stdout).toBe(`__devDep__${os.EOL}`);
    }

    {
      const {stdout} = await p.esy('x withDevDep.cmd');
      expect(stdout).toBe(`__devDep__${os.EOL}__withDevDep-dev__${os.EOL}`);
    }
  });

  it(`can be built with 'esy build --release'`, async () => {
    const p = await createTestSandbox();

    {
      const {stdout} = await p.esy('build --release');
      expect(stdout).toBe(``);
    }

    {
      const {stdout} = await p.esy('x --release withDevDep.cmd');
      expect(stdout).toBe(`__withDevDep__${os.EOL}`);
    }
  });
});
