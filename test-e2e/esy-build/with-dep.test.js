// @flow

const path = require('path');
const helpers = require('../test/helpers');
const {test, isWindows, isMacos, isLinux} = helpers;

function makeFixture(p, buildDep) {
  return [
    helpers.packageJson({
      name: 'withDep',
      version: '1.0.0',
      esy: {
        build: 'true',
      },
      dependencies: {
        dep: 'path:./dep',
      },
    }),
    helpers.dir(
      'dep',
      helpers.packageJson({
        name: 'dep',
        version: '1.0.0',
        esy: {
          ...buildDep,
          exportedEnv: {
            dep__local: {val: 'dep__local'},
            dep__global: {val: 'dep__global', scope: 'global'},
            dep__local_dyn: {val: 'dep__local_dyn:$cur__name'},
            dep__global_dyn: {val: 'dep__global_dyn:$cur__name', scope: 'global'},
          },
        },
        dependencies: {
          depOfDep: 'path:../depOfDep',
        },
      }),
      helpers.dummyExecutable('dep'),
    ),
    helpers.dir(
      'depOfDep',
      helpers.packageJson({
        name: 'depOfDep',
        version: '1.0.0',
        esy: {
          build: 'true',
          exportedEnv: {
            depOfDep__local: {val: 'depOfDep__local'},
            depOfDep__global: {val: 'depOfDep__global', scope: 'global'},
            depOfDep__local_dyn: {val: 'depOfDep__local_dyn:$cur__name'},
            depOfDep__global_dyn: {
              val: 'depOfDep__global_dyn:$cur__name',
              scope: 'global',
            },
          },
        },
      }),
      helpers.dummyExecutable('dep'),
    ),
  ];
}

describe('Build with dep', () => {
  let winsysDir =
    process.platform === 'win32' ? [helpers.getWindowsSystemDirectory()] : [];

  describe('out of source build', () => {
    function withProject(assertions) {
      return async () => {
        const p = await helpers.createTestSandbox();
        await p.fixture(
          ...makeFixture(p, {
            build: [
              'cp #{self.root / self.name}.js #{self.target_dir / self.name}.js',
              helpers.buildCommand(p, '#{self.target_dir / self.name}.js'),
            ],
            install: [
              `cp #{self.target_dir / self.name}.cmd #{self.bin / self.name}.cmd`,
              `cp #{self.target_dir / self.name}.js #{self.bin / self.name}.js`,
            ],
          }),
        );
        await p.esy('install');
        await p.esy('build');
        await assertions(p);
      };
    }

    it(
      'makes dep available in envs',
      withProject(async (p) => {
        {
          const {stdout} = await p.esy('dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }

        {
          const {stdout} = await p.esy('b dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }

        {
          const {stdout} = await p.esy('x dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }
      }),
    );

    test(
      'build-env',
      withProject(async function (p) {
        const id = JSON.parse((await p.esy('build-plan')).stdout).id;
        const depId = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
        const depOfDepId = JSON.parse((await p.esy('build-plan -p depOfDep')).stdout).id;

        const {stdout} = await p.esy('build-env --json');
        const env = JSON.parse(stdout);
        expect(env).toMatchObject({
          // exports
          dep__local: 'dep__local',
          dep__global: 'dep__global',
          dep__local_dyn: 'dep__local_dyn:withDep',
          dep__global_dyn: 'dep__global_dyn:withDep',
          depOfDep__global: 'depOfDep__global',
          depOfDep__global_dyn: 'depOfDep__global_dyn:withDep',
          //built ins (cur)
          cur__version: '1.0.0',
          cur__toplevel: `${p.projectPath}/_esy/default/store/i/${id}/toplevel`,
          cur__target_dir: `${p.projectPath}/_esy/default/store/b/${id}`,
          cur__stublibs: `${p.projectPath}/_esy/default/store/i/${id}/stublibs`,
          cur__share: `${p.projectPath}/_esy/default/store/i/${id}/share`,
          cur__sbin: `${p.projectPath}/_esy/default/store/i/${id}/sbin`,
          cur__root: `${p.projectPath}`,
          cur__original_root: `${p.projectPath}`,
          cur__name: `withDep`,
          cur__man: `${p.projectPath}/_esy/default/store/i/${id}/man`,
          cur__lib: `${p.projectPath}/_esy/default/store/i/${id}/lib`,
          cur__install: `${p.projectPath}/_esy/default/store/i/${id}`,
          cur__etc: `${p.projectPath}/_esy/default/store/i/${id}/etc`,
          cur__doc: `${p.projectPath}/_esy/default/store/i/${id}/doc`,
          cur__bin: `${p.projectPath}/_esy/default/store/i/${id}/bin`,
          // built ins
          PATH: [
            `${p.esyStorePath}/i/${depId}/bin`,
            `${p.esyStorePath}/i/${depOfDepId}/bin`,
            ``,
            `/usr/local/bin`,
            `/usr/bin`,
            `/bin`,
            `/usr/sbin`,
            `/sbin`,
            ...winsysDir,
          ].join(path.delimiter),
          OCAMLFIND_CONF: `${p.projectPath}/_esy/default/store/p/${id}/etc/findlib.conf`,
          DUNE_BUILD_DIR: `${p.projectPath}/_esy/default/store/b/${id}`,
        });
      }),
    );

    test.enableIf(isMacos || isLinux)(
      'macos || linux: build-env snapshot',
      withProject(async function (p) {
        const id = JSON.parse((await p.esy('build-plan')).stdout).id;
        const depId = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
        const depOfDepId = JSON.parse((await p.esy('build-plan -p depOfDep')).stdout).id;
        const {stdout} = await p.esy('build-env');
        expect(
          p.normalizePathsForSnapshot(stdout, {id, depId, depOfDepId}),
        ).toMatchSnapshot();
      }),
    );

    test(
      'build-env dep',
      withProject(async function (p) {
        const id = JSON.parse((await p.esy('build-plan')).stdout).id;
        const depId = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
        const depOfDepId = JSON.parse((await p.esy('build-plan -p depOfDep')).stdout).id;

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
          PATH: [
            `${p.esyStorePath}/i/${depOfDepId}/bin`,
            ``,
            `/usr/local/bin`,
            `/usr/bin`,
            `/bin`,
            `/usr/sbin`,
            `/sbin`,
            ...winsysDir,
          ].join(path.delimiter),
          OCAMLFIND_CONF: `${p.esyStorePath}/p/${depId}/etc/findlib.conf`,
          DUNE_BUILD_DIR: `${p.esyPrefixPath}/3/b/${depId}`,
        });
      }),
    );

    test.enableIf(isMacos || isLinux)(
      'macos || linux: build-env dep snapshot',
      withProject(async function (p) {
        const id = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
        const depOfDepId = JSON.parse((await p.esy('build-plan -p depOfDep')).stdout).id;
        const {stdout} = await p.esy('build-env -p dep');
        expect(p.normalizePathsForSnapshot(stdout, {id, depOfDepId})).toMatchSnapshot();
      }),
    );

    test(
      'exec-env',
      withProject(async function (p) {
        const id = JSON.parse((await p.esy('build-plan')).stdout).id;
        const depId = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
        const {stdout} = await p.esy('exec-env --json');
        const envpath = JSON.parse(stdout).PATH.split(path.delimiter);
        expect(
          envpath.includes(`${p.projectPath}/_esy/default/store/i/${id}/bin`),
        ).toBeTruthy();
        expect(envpath.includes(`${p.esyStorePath}/i/${depId}/bin`)).toBeTruthy();
      }),
    );

    test(
      'command-env',
      withProject(async function (p) {
        const id = JSON.parse((await p.esy('build-plan')).stdout).id;
        const depId = JSON.parse((await p.esy('build-plan -p dep')).stdout).id;
        const depOfDepId = JSON.parse((await p.esy('build-plan -p depOfDep')).stdout).id;
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
          cur__name: `withDep`,
          cur__man: `${p.projectPath}/_esy/default/store/i/${id}/man`,
          cur__lib: `${p.projectPath}/_esy/default/store/i/${id}/lib`,
          cur__install: `${p.projectPath}/_esy/default/store/i/${id}`,
          cur__etc: `${p.projectPath}/_esy/default/store/i/${id}/etc`,
          cur__doc: `${p.projectPath}/_esy/default/store/i/${id}/doc`,
          cur__bin: `${p.projectPath}/_esy/default/store/i/${id}/bin`,
          OCAMLFIND_CONF: `${p.projectPath}/_esy/default/store/p/${id}/etc/findlib.conf`,
          DUNE_BUILD_DIR: `${p.projectPath}/_esy/default/store/b/${id}`,
        });
        const envpath = env.PATH.split(path.delimiter);
        expect(envpath.includes(`${p.esyStorePath}/i/${depId}/bin`)).toBeTruthy();
        expect(envpath.includes(`${p.esyStorePath}/i/${depOfDepId}/bin`)).toBeTruthy();
      }),
    );
  });

  describe('in source build', () => {
    function withProject(assertions) {
      return async () => {
        const p = await helpers.createTestSandbox();
        await p.fixture(
          ...makeFixture(p, {
            buildsInSource: true,
            build: [helpers.buildCommand(p, '#{self.root / self.name}.js')],
            install: [
              `cp #{self.root / self.name}.cmd #{self.bin / self.name}.cmd`,
              `cp #{self.root / self.name}.js #{self.bin / self.name}.js`,
            ],
          }),
        );
        await p.esy('install');
        await p.esy('build');
        await assertions(p);
      };
    }

    it(
      'makes dep available in envs',
      withProject(async (p) => {
        {
          const {stdout} = await p.esy('dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }

        {
          const {stdout} = await p.esy('b dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }

        {
          const {stdout} = await p.esy('x dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }
      }),
    );
  });

  describe('_build build', () => {
    function withProject(assertions) {
      return async () => {
        const p = await helpers.createTestSandbox();
        await p.fixture(
          ...makeFixture(p, {
            buildsInSource: '_build',
            build: [
              "mkdir -p #{self.root / '_build'}",
              "cp #{self.root / self.name}.js #{self.root / '_build' / self.name}.js",
              helpers.buildCommand(p, "#{self.root / '_build' / self.name}.js"),
            ],
            install: [
              `cp #{self.root / '_build' / self.name}.cmd #{self.bin / self.name}.cmd`,
              `cp #{self.root / '_build' / self.name}.js #{self.bin / self.name}.js`,
            ],
          }),
        );
        await p.esy('install');
        await p.esy('build');
        await assertions(p);
      };
    }
    it(
      'makes dep available in envs',
      withProject(async (p) => {
        {
          const {stdout} = await p.esy('dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }

        {
          const {stdout} = await p.esy('b dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }

        {
          const {stdout} = await p.esy('x dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }
      }),
    );
  });

  describe('out of source build with buildDev in deps', () => {
    function withProject(assertions) {
      return async () => {
        const p = await helpers.createTestSandbox();
        await p.fixture(
          ...makeFixture(p, {
            build: [
              'cp #{self.root / self.name}.js #{self.target_dir / self.name}.js',
              helpers.buildCommand(p, '#{self.target_dir / self.name}.js'),
            ],
            // set buildDev to false to make sure we fail if it's going to be
            // executed
            buildDev: 'false',
            install: [
              `cp #{self.target_dir / self.name}.cmd #{self.bin / self.name}.cmd`,
              `cp #{self.target_dir / self.name}.js #{self.bin / self.name}.js`,
            ],
          }),
        );
        await p.esy('install');
        await p.esy('build');
        await assertions(p);
      };
    }

    it(
      'makes dep available in envs',
      withProject(async (p) => {
        {
          const {stdout} = await p.esy('dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }

        {
          const {stdout} = await p.esy('b dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }

        {
          const {stdout} = await p.esy('x dep.cmd');
          expect(stdout.trim()).toEqual('__dep__');
        }
      }),
    );
  });
});
