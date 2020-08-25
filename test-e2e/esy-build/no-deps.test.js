// @flow

const path = require('path');
const helpers = require('../test/helpers');
const {test, isWindows, isMacos, isLinux} = helpers;

async function fixture(p, buildDep) {
  const fixture = [
    helpers.packageJson({
      name: 'no-deps',
      version: '1.0.0',
      esy: buildDep,
    }),
    helpers.dummyExecutable('no-deps'),
    helpers.dummyExecutable('no-deps-dev'),
  ];
  await p.fixture(...fixture);
}

describe(`'esy build': simple executable with no deps`, () => {
  describe('out of source build', () => {

    function withProject(assertions) {
      return async () => {
        const p = await helpers.createTestSandbox();
        await fixture(p, {
          build: [
            ['cp', '#{self.name}.js', '#{self.target_dir / self.name}.js'],
            helpers.buildCommand(p, '#{self.target_dir / self.name}.js'),
          ],
          install: [
            `cp #{self.target_dir / self.name}.cmd #{self.bin / self.name}.cmd`,
            `cp #{self.target_dir / self.name}.js #{self.bin / self.name}.js`,
          ],
        });
        await p.esy('install');
        await assertions(p);
      };
    }

    test('executable is available in sandbox env', withProject(async (p) => {
      await p.esy('build');
      const {stdout} = await p.esy('x no-deps.cmd');
      expect(stdout.trim()).toEqual('__no-deps__');
    }));

    test.disableIf(isWindows)('passing --install installs built artifacts', withProject(async (p) => {
      await p.esy('build --install');
      const {stdout} = await p.esy("'#{self.bin}/no-deps.cmd'");
      expect(stdout.trim()).toEqual('__no-deps__');
    }));

    test.disableIf(isWindows)('produces _esy/*/build link to #{self.target_dir} for root', withProject(async (p) => {
      await p.esy('build');
      const {stdout} = await p.run("./_esy/default/build/no-deps.cmd");
      expect(stdout.trim()).toEqual('__no-deps__');
    }));

    test.disableIf(isWindows)('produces _esy/*/install link to #{self.install} for root', withProject(async (p) => {
      await p.esy('build --install');
      const {stdout} = await p.run("./_esy/default/install/bin/no-deps.cmd");
      expect(stdout.trim()).toEqual('__no-deps__');
    }));

    test.disableIf(isWindows)(
      'produces _esy/*/build-release link to #{self.target_dir} for root --release',
      withProject(async (p) => {
        await p.esy('build --release');
        const {stdout} = await p.run("./_esy/default/build-release/no-deps.cmd");
        expect(stdout.trim()).toEqual('__no-deps__');
      }));

    test.disableIf(isWindows)(
      'produces _esy/*/install-release link to #{self.install} for root on --release',
      withProject(async (p) => {
        await p.esy('build --release --install');
        const {stdout} = await p.run("./_esy/default/install-release/bin/no-deps.cmd");
        expect(stdout.trim()).toEqual('__no-deps__');
      }));

    test(
      'build-env',
      withProject(async function(p) {
        await p.esy('build');
        let winsysDir =
          process.platform === 'win32' ? [helpers.getWindowsSystemDirectory()] : [];
        const id = JSON.parse((await p.esy('build-plan')).stdout).id;
        const {stdout} = await p.esy('build-env --json');
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
          cur__name: `no-deps`,
          cur__man: `${p.projectPath}/_esy/default/store/i/${id}/man`,
          cur__lib: `${p.projectPath}/_esy/default/store/i/${id}/lib`,
          cur__install: `${p.projectPath}/_esy/default/store/i/${id}`,
          cur__etc: `${p.projectPath}/_esy/default/store/i/${id}/etc`,
          cur__doc: `${p.projectPath}/_esy/default/store/i/${id}/doc`,
          cur__bin: `${p.projectPath}/_esy/default/store/i/${id}/bin`,
          PATH: [
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
      withProject(async function(p) {
        await p.esy('build');
        const id = JSON.parse((await p.esy('build-plan')).stdout).id;
        const {stdout} = await p.esy('build-env');
        expect(p.normalizePathsForSnapshot(stdout, {id: id})).toMatchSnapshot();
      }),
    );

    test(
      'exec-env',
      withProject(async function(p) {
        await p.esy('build');
        const id = JSON.parse((await p.esy('build-plan')).stdout).id;
        const {stdout} = await p.esy('exec-env --json');
        const envpath = JSON.parse(stdout).PATH.split(path.delimiter);
        expect(
          envpath.includes(`${p.projectPath}/_esy/default/store/i/${id}/bin`),
        ).toBeTruthy();
      }),
    );

    test(
      'command-env',
      withProject(async function(p) {
        await p.esy('build');
        const id = JSON.parse((await p.esy('build-plan')).stdout).id;
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
          cur__name: `no-deps`,
          cur__man: `${p.projectPath}/_esy/default/store/i/${id}/man`,
          cur__lib: `${p.projectPath}/_esy/default/store/i/${id}/lib`,
          cur__install: `${p.projectPath}/_esy/default/store/i/${id}`,
          cur__etc: `${p.projectPath}/_esy/default/store/i/${id}/etc`,
          cur__doc: `${p.projectPath}/_esy/default/store/i/${id}/doc`,
          cur__bin: `${p.projectPath}/_esy/default/store/i/${id}/bin`,
          OCAMLFIND_CONF: `${p.projectPath}/_esy/default/store/p/${id}/etc/findlib.conf`,
          DUNE_BUILD_DIR: `${p.projectPath}/_esy/default/store/b/${id}`,
        });
      }),
    );
  });

  describe('in source build', () => {
    function withProject(assertions) {
      return async () => {
        const p = await helpers.createTestSandbox();
        await fixture(p, {
          buildsInSource: true,
          build: [helpers.buildCommand(p, '#{self.name}.js')],
          install: [
            `cp #{self.name}.cmd #{self.bin / self.name}.cmd`,
            `cp #{self.name}.js #{self.bin / self.name}.js`,
          ],
        });
        await p.esy('install');
        await p.esy('build');
        await assertions(p);
      };
    }
    test('executable is available in sandbox env', withProject(async (p) => {
      await p.esy('build');
      const {stdout} = await p.esy('x no-deps.cmd');
      expect(stdout.trim()).toEqual('__no-deps__');
    }));
  });

  describe('_build build', () => {
    function withProject(assertions) {
      return async () => {
        const p = await helpers.createTestSandbox();
        await fixture(p, {
          buildsInSource: '_build',
          build: [
            'mkdir -p _build',
            'cp #{self.name}.js _build/#{self.name}.js',
            helpers.buildCommand(p, '_build/#{self.name}.js'),
          ],
          install: [
            `cp _build/#{self.name}.cmd #{self.bin / self.name}.cmd`,
            `cp _build/#{self.name}.js #{self.bin / self.name}.js`,
          ],
        });
        await p.esy('install');
        await p.esy('build');
        await assertions(p);
      };
    }
    test('executable is available in sandbox env', withProject(async (p) => {
      await p.esy('build');
      const {stdout} = await p.esy('x no-deps.cmd');
      expect(stdout.trim()).toEqual('__no-deps__');
    }));
  });

  describe('out of source build with buildDev', () => {

    async function createTestSandbox() {
      const p = await helpers.createTestSandbox();
      await p.fixture(
        helpers.packageJson({
          name: 'no-deps',
          version: '1.0.0',
          esy: {
            build: [
              ['cp', '#{self.name}.js', '#{self.target_dir / self.name}.js'],
              helpers.buildCommand(p, '#{self.target_dir / self.name}.js'),
            ],
            buildDev: [
              ['cp', '#{self.name}-dev.js', '#{self.target_dir / self.name}.js'],
              helpers.buildCommand(p, '#{self.target_dir / self.name}.js'),
            ],
            install: [
              `cp #{self.target_dir / self.name}.cmd #{self.bin / self.name}.cmd`,
              `cp #{self.target_dir / self.name}.js #{self.bin / self.name}.js`,
            ],
          }
        }),
        helpers.dummyExecutable('no-deps'),
        helpers.dummyExecutable('no-deps-dev'),
      );
      await p.esy('install');
      return p;
    }

    test('builds using "buildDev" command if it is set', async () => {
      const p = await createTestSandbox();
      // build will use "buildDev" if it exists by default for the root package
      await p.esy('build');
      const {stdout} = await p.esy('x no-deps.cmd');
      expect(stdout.trim()).toEqual('__no-deps-dev__');
    });

    test('we can force using "build" command by passing --release', async () => {
      const p = await createTestSandbox();
      // we can force to use "build" instead by passing --release flag
      await p.esy('build --release');
      const {stdout} = await p.esy('x --release no-deps.cmd');
      expect(stdout.trim()).toEqual('__no-deps__');
    });

    test.disableIf(isWindows)('both built artifacts are present at the same time', async () => {
      const p = await createTestSandbox();

      // get the path to built executable in release mode
      await p.esy('build --release');
      const {stdout: releaseStdout} = await p.esy('x --release which no-deps.cmd');

      // get the path to built executable in dev mode
      await p.esy('build');
      const {stdout: devStdout} = await p.esy('x which no-deps.cmd');

      // run them directly so we don't trigger builds and thus we make sure we
      // run them build dirs
      {
        const {stdout} = await p.run(releaseStdout.trim());
        expect(stdout.trim()).toEqual('__no-deps__');
      }
      {
        const {stdout} = await p.run(devStdout.trim());
        expect(stdout.trim()).toEqual('__no-deps-dev__');
      }
    });


  });
});
