// @flow

const helpers = require('../test/helpers');

function makeFixture(p, buildDep) {
  return [
    helpers.packageJson({
      name: 'no-deps',
      version: '1.0.0',
      esy: buildDep,
    }),
    helpers.dummyExecutable('no-deps', 'js'),
  ];
}

describe('Build simple executable with no deps', () => {
  let p;

  async function checkIsInEnv() {
    const {stdout} = await p.esy('x no-deps.cmd');
    expect(stdout.trim()).toEqual('__no-deps__');
  }

  describe('out of source build', () => {
    beforeAll(async () => {
      p = await helpers.createTestSandbox();
      p.fixture(
        ...makeFixture(p, {
          build: [
            ['cp', '#{self.name}.js', '#{self.target_dir / self.name}.js'],
            helpers.buildCommand('#{self.target_dir / self.name}.js'),
          ],
          install: [
            `cp #{self.target_dir / self.name}.cmd #{self.bin / self.name}.cmd`,
            `cp #{self.target_dir / self.name}.js #{self.bin / self.name}.js`,
          ],
        }),
      );
      await p.esy('build');
    });
    test('executable is available in sandbox env', checkIsInEnv);
  });

  describe('in source build', () => {
    beforeAll(async () => {
      p = await helpers.createTestSandbox();
      p.fixture(
        ...makeFixture(p, {
          buildsInSource: true,
          build: [helpers.buildCommand('#{self.name}.js')],
          install: [
            `cp #{self.name}.cmd #{self.bin / self.name}.cmd`,
            `cp #{self.name}.js #{self.bin / self.name}.js`,
          ],
        }),
      );
      await p.esy('build');
    });
    test('executable is available in sandbox env', checkIsInEnv);
  });

  describe('_build build', () => {
    beforeAll(async () => {
      p = await helpers.createTestSandbox();
      p.fixture(
        ...makeFixture(p, {
          buildsInSource: '_build',
          build: [
            'mkdir -p _build',
            'cp #{self.name}.js _build/#{self.name}.js',
            helpers.buildCommand('_build/#{self.name}.js'),
          ],
          install: [
            `cp _build/#{self.name}.cmd #{self.bin / self.name}.cmd`,
            `cp _build/#{self.name}.js #{self.bin / self.name}.js`,
          ],
        }),
      );
      await p.esy('build');
    });
    test('executable is available in sandbox env', checkIsInEnv);
  });
});
