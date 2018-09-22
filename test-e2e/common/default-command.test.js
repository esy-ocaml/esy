// @flow

const helpers = require('../test/helpers');
const {file, dir, packageJson, dummyExecutable} = helpers;

helpers.skipSuiteOnWindows('Needs esyi to work');

const fixture = [
  packageJson({
    name: 'default-command',
    version: '1.0.0',
    esy: {
      build: 'true',
    },
    dependencies: {
      dep: 'link:./dep',
    },
  }),
  dir(
    'dep',
    packageJson({
      name: 'dep',
      version: '1.0.0',
      esy: {
        build: [
          [
            'cp',
            '#{self.original_root / self.name}.js',
            '#{self.target_dir / self.name}.js',
          ],
          helpers.buildCommand('#{self.target_dir / self.name}.js'),
        ],
        install: [
          ['cp', '#{self.target_dir / self.name}.cmd', '#{self.bin / self.name}.cmd'],
          ['cp', '#{self.target_dir / self.name}.js', '#{self.bin / self.name}.js'],
        ],
      },
      '_esy.source': 'path:./',
    }),
    dummyExecutable('dep'),
  ),
];

it('Build - default command', async () => {
  let p = await helpers.createTestSandbox(...fixture);
  await p.esy();

  const dep = await p.esy('dep.cmd');

  expect(dep.stdout.trim()).toEqual('__dep__');
});
