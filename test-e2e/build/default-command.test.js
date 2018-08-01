// @flow

const outdent = require('outdent');

const {
  genFixture,
  packageJson,
  dir,
  file,
  symlink,
  ocamlPackage,
} = require('../test/helpers');

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
  dir('node_modules', ocamlPackage()),
  dir(
    'dep',
    packageJson({
      name: 'dep',
      version: '1.0.0',
      esy: {
        build: [
          'cp #{self.root / self.name}.ml #{self.target_dir / self.name}.ml',
          'ocamlopt -o #{self.target_dir / self.name} #{self.target_dir / self.name}.ml',
        ],
        install: 'cp #{self.target_dir / self.name} #{self.bin / self.name}',
      },
      _resolved: '...',
    }),
    file('dep.ml', 'let () = print_endline "__dep__"'),
  ),
];

jest.setTimeout(500000);

it('Build - default command', async () => {
  expect.assertions(2);
  let p = await genFixture(...fixture);
  await expect(p.esy()).resolves.not.toThrow();

  const dep = await p.esy('dep');

  expect(dep.stdout).toEqual('__dep__\n');
});
