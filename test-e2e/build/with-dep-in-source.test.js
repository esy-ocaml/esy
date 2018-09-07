// @flow

const path = require('path');

const {
  createTestSandbox,
  packageJson,
  dir,
  file,
  ocamlPackage,
  exeExtension,
} = require('../test/helpers');

const fixture = [
  packageJson({
    name: 'with-dep-in-source',
    version: '1.0.0',
    esy: {
      build: 'true',
    },
    dependencies: {
      dep: '*',
    },
  }),
  dir(
    'node_modules',
    dir(
      'dep',
      packageJson({
        name: 'dep',
        version: '1.0.0',
        esy: {
          buildsInSource: true,
          build: 'ocamlopt -o #{self.root / self.name}.exe #{self.root / self.name}.ml',
          install: `cp #{self.root / self.name}.exe #{self.bin / self.name}${exeExtension}`,
        },
        dependencies: {
          ocaml: '*',
        },
        '_esy.source': 'path:./',
      }),
      file('dep.ml', 'let () = print_endline "__dep__"'),
    ),
    ocamlPackage(),
  ),
];

describe('Build - with dep in source', () => {
  it('package "dep" should be visible in all envs', async () => {
    const p = await createTestSandbox(...fixture);
    await p.esy('build');

    const expecting = expect.stringMatching('__dep__');

    const dep = await p.esy('dep');
    expect(dep.stdout).toEqual(expecting);

    const b = await p.esy('b dep');
    expect(b.stdout).toEqual(expecting);

    const x = await p.esy('x dep');
    expect(x.stdout).toEqual(expecting);
  });
});
