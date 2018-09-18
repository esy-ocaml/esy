// @flow

const path = require('path');
const fs = require('fs');

const {
  createTestSandbox,
  packageJson,
  file,
  dir,
  symlink,
  ocamlPackage,
} = require('../test/helpers');

const fixture = [
  packageJson({
    name: 'with-linked-dep-in-source',
    version: '1.0.0',
    esy: {
      build: 'true',
    },
    dependencies: {
      dep: '*',
    },
  }),
  dir(
    'dep',
    packageJson({
      name: 'dep',
      version: '1.0.0',
      esy: {
        buildsInSource: true,
        build: 'ocamlopt -o #{self.root / self.name} #{self.root / self.name}.ml',
        install: 'cp #{self.root / self.name} #{self.bin / self.name}',
      },
      dependencies: {
        ocaml: '*',
      },
    }),
    file('dep.ml', 'let () = print_endline "__dep__"'),
  ),
  dir(
    'node_modules',
    dir(
      'dep',
      file('_esylink', '{"path": "./dep"}'),
      symlink('package.json', '../../dep/package.json'),
    ),
    ocamlPackage(),
  ),
];

describe('Build - with linked dep _build', () => {
  it('package "dep" should be visible in all envs', async () => {
    const p = await createTestSandbox(...fixture);

    const expecting = expect.stringMatching('__dep__');

    const dep = await p.esy('dep');
    expect(dep.stdout).toEqual(expecting);

    const b = await p.esy('b dep');
    expect(b.stdout).toEqual(expecting);

    const x = await p.esy('x dep');
    expect(x.stdout).toEqual(expecting);
  });
});
