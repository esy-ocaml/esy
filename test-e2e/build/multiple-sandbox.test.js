// @flow

const helpers = require('../test/helpers.js');

const {file, dir, packageJson} = helpers;

helpers.skipSuiteOnWindows();

function makePackage(
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
  return dir(
    name,
    packageJson({
      name: name,
      version: '1.0.0',
      license: 'MIT',
      esy: {
        buildsInSource: true,
        build: helpers.buildCommand('#{self.name}.js'),
        install: [
          `cp #{self.name}.cmd #{self.bin / self.name}.cmd`,
          `cp #{self.name}.js #{self.bin / self.name}.js`,
        ],
      },
      dependencies,
      devDependencies,
      '_esy.source': 'path:./',
    }),
    helpers.dummyExecutable(name),
    ...items,
  );
}

describe('Projects with multiple sandboxes', function() {
  it('can build multiple sandboxes', async () => {
    const fixture = [
      file(
        'package.json',
        `
        {
          "esy": {},
          "dependencies": {"default-dep": "*"}
        }
        `,
      ),
      file(
        'package.custom.json',
        `
        {
          "esy": {},
          "dependencies": {"custom-dep": "*"}
        }
        `,
      ),
      dir(
        '_esy',
        dir(['default', 'node_modules'], makePackage({name: 'default-dep'})),
        dir(['package.custom', 'node_modules'], makePackage({name: 'custom-dep'})),
      ),
    ];

    const p = await helpers.createTestSandbox(...fixture);

    await p.esy('build');

    {
      const {stdout} = await p.esy('default-dep.cmd');
      expect(stdout.trim()).toBe('__default-dep__');
    }

    expect(p.esy('custom-dep')).rejects.toThrow();

    await p.esy('@package.custom.json build');

    {
      const {stdout} = await p.esy('@package.custom.json custom-dep.cmd');
      expect(stdout.trim()).toBe('__custom-dep__');
    }

    expect(p.esy('@package.custom.json default-dep.cmd')).rejects.toThrow();

    {
      // .json extension could be dropped
      const {stdout} = await p.esy('@package.custom custom-dep.cmd');
      expect(stdout.trim()).toBe('__custom-dep__');
    }
  });
});
