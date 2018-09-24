// @flow

const path = require('path');
const fs = require('fs-extra');
const tar = require('tar');
const del = require('del');

const helpers = require('../test/helpers');
const {packageJson, dir, file, dummyExecutable} = helpers;

helpers.skipSuiteOnWindows('Needs investigation');

function makeFixture(p) {
  return [
    packageJson({
      name: 'app',
      version: '1.0.0',
      license: 'MIT',
      esy: {
        build: ['ln -s #{dep.bin / dep.name}.cmd #{self.bin / self.name}.cmd'],
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
          license: 'MIT',
          esy: {
            build: ['ln -s #{subdep.bin / subdep.name}.cmd #{self.bin / self.name}.cmd'],
          },
          dependencies: {
            subdep: '*',
          },
        }),
        file('_esylink', JSON.stringify({source: `path:.`})),
        dir(
          'node_modules',
          dir(
            'subdep',
            packageJson({
              name: 'subdep',
              version: '1.0.0',
              license: 'MIT',
              esy: {
                buildsInSource: true,
                build: [helpers.buildCommand(p, '#{self.name}.js')],
                install: [
                  'cp #{self.name}.cmd #{self.bin / self.name}.cmd',
                  'cp #{self.name}.js #{self.bin / self.name}.js',
                ],
              },
            }),
            file('_esylink', JSON.stringify({source: `path:.`})),
            dummyExecutable('subdep'),
          ),
        ),
      ),
    ),
  ];
}

describe('export import build - import app', () => {
  async function createTestSandbox() {
    const p = await helpers.createTestSandbox();
    await p.fixture(...makeFixture(p));
    await p.esy('build');
    return p;
  }

  it('package "subdep" should be visible in all envs', async () => {
    const p = await createTestSandbox();

    const subdep = await p.esy('subdep.cmd');
    expect(subdep.stdout.trim()).toEqual('__subdep__');
    const b = await p.esy('b subdep.cmd');
    expect(b.stdout.trim()).toEqual('__subdep__');
    const x = await p.esy('x subdep.cmd');
    expect(x.stdout.trim()).toEqual('__subdep__');
  });

  it('same for package "dep" but it should reuse the impl of "subdep"', async () => {
    const p = await createTestSandbox();

    const subdep = await p.esy('dep.cmd');
    expect(subdep.stdout.trim()).toEqual('__subdep__');
    const b = await p.esy('b dep.cmd');
    expect(b.stdout.trim()).toEqual('__subdep__');
    const x = await p.esy('x dep.cmd');
    expect(x.stdout.trim()).toEqual('__subdep__');
  });

  it('and root package links into "dep" which links into "subdep"', async () => {
    const p = await createTestSandbox();

    const x = await p.esy('x app.cmd');
    expect(x.stdout.trim()).toEqual('__subdep__');
  });

  it('check that link is here', async () => {
    const p = await createTestSandbox();

    const depFolder = await fs
      .readdir(path.join(p.projectPath, '../esy/3/i'))
      .then(dir => dir.filter(d => d.includes('dep-1.0.0'))[0]);

    const storeTarget = await fs.readlink(
      path.join(p.projectPath, '../esy/3/i', depFolder, '/bin/dep.cmd'),
    );

    expect(storeTarget).toEqual(expect.stringMatching(p.esyPrefixPath));
  });

  it('export build from store', async () => {
    const p = await createTestSandbox();

    // export build from store
    // TODO: does this work in windows?
    await p.esy('export-build ../esy/3/i/dep-1.0.0-*');

    const tarFile = await fs
      .readdir(path.join(p.projectPath, '_export'))
      .then(dir => dir.filter(d => d.includes('.tar.gz'))[0]);

    await tar.x({
      gzip: true,
      cwd: path.join(p.projectPath, '_export'),
      file: path.join(p.projectPath, '_export', tarFile),
    });

    // check symlink target for exported build
    const buildFolder = tarFile.split('.tar.gz')[0];
    const exportedTarget = await fs.readlink(
      path.join(p.projectPath, '_export', buildFolder, '/bin/dep.cmd'),
    );
    expect(exportedTarget).toEqual(expect.stringMatching('________'));

    // drop & import
    const delResult = await del(path.join(p.projectPath, '../esy/3/i', buildFolder), {
      force: true,
    });
    // Should delete 1 folder
    expect(delResult.length).toEqual(1);

    await p.esy('import-build ./_export/dep-1.0.0-*.tar.gz');

    // check symlink target for imported build
    const importedTarget = await fs.readlink(
      path.join(p.projectPath, '../esy/3/i', buildFolder, '/bin/dep.cmd'),
    );
    expect(importedTarget).toEqual(expect.stringMatching(p.esyPrefixPath));
  });
});
