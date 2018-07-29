// @flow

const path = require('path');
const fs = require('fs-extra');

const {initFixture, promiseExec} = require('../test/helpers');
const ESYCOMMAND = require.resolve('../../bin/esy');

describe('Common - symlink workflow', async () => {
  let p;
  let appEsy;

  beforeAll(async () => {
    p = await initFixture(path.join(__dirname, 'fixtures', 'symlink-workflow'));
    appEsy = args =>
      promiseExec(`${ESYCOMMAND} ${args}`, {
        cwd: path.resolve(p.projectPath, 'app'),
        env: {...process.env, ESY__PREFIX: p.esyPrefixPath},
      });
  });

  it('works without changes', async () => {
    expect.assertions(2);

    await appEsy('install');
    await appEsy('build');

    const dep = await appEsy('dep');
    expect(dep.stdout).toEqual('HELLO\n');
    const anotherDep = await appEsy('another-dep');
    expect(anotherDep.stdout).toEqual('HELLO\n');
  });

  it('works with modified dep sources', async () => {
    expect.assertions(1);

    await fs.writeFile(
      path.join(p.projectPath, 'dep', 'dep.ml'),
      'print_endline "HELLO_MODIFIED"',
    );

    await appEsy('build');
    const dep = await appEsy('dep');
    expect(dep.stdout).toEqual('HELLO_MODIFIED\n');
  });
});
