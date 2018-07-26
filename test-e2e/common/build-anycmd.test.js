// @flow

const path = require('path');

const {initFixture, skipSuiteOnWindows} = require('../test/helpers');

skipSuiteOnWindows("#272");

it('Common - build anycmd', async () => {
  const p = await initFixture(path.join(__dirname, './fixtures/simple-project'));

  await p.esy('build');

  await expect(p.esy('build dep')).resolves.toEqual({
    stdout: 'dep\n',
    stderr: '',
  });

  await expect(p.esy('b dep')).resolves.toEqual({
    stdout: 'dep\n',
    stderr: '',
  });

  // make sure exit code is preserved
  await expect(p.esy("b bash -c 'exit 1'")).rejects.toEqual(
    expect.objectContaining({code: 1}),
  );
  await expect(p.esy("b bash -c 'exit 7'")).rejects.toEqual(
    expect.objectContaining({code: 7}),
  );
});
