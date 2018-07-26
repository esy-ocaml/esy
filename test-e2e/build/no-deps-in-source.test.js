// @flow

const path = require('path');
const {initFixture, skipTestOnWindows} = require('../test/helpers');

skipTestOnWindows("#272 - needs to be converted to use OCaml scripts for validation");

it('Build - no deps in source', async () => {
  expect.assertions(1);
  const p = await initFixture(path.join(__dirname, './fixtures/no-deps-in-source'));
  await p.esy('build');
  const {stdout} = await p.esy('x no-deps-in-source');
  expect(stdout).toEqual(expect.stringMatching('no-deps-in-source'));
});
