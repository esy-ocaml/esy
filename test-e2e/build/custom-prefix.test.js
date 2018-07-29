// @flow

const path = require('path');

const {initFixture} = require('../test/helpers');

it('Build - custom prefix', async () => {
  jest.setTimeout(200000);
  expect.assertions(1);
  const p = await initFixture(path.join(__dirname, './fixtures/custom-prefix'));

  await p.esy('install', {noEsyPrefix: true});
  await p.esy('build', {noEsyPrefix: true});

  const {stdout} = await p.esy('x custom-prefix', {noEsyPrefix: true});
  expect(stdout).toEqual('custom-prefix\n');
});
