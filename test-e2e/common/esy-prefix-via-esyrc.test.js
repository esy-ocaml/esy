// @flow

const os = require('os');
const path = require('path');
const del = require('del');
const fs = require('fs-extra');

const {initFixture} = require('../test/helpers');

it('Common - esy prefix via esyrc', async () => {
  expect.assertions(2);
  const p = await initFixture(path.join(__dirname, './fixtures/simple-project'));

  await del(path.join(os.homedir(), '.esytest', 'custom-esy-prefix'), {force: true});

  await fs.writeFile(
    path.join(p.projectPath, '.esyrc'),
    `esy-prefix-path: ${path.join(os.homedir(), '.esytest', 'custom-esy-prefix')}`,
  );

  const prevEnv = process.env;
  process.env = Object.assign({}, process.env, {OCAMLRUNPARAM: 'b'});

  await p.esy('build', {noEsyPrefix: true});

  await expect(p.esy('dep', {noEsyPrefix: true})).resolves.toEqual({
    stdout: 'dep\n',
    stderr: '',
  });

  await expect(p.esy('which dep', {noEsyPrefix: true})).resolves.toEqual({
    stdout: expect.stringMatching(
      path.join(os.homedir(), '.esytest', 'custom-esy-prefix'),
    ),
    stderr: '',
  });

  process.env = prevEnv;
});
