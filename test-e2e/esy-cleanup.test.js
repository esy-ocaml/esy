// @flow

const outdent = require('outdent');
const helpers = require('./test/helpers.js');
const path = require('path');
const fs = require('./test/fs.js');
const {test, isWindows} = helpers;

describe('cleanup command', () => {
  test(`should add project path to project.json`, async () => {
    const fixture = [
      helpers.packageJson({
        name: 'root',
        version: '1.0.0',
        esy: {},
        dependencies: {},
      }),
    ];
    const p = await helpers.createTestSandbox(...fixture);
    
    {
      const {stderr} = await p.esy('install');
    }


    await expect(fs.readJson(p.esyPrefixPath + '/projects.json'))
    .resolves
    .toMatchObject([
      p.projectPath
    ]);
  });

  test(`should skip adding project if it is already added`, async () => {
    const fixture = [
      helpers.packageJson({
        name: 'root',
        version: '1.0.0',
        esy: {},
        dependencies: {},
      }),
    ];
    const p = await helpers.createTestSandbox(...fixture);
    await fs.writeJson(p.esyPrefixPath + "/projects.json", [
      p.projectPath
    ])
    
    {
      const {stderr} = await p.esy('install');
    }


    await expect(fs.readJson(p.esyPrefixPath + '/projects.json'))
    .resolves
    .toMatchObject([
      p.projectPath
    ]);
  });
})

