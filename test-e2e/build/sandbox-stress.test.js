// @flow

const path = require('path');
const {genFixture, packageJson, skipSuiteOnWindows} = require('../test/helpers');

skipSuiteOnWindows();

const fixture = [
  packageJson({
    "name": "sandbox-stress",
    "version": "1.0.0",
    "license": "MIT",
    "esy": {
      "build": [
        [
          "touch",
          "$cur__target_dir/ok"
        ],
        [
          "touch",
          "#{self.target_dir / 'ok'}"
        ],
        [
          "touch",
          "$cur__original_root/.merlin"
        ],
        [
          "touch",
          "#{self.original_root / '.merlin'}"
        ]
      ],
      "install": [
        [
          "touch",
          "$cur__install/ok"
        ],
        [
          "touch",
          "#{self.install / 'ok'}"
        ]
      ]
    }
  })
];

it('Build - sandbox stress', async () => {
  expect.assertions(1);
  const p = await genFixture(...fixture);
  await p.esy('build');
  const {stdout} = await p.esy('x echo ok');
  expect(stdout).toEqual(expect.stringMatching('ok'));
});
