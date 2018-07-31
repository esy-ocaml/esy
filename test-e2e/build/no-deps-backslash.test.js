// @flow

const path = require('path');
const outdent = require('outdent');
const {
  genFixture,
  ocamlPackage,
  dir,
  file,
  packageJson,
  exeExtension,
  skipSuiteOnWindows,
} = require('../test/helpers');

skipSuiteOnWindows();

const fixture = [
  packageJson({
    name: 'no-deps-backslash',
    version: '1.0.0',
    license: 'MIT',
    esy: {
      build: [
        ['cp', '#{self.original_root /}test.ml', '#{self.target_dir /}test.ml'],
        [
          'ocamlopt',
          '-o',
          '#{self.target_dir / self.name}.exe',
          '#{self.target_dir /}test.ml',
        ],
      ],
      install: [`cp $cur__target_dir/$cur__name.exe $cur__bin/$cur__name${exeExtension}`],
    },
    dependencies: {
      ocaml: '*',
    },
  }),
  file(
    'test.ml',
    outdent`
    let () = print_endline "\\\\ no-deps-backslash \\\\"
  `,
  ),
  dir('node_modules', ocamlPackage()),
];

it('Build - no deps backslash', async () => {
  const p = await genFixture(...fixture);

  await p.esy('build');

  const {stdout} = await p.esy('x no-deps-backslash');
  expect(stdout).toEqual('\\ no-deps-backslash \\\n');
});
