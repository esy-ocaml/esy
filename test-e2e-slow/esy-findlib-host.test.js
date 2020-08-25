// @flow

const path = require('path');
const {createSandbox} = require('./setup.js');

const sandbox = createSandbox();

console.log(`*** Esy with host findlib workflow test at ${sandbox.path} ***`);

const fixture = path.join(__dirname, 'esy-findlib-host');
sandbox.exec(`cp -rf ${fixture} ./project`);
sandbox.cd('./project');

sandbox.esy('install');
sandbox.esy('ocamlfind', 'list');
sandbox.dispose();
