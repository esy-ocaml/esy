/**
 * @flow
 */

jest.setTimeout(200000);

import * as path from 'path';
import {defineTestCaseWithShell} from '../utils';

defineTestCaseWithShell(
  path.join(__dirname, 'fixtures', 'with-binary'),
  `
    run esy release bin
    run cd _release/bin-*
    run npmGlobal pack
    run npmGlobal -g install ./with-binary-0.1.0.tgz
    assertStdout say-hello.exe HELLO
  `,
);
