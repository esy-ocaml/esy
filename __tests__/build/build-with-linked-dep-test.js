/**
 * @flow
 */

import {defineTestCaseWithShell} from './utils';

defineTestCaseWithShell(
  'with-linked-dep',
  `
    run esy build

    # package "dep" should be visible in all envs
    assertStdout "esy dep" "dep"
    assertStdout "esy b dep" "dep"
    assertStdout "esy x dep" "dep"

    assertStdout "esy x with-linked-dep" "with-linked-dep"
  `,
);
