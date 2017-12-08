/**
 * @flow
 */

import type {StoreTree} from './types';
import * as path from './lib/path';

// This is invariant both for dev and released versions of Esy as bin/esy always
// calls into bin/esy.js (same dirname). `process.argv[1]` is the filename of
// the script executed by `node`.
export const CURRENT_ESY_EXECUTABLE = path.join(path.dirname(process.argv[1]), 'esy');

/**
 * Names of the symlinks to build and install trees of the sandbox.
 */
export const BUILD_TREE_SYMLINK = '_build';
export const INSTALL_TREE_SYMLINK = '_install';

/**
 * Name of the tree used to store releases for the sandbox.
 */
export const RELEASE_TREE = '_release';

/**
 * Name of the file used to declare references to source trees.
 */
export const REFERENCE_FILENAME = '_esylink';

/**
 * Constants for tree names inside stores. We keep them short not to exhaust
 * available shebang length as install tree will be there.
 */
export const STORE_BUILD_TREE: StoreTree = 'b';
export const STORE_INSTALL_TREE: StoreTree = 'i';
export const STORE_STAGE_TREE: StoreTree = 's';

/**
 * The current version of esy store, bump it whenever the store layout changes.
 */
export const ESY_STORE_VERSION = 3;

/**
 * The current version of esy metadata format.
 */
export const ESY_METADATA_VERSION = 4;

/**
 * This is a limit imposed by POSIX.
 *
 * Darwin is less strict with it but we found that Linux is.
 */
const MAX_SHEBANG_LENGTH = 127;

/**
 * This is how OCaml's ocamlrun executable path within store look like given the
 * currently used versioning schema.
 */
const OCAMLRUN_STORE_PATH = 'ocaml-n.00.000-########/bin/ocamlrun';

export const ESY_STORE_PADDING_LENGTH =
  MAX_SHEBANG_LENGTH -
  '!#'.length -
  `/${STORE_INSTALL_TREE}/${OCAMLRUN_STORE_PATH}`.length;
