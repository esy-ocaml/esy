/**
 * Implementation of `esy release` process.
 *
 * Release types:
 *
 * **dev**: Dev releases perform everything on the client installer machine
 * (download, build).
 *
 * **pack**: Pack releases perform download and "pack"ing on the "server", and
 * then only builds will be performed on the client. This snapshots a giant
 * tarball of all dependencies' source files into the release.
 *
 * **bin**: Bin releases perform everything on "the server", and "the client"
 * installs a package consisting only of binary executables.
 *
 *                                     RELEASE PROCESS
 *
 *
 *
 *      ○ make release TYPE=dev        ○ make release TYPE=pack      ○─ make release TYPE=bin
 *      │                              │                             │
 *      ○ trivial configuration        ○ trivial configuration       ○ trivial configuration
 *      │                              │                             │
 *      ●─ Dev Release                 │                             │
 *      .                              │                             │
 *      .                              │                             │
 *      ○ npm install                  │                             │
 *      │                              │                             │
 *      ○ Download dependencies        ○ Download dependencies       ○ Download dependencies
 *      │                              │                             │
 *      ○ Pack all dependencies        ○ Pack all dependencies       ○ Pack all dependencies
 *      │ into single tar+Makefile     │ into single tar+Makefile    │ into single tar+Makefile
 *      │                              │                             │
 *      │                              ●─ Pack Release               │
 *      │                              .                             │
 *      │                              .                             │
 *      │                              ○ npm install                 │
 *      │                              │                             │
 *      ○─ Build Binaries              ○─ Build Binaries             ○─ Build Binaries
 *      │                              │                             │
 *      │                              │                             ●─ Bin Release
 *      │                              │                             .
 *      │                              │                             .
 *      │                              │                             ○ npm install
 *      │                              │                             │
 *      ○─ Npm puts binaries in path   ○─ Npm puts binaries in path  ○─ Npm puts binaries in path.
 *
 *
 * For 'bin' releases, it doesn't make sense to use any build cache, so the `Makefile`
 * at the root of this project substitutes placeholders in the generated binary
 * wrappers indicating where the build cache should be.
 *
 * > Relocating: "But aren't binaries built with particular paths encoded? How do
 * we distribute binaries that were built on someone else's machine?"
 *
 * That's one of the main challenges with distributing binaries. But most
 * applications that assume hard coded paths also allow overriding that hard
 * coded-ness in a wrapper script.  (Merlin, ocamlfind, and many more). Thankfully
 * we can have binary releases wrap the intended binaries that not only makes
 * Windows compatibility easier, but that also fixes many of the problems of
 * relocatability.
 *
 * > NOTE: Many binary npm releases include binary wrappers that correctly resolve
 * > the binary depending on platform, but they use a node.js script wrapper. The
 * > problem with this is that it can *massively* slow down build times when your
 * > builds call out to your binary which must first boot an entire V8 runtime. For
 * > `reason-cli` binary releases, we create lighter weight shell scripts that load
 * > in a fraction of the time of a V8 environment.
 *
 * The binary wrapper is generally helpful whether or *not* you are using
 * prereleased binaries vs. compiling from source, and whether or not you are
 * targeting linux/osx vs. Windows.
 *
 * When using Windows:
 *   - The wrapper script allows your linux and osx builds to produce
 *     `executableName.exe` files while still allowing your windows builds to
 *     produce `executableName.exe` as well.  It's usually a good idea to name all
 *     your executables `.exe` regardless of platform, but npm gets in the way
 *     there because you can't have *three* binaries named `executableName.exe`
 *     all installed upon `npm install -g`. Wrapper scripts to the rescue.  We
 *     publish two script wrappers per exposed binary - one called
 *     `executableName` (a shell script that works on Mac/Linux) and one called
 *     `executableName.cmd` (Windows cmd script) and npm will ensure that both are
 *     installed globally installed into the PATH when doing `npm install -g`, but
 *     in windows command line, `executableName` will resolve to the `.cmd` file.
 *     The wrapper script will execute the *correct* binary for the platform.
 * When using binaries:
 *   - The wrapper script will typically make *relocated* binaries more reliable.
 * When building pack or dev releases:
 *   - Binaries do not exist at the time the packages are installed (they are
 *     built in postinstall), but npm requires that bin links exists *at the time*
 *     of installation. Having a wrapper script allows you to publish `npm`
 *     packages that build binaries, where those binaries do not yet exist, yet
 *     have all the bin links installed correctly at install time.
 *
 * The wrapper scripts are common practice in npm packaging of binaries, and each
 * kind of release/development benefits from those wrappers in some way.
 *
 * TODO:
 *  - Support local installations of <package_name> which would work for any of
 *    the three release forms.
 *    - With the wrapper script, it might already even work.
 *  - Actually create `.cmd` launcher.
 *
 * NOTES:
 *
 *  We maintain two global variables that wrappers consult:
 *
 *  - `<PACKAGE_NAME>_ENVIRONMENT_SOURCED`: So that if one wrapped binary calls
 *    out to another we don't need to repeatedly setup the path.
 *
 *  - `<PACKAGE_NAME>_ENVIRONMENT_SOURCED_<binary_name>`: So that if
 *    `<binary_name>` ever calls out to the same `<binary_name>` script we know
 *    it's because the environment wasn't sourced correctly and therefore it is
 *    infinitely looping.  An early check detects this.
 *
 *  Only if we even need to compute the environment will we do the expensive work
 *  of sourcing the paths. That makes it so merlin can repeatedly call
 *  `<binary_name>` with very low overhead for example.
 *
 *  If the env didn't correctly load and no `<binary_name>` shadows it, this will
 *  infinitely loop. Therefore, we put a check to make sure that no
 *  `<binary_name>` calls out to ocaml again. See
 *  `<PACKAGE_NAME>_ENVIRONMENT_SOURCED_<binary_name>`
 *
 *  @flow
 */

import * as fs from './lib/fs';
import * as child_process from './lib/child_process';
import * as os from 'os';
import * as path from 'path';
import * as bashgen from './builders/bashgen';
import outdent from 'outdent';
import {
  ESY_STORE_VERSION,
  DESIRED_ESY_STORE_PATH_LENGTH,
  RELEASE_TREE,
} from './constants';

type ReleaseType = 'dev' | 'pack' | 'bin';

type ReleaseStage = 'forClientInstallation' | 'forPreparingRelease';

type ReleaseActionsSpec = {
  checkIfReleaseIsBuilt: ?ReleaseStage,
  installEsy: ?ReleaseStage,
  configureEsy: ?ReleaseStage,
  download: ?ReleaseStage,
  pack: ?ReleaseStage,
  compressPack: ?ReleaseStage,
  decompressPack: ?ReleaseStage,
  buildPackages: ?ReleaseStage,
  compressBuiltPackages: ?ReleaseStage,
  decompressAndRelocateBuiltPackages: ?ReleaseStage,
  markReleaseAsBuilt: ?ReleaseStage,
};

type BuildReleaseConfig = {
  type: ReleaseType,
  version: string,
  sandboxPath: string,
};

// This is invariant both for dev and released versions of Esy as bin/esy always
// calls into bin/esy.js (same dirname). `process.argv[1]` is the filename of
// the script executed by `node`.
const currentEsyExecutable = path.join(path.dirname(process.argv[1]), 'esy');

const currentEsyVersion = require('../package.json').version;

/**
 * TODO: Make this language agnostic. Nothing else in the eject/build process
 * is really specific to Reason/OCaml.  Binary _install directories *shouldn't*
 * contain some of these artifacts, but very often they do. For other
 * extensions, they are left around for the sake of linking/building against
 * those packages, but aren't useful as a form of binary executable releases.
 * This cleans up those files that just bloat the installation, creating a lean
 * executable distribution.
 */
const extensionsToDeleteForBinaryRelease = [
  'Makefile',
  'README',
  'CHANGES',
  'LICENSE',
  '_tags',
  '*.pdf',
  '*.md',
  '*.org',
  '*.org',
  '*.txt',
];

const pathPatternsToDeleteForBinaryRelease = ['*/doc/*'];

const postinstallScriptSupport = outdent`

  # Exporting so we can call it from xargs
  # https://stackoverflow.com/questions/11003418/calling-functions-with-xargs-within-a-bash-script
  unzipAndUntarFixupLinks() {
    serverEsyEjectStore=$1
    gunzip "$2"
    # Beware of the issues of using "which". https://stackoverflow.com/a/677212
    # Also: hash is only safe/reliable to use in bash, so make sure shebang line is bash.
    if hash bsdtar 2>/dev/null; then
      bsdtar -s "|\${serverEsyEjectStore}|\${ESY_EJECT__INSTALL_STORE}|gs" -xf ./\`basename "$2" .gz\`
    else
      if hash tar 2>/dev/null; then
        # Supply --warning=no-unknown-keyword to supresses warnings when packed on OSX
        tar --warning=no-unknown-keyword --transform="s|\${serverEsyEjectStore}|\${ESY_EJECT__INSTALL_STORE}|" -xf ./\`basename "$2" .gz\`
      else
        echo >&2 "Installation requires either bsdtar or tar - neither is found.  Aborting.";
      fi
    fi
    # remove the .tar file
    rm ./\`basename "$2" .gz\`
  }
  export -f unzipAndUntarFixupLinks

  printByteLengthError() {
    echo >&2 "ERROR:";
    echo >&2 "  $1";
    echo >&2 "Could not perform binary build or installation because the location you are installing to ";
    echo >&2 "is too 'deep' in the file system. That sounds like a strange limitation, but ";
    echo >&2 "the scripts contain shebangs that encode this path to executable, and posix ";
    echo >&2 "systems limit the length of those shebang lines to 127.";
    echo >&2 "";
  }
`;

function scrubBinaryReleaseCommandExtensions(searchDir) {
  return (
    'find ' +
    searchDir +
    ' -type f \\( -name ' +
    extensionsToDeleteForBinaryRelease
      .map(ext => {
        return "'" + ext + "'";
      })
      .join(' -o -name ') +
    ' \\) -delete'
  );
}

function scrubBinaryReleaseCommandPathPatterns(searchDir) {
  return (
    'find ' +
    searchDir +
    ' -type f \\( -path ' +
    pathPatternsToDeleteForBinaryRelease.join(' -o -path ') +
    ' \\) -delete'
  );
}

function escapeBashVarName(str) {
  const map = {'.': 'd', _: '_', '-': 'h'};
  const replacer = match => (map.hasOwnProperty(match) ? '_' + map[match] : match);
  return str.replace(/./g, replacer);
}

function getCommandsToRelease(pkg) {
  return pkg && pkg.esy && pkg.esy.release && pkg.esy.release.releasedBinaries;
}

function createCommandWrapper(pkg, commandName) {
  const packageName = pkg.name;
  const sandboxEntryCommandName = getSandboxEntryCommandName(packageName);
  const packageNameUppercase = escapeBashVarName(pkg.name.toUpperCase());
  const binaryNameUppercase = escapeBashVarName(commandName.toUpperCase());
  const commandsToRelease = getCommandsToRelease(pkg) || [];
  const releasedBinariesStr = commandsToRelease
    .concat(sandboxEntryCommandName)
    .join(', ');

  const execute = commandName !== sandboxEntryCommandName
    ? outdent`
      if [ "$1" == "----where" ]; then
        which "${commandName}"
      else
        exec "${commandName}" "$@"
      fi
      `
    : outdent`
      if [[ "$1" == ""  ]]; then
        cat << EOF

      Welcome to ${packageName}

      The following commands are available: ${releasedBinariesStr}

      Note:

      - ${sandboxEntryCommandName} bash

        Starts a sandboxed bash shell with access to the ${packageName} environment.

        Running builds and scripts from within "${sandboxEntryCommandName} bash" will typically increase
        the performance as environment is already sourced.

      - <command name> ----where

        Prints the location of <command name>

        Example: ocaml ----where

      EOF
      else
        if [ "$1" == "bash" ]; then
          # Important to pass --noprofile, and --rcfile so that the user's
          # .bashrc doesn't run and the npm global packages don't get put in front
          # of the already constructed PATH.
          bash --noprofile --rcfile <(echo 'export PS1="[${packageName} sandbox]"')
        else
          echo "Invalid argument $1, type ${sandboxEntryCommandName} for help"
        fi
      fi
      `;

  return outdent`
    #!/bin/bash

    export ESY__STORE_VERSION=${ESY_STORE_VERSION}

    printError() {
      echo >&2 "ERROR:";
      echo >&2 "$0 command is not installed correctly. ";
      TROUBLESHOOTING="When installing <package_name>, did you see any errors in the log? "
      TROUBLESHOOTING="$TROUBLESHOOTING - What does (which <binary_name>) return? "
      TROUBLESHOOTING="$TROUBLESHOOTING - Please file a github issue on <package_name>'s repo."
      echo >&2 "$TROUBLESHOOTING";
    }

    if [ -z \${${packageNameUppercase}__ENVIRONMENTSOURCED__${binaryNameUppercase}+x} ]; then
      if [ -z \${${packageNameUppercase}__ENVIRONMENTSOURCED+x} ]; then
        ${bashgen.defineScriptDir}
        export ESY_EJECT__SANDBOX="$SCRIPTDIR/../rel"
        export ESY_EJECT__ROOT="$ESY_EJECT__SANDBOX/node_modules/.cache/_esy/build-eject"
        export PACKAGE_ROOT="$SCRIPTDIR/.."
        # Remove dependency on esy and package managers in general
        # We fake it so that the eject store is the location where we relocated the
        # binaries to.
        export ESY_EJECT__STORE=\`cat $PACKAGE_ROOT/records/recordedClientInstallStorePath.txt\`
        ENV_PATH="$ESY_EJECT__ROOT/command-env"
        source "$ENV_PATH"
        export ${packageNameUppercase}__ENVIRONMENTSOURCED="sourced"
        export ${packageNameUppercase}__ENVIRONMENTSOURCED__${binaryNameUppercase}="sourced"
      fi
      command -v $0 >/dev/null 2>&1 || {
        printError;
        exit 1;
      }
      ${execute}
    else
      printError;
      exit 1;
    fi

  `;
}

const actions: {[releaseType: ReleaseType]: ReleaseActionsSpec} = {
  dev: {
    checkIfReleaseIsBuilt: 'forClientInstallation',
    installEsy: 'forClientInstallation',
    configureEsy: null,
    download: 'forClientInstallation',
    pack: 'forClientInstallation',
    compressPack: null,
    decompressPack: null,
    buildPackages: 'forClientInstallation',
    compressBuiltPackages: 'forClientInstallation',
    decompressAndRelocateBuiltPackages: 'forClientInstallation',
    markReleaseAsBuilt: 'forClientInstallation',
  },
  pack: {
    checkIfReleaseIsBuilt: 'forClientInstallation',
    installEsy: null,
    configureEsy: 'forPreparingRelease',
    download: 'forPreparingRelease',
    pack: 'forPreparingRelease',
    compressPack: 'forPreparingRelease',
    decompressPack: 'forClientInstallation',
    buildPackages: 'forClientInstallation',
    compressBuiltPackages: 'forClientInstallation',
    decompressAndRelocateBuiltPackages: 'forClientInstallation',
    markReleaseAsBuilt: 'forClientInstallation',
  },
  bin: {
    checkIfReleaseIsBuilt: 'forClientInstallation',
    installEsy: null,
    configureEsy: 'forPreparingRelease',
    download: 'forPreparingRelease',
    pack: 'forPreparingRelease',
    compressPack: null,
    decompressPack: null,
    buildPackages: 'forPreparingRelease',
    compressBuiltPackages: 'forPreparingRelease',
    decompressAndRelocateBuiltPackages: 'forClientInstallation',
    markReleaseAsBuilt: 'forClientInstallation',
  },
};

/**
 * Derive npm release package.
 *
 * This strips all dependency info and add "bin" metadata.
 */
async function deriveNpmReleasePackage(pkg, releasePath, releaseType) {
  let copy = JSON.parse(JSON.stringify(pkg));

  // We don't manage dependencies with npm, esy is being installed via a
  // postinstall script and then it is used to manage release dependencies.
  copy.dependencies = {};
  copy.peerDependencies = {};
  copy.devDependencies = {};

  // Populate "bin" metadata.
  await fs.mkdirp(path.join(releasePath, '.bin'));
  const binsToWrite = getSandboxCommands(releaseType, releasePath, pkg);
  const packageJsonBins = {};
  for (let i = 0; i < binsToWrite.length; i++) {
    const toWrite = binsToWrite[i];
    await fs.writeFile(path.join(releasePath, toWrite.path), toWrite.contents);
    await fs.chmod(path.join(releasePath, toWrite.path), /* octal 0755 */ 493);
    packageJsonBins[toWrite.name] = toWrite.path;
  }
  copy.bin = packageJsonBins;

  // Add postinstall script
  copy.scripts.postinstall = './postinstall.sh';

  return copy;
}

/**
 * Derive esy release package.
 */
async function deriveEsyReleasePackage(pkg, releasePath, releaseType) {
  const copy = JSON.parse(JSON.stringify(pkg));
  delete copy.dependencies.esy;
  delete copy.devDependencies.esy;
  return copy;
}

async function putJson(filename, pkg) {
  await fs.writeFile(filename, JSON.stringify(pkg, null, 2), 'utf8');
}

async function verifyBinSetup(sandboxPath, pkg) {
  const binDirExists = await fs.exists(path.join(sandboxPath, '.bin'));
  if (binDirExists) {
    throw new Error(
      outdent`
      Run make clean first. The release script needs to be in charge of generating the binaries.
      Found existing binaries dir .bin. This should not exist. Release script creates it.
    `,
    );
  }
  if (pkg.bin) {
    throw new Error(
      outdent`
      Run make clean first. The release script needs to be in charge of generating the binaries.
      package.json has a bin field. It should have a "commandsToRelease" field instead - a list of released binary names.
    `,
    );
  }
}

/**
 * To relocate binary artifacts: We need to make sure that the length of
 * shebang lines do not exceed 127 (common on most linuxes).
 *
 * For binary releases, they will be built in the form of:
 *
 *        This will be replaced by the actual      This must remain.
 *        install location.
 *       +------------------------------+  +--------------------------------+
 *      /                                \/                                  \
 *   #!/path/to/rel/store___padding____/i/ocaml-4.02.3-d8a857f3/bin/ocamlrun
 *
 * The goal is to make this path exactly 127 characters long (maybe a little
 * less to allow room for some other shebangs like `ocamlrun.opt` etc?)
 *
 * Therefore, it is optimal to make this path as long as possible, but no
 * longer than 127 characters, while minimizing the size of the final
 * "ocaml-4.02.3-d8a857f3/bin/ocamlrun" portion. That allows installation of
 * the release in as many destinations as possible.
 */
function createInstallScript(releaseStage: ReleaseStage, releaseType: ReleaseType, pkg) {
  const shouldCheckIfReleaseIsBuilt =
    actions[releaseType].checkIfReleaseIsBuilt === releaseStage;
  const shouldConfigureEsy = actions[releaseType].configureEsy === releaseStage;
  const shouldInstallEsy = actions[releaseType].installEsy === releaseStage;
  const shouldDownload = actions[releaseType].download === releaseStage;
  const shouldPack = actions[releaseType].pack === releaseStage;
  const shouldCompressPack = actions[releaseType].compressPack === releaseStage;
  const shouldDecompressPack = actions[releaseType].decompressPack === releaseStage;
  const shouldBuildPackages = actions[releaseType].buildPackages === releaseStage;
  const shouldCompressBuiltPackages =
    actions[releaseType].compressBuiltPackages === releaseStage;
  const shouldDecompressAndRelocateBuiltPackages =
    actions[releaseType].decompressAndRelocateBuiltPackages === releaseStage;
  const shouldMarkReleaseAsBuilt =
    actions[releaseType].markReleaseAsBuilt === releaseStage;

  const message = outdent`

    #
    # Release releaseType: "${releaseType}"
    # ------------------------------------------------------
    # Executed ${releaseStage === 'forPreparingRelease' ? 'while creating the release' : 'while installing the release on client machine'}
    #
    # Check if release is built:    ${String(shouldCheckIfReleaseIsBuilt)}
    # Configure Esy:                ${String(shouldConfigureEsy)}
    # Install Esy:                  ${String(shouldInstallEsy)}
    # Download:                     ${String(shouldDownload)}
    # Pack:                         ${String(shouldPack)}
    # Compress Pack:                ${String(shouldCompressPack)}
    # Decompress Pack:              ${String(shouldDecompressPack)}
    # Build Packages:               ${String(shouldBuildPackages)}
    # Compress Built Packages:      ${String(shouldCompressBuiltPackages)}
    # Decompress Built Packages:    ${String(shouldDecompressAndRelocateBuiltPackages)}
    # Mark release as built:        ${String(shouldMarkReleaseAsBuilt)}
    #

  `;

  const deleteFromBinaryRelease =
    pkg.esy && pkg.esy.release && pkg.esy.release.deleteFromBinaryRelease;

  const checkIfReleaseIsBuiltCmds = outdent`

    #
    # checkIfReleaseIsBuilt
    #
    if [ -f "$PACKAGE_ROOT/records/done.txt" ]; then
     exit 0;
    fi

  `;

  const configureEsyCmds = outdent`

    #
    # configureEsy
    #
    export ESY_COMMAND="${currentEsyExecutable}"

  `;

  const installEsyCmds = outdent`

    #
    # installEsy
    #
    echo '*** Installing esy for the release...'
    LOG=$(npm install --global --prefix "$PACKAGE_ROOT/_esy" "esy@${pkg.esy.release.esyDependency}")
    if [ $? -ne 0 ]; then
      echo "Failed to install esy..."
      echo $LOG
      exit 1
    fi
    # overwrite esy command with just installed esy bin
    export ESY_COMMAND="$PACKAGE_ROOT/_esy/bin/esy"

  `;

  const downloadCmds = outdent`

    #
    # download
    #
    echo '*** Installing dependencies...'
    cd $ESY_EJECT__SANDBOX
    LOG=$($ESY_COMMAND install)
    if [ $? -ne 0 ]; then
      echo "Failed to install dependencies..."
      echo $LOG
      exit 1
    fi
    cd $PACKAGE_ROOT

  `;
  const packCmds = outdent`

    #
    # Pack
    #
    # Peform build eject.  Warms up *just* the artifacts that require having a
    # modern node installed.
    # Generates the single Makefile etc.
    echo '*** Ejecting build environment...'
    cd $ESY_EJECT__SANDBOX
    $ESY_COMMAND build-eject
    cd $PACKAGE_ROOT

  `;
  const compressPackCmds = outdent`

    #
    # compressPack
    #
    # Avoid npm stripping out vendored node_modules via tar. Merely renaming node_modules
    # is not sufficient!
    echo '*** Packing the release...'
    tar -czf rel.tar.gz rel
    rm -rf $ESY_EJECT__SANDBOX

  `;
  const decompressPackCmds = outdent`

    #
    # decompressPack
    #
    # Avoid npm stripping out vendored node_modules.
    echo '*** Unpacking the release...'
    gunzip "$ESY_EJECT__SANDBOX.tar.gz"
    if hash bsdtar 2>/dev/null; then
      bsdtar -xf "$ESY_EJECT__SANDBOX.tar"
    else
      if hash tar 2>/dev/null; then
        # Supply --warning=no-unknown-keyword to supresses warnings when packed on OSX
        tar --warning=no-unknown-keyword -xf "$ESY_EJECT__SANDBOX.tar"
      else
        echo >&2 "Installation requires either bsdtar or tar - neither is found.  Aborting.";
      fi
    fi
    rm -rf "$ESY_EJECT__SANDBOX.tar"

  `;
  const buildPackagesCmds = outdent`

    #
    # buildPackages
    #
    # Always reserve enough path space to perform relocation.
    echo '*** Building the release...'
    cd $ESY_EJECT__SANDBOX
    make -j -f "$ESY_EJECT__ROOT/Makefile"
    cd $PACKAGE_ROOT

    cp \
      "$ESY_EJECT__ROOT/records/store-path.txt" \
      "$PACKAGE_ROOT/records/recordedServerBuildStorePath.txt"
    # For client side builds, recordedServerBuildStorePath is equal to recordedClientBuildStorePath.
    # For prebuilt binaries these will differ, and recordedClientBuildStorePath.txt is overwritten.
    cp \
      "$ESY_EJECT__ROOT/records/store-path.txt" \
      "$PACKAGE_ROOT/records/recordedClientBuildStorePath.txt"

  `;

  /**
   * In bash:
   * [[ "hellow4orld" =~ ^h(.[a-z]*) ]] && echo ${BASH_REMATCH[0]}
   * Prints out: hellow
   * [[ "zzz" =~ ^h(.[a-z]*) ]] && echo ${BASH_REMATCH[1]}
   * Prints out: ellow
   * [[ "zzz" =~ ^h(.[a-z]*) ]] && echo ${BASH_REMATCH[1]}
   * Prints out empty
   */
  const compressBuiltPackagesCmds = outdent`

    #
    # compressBuiltPackages
    #
    # Double backslash in es6 literals becomes one backslash
    # Must use . instead of source for some reason.
    # Remove the sources, keep the .cache which has some helpful information.
    mv "$ESY_EJECT__SANDBOX/node_modules" "$ESY_EJECT__SANDBOX/node_modules_tmp"
    mkdir -p "$ESY_EJECT__SANDBOX/node_modules"
    mv "$ESY_EJECT__SANDBOX/node_modules_tmp/.cache" "$ESY_EJECT__SANDBOX/node_modules/.cache"
    rm -rf "$ESY_EJECT__SANDBOX/node_modules_tmp"
    # Copy over the installation artifacts.

    mkdir -p "$ESY_EJECT__TMP/i"
    # Grab all the install directories
    for res in $(cat $ESY_EJECT__ROOT/records/final-install-path-set.txt); do
      if [[ "$res" != ""  ]]; then
        cp -r "$res" "$ESY_EJECT__TMP/i/"
        cd "$ESY_EJECT__TMP/i/"
        tar -czf \`basename "$res"\`.tar.gz \`basename "$res"\`
        rm -rf \`basename "$res"\`
        echo "$res" >> $PACKAGE_ROOT/records/recordedCoppiedArtifacts.txt
      fi
    done
    cd "$PACKAGE_ROOT"
    ${releaseStage === 'forPreparingRelease' ? scrubBinaryReleaseCommandPathPatterns('"$ESY_EJECT__TMP/i/"') : '#'}
    ${releaseStage === 'forPreparingRelease' ? (deleteFromBinaryRelease || [])
          .map(function(pattern) {
            return 'rm ' + pattern;
          })
          .join('\n') : ''}
    # Built packages have a special way of compressing the release, putting the
    # eject store in its own tar so that all the symlinks in the store can be
    # relocated using tools that exist in the eject sandbox.

    tar -czf rel.tar.gz rel
    rm -rf $ESY_EJECT__SANDBOX

  `;
  const decompressAndRelocateBuiltPackagesCmds = outdent`

    #
    # decompressAndRelocateBuiltPackages
    #
    if [ -d "$ESY_EJECT__INSTALL_STORE" ]; then
      echo >&2 "$ESY_EJECT__INSTALL_STORE already exists. This will not work. It has to be a new directory.";
      exit 1;
    fi
    serverEsyEjectStore=\`cat "$PACKAGE_ROOT/records/recordedServerBuildStorePath.txt"\`
    serverEsyEjectStoreDirName=\`basename "$serverEsyEjectStore"\`

    # Decompress the actual sandbox:
    unzipAndUntarFixupLinks "$serverEsyEjectStore" "$ESY_EJECT__SANDBOX.tar.gz"

    cd "$ESY_EJECT__TMP/i/"
    # Executing the untar/unzip in parallel!
    echo '*** Decompressing artefacts...'
    find . -name '*.gz' -print0 | xargs -0 -I {} -P 30 bash -c "unzipAndUntarFixupLinks $serverEsyEjectStore {}"

    cd "$PACKAGE_ROOT"
    mv "$ESY_EJECT__TMP" "$ESY_EJECT__INSTALL_STORE"
    # Write the final store path, overwritting the (original) path on server.
    echo "$ESY_EJECT__INSTALL_STORE" > "$PACKAGE_ROOT/records/recordedClientInstallStorePath.txt"

    # Not that this is really even used for anything once on the client.
    # We use the install store. Still, this might be good for debugging.
    cp "$ESY_EJECT__ROOT/records/store-path.txt" "$PACKAGE_ROOT/records/recordedClientBuildStorePath.txt"
    # Executing the replace string in parallel!
    # https://askubuntu.com/questions/431478/decompressing-multiple-files-at-once
    echo '*** Relocating artefacts to the final destination...'
    find $ESY_EJECT__INSTALL_STORE -type f -print0 \
      | xargs -0 -I {} -P 30 $ESY_EJECT__ROOT/bin/fastreplacestring.exe "{}" "$serverEsyEjectStore" "$ESY_EJECT__INSTALL_STORE"

  `;

  const markReleaseAsBuiltCmds = outdent`

    #
    # markReleaseAsBuilt
    #
    touch "$PACKAGE_ROOT/records/done.txt"

  `;

  function renderCommands(commands, enabled) {
    // Notice how we comment out each section which doesn't apply to this
    // combination of releaseStage/releaseType.
    return commands.split('\n').join(enabled ? '\n' : '\n# ');
  }

  const checkIfReleaseIsBuilt = renderCommands(
    checkIfReleaseIsBuiltCmds,
    shouldCheckIfReleaseIsBuilt,
  );
  const configureEsy = renderCommands(configureEsyCmds, shouldConfigureEsy);
  const installEsy = renderCommands(installEsyCmds, shouldInstallEsy);
  const download = renderCommands(downloadCmds, shouldDownload);
  const pack = renderCommands(packCmds, shouldPack);
  const compressPack = renderCommands(compressPackCmds, shouldCompressPack);
  const decompressPack = renderCommands(decompressPackCmds, shouldDecompressPack);
  const buildPackages = renderCommands(buildPackagesCmds, shouldBuildPackages);
  const compressBuiltPackages = renderCommands(
    compressBuiltPackagesCmds,
    shouldCompressBuiltPackages,
  );
  const decompressAndRelocateBuiltPackages = renderCommands(
    decompressAndRelocateBuiltPackagesCmds,
    shouldDecompressAndRelocateBuiltPackages,
  );
  const markReleaseAsBuilt = renderCommands(
    markReleaseAsBuiltCmds,
    shouldMarkReleaseAsBuilt,
  );
  return outdent`
    #!/bin/bash

    set -e

    ${postinstallScriptSupport}

    ${message}

    #                server               |              client
    #                                     |
    # ESY_EJECT__STORE -> ESY_EJECT__TMP  |  ESY_EJECT__TMP -> ESY_EJECT__INSTALL_STORE
    # =================================================================================

    ${bashgen.defineScriptDir}
    ${bashgen.defineEsyUtil}

    export PACKAGE_ROOT="$SCRIPTDIR"

    mkdir -p "$PACKAGE_ROOT/records"

    export ESY__STORE_VERSION="${ESY_STORE_VERSION}"

    export ESY_EJECT__SANDBOX="$SCRIPTDIR/rel"
    export ESY_EJECT__ROOT="$ESY_EJECT__SANDBOX/node_modules/.cache/_esy/build-eject"

    # We Build into the ESY_EJECT__STORE, copy into ESY_EJECT__TMP, potentially
    # transport over the network then finally we copy artifacts into the
    # ESY_EJECT__INSTALL_STORE and relocate them as if they were built there to
    # begin with.  ESY_EJECT__INSTALL_STORE should not ever be used if we're
    # running on the server.
    export ESY_EJECT__INSTALL_ROOT="$ESY_EJECT__SANDBOX"
    export ESY_EJECT__INSTALL_STORE=$(esyGetStorePathFromPrefix $ESY_EJECT__INSTALL_ROOT)

    # Regardless of where artifacts are actually built, or where they will be
    # installed to, or if we're on the server/client we will copy artifacts
    # here temporarily. Sometimes the build location is the same as where we
    # copy them to inside the sandbox - sometimes not.
    export ESY_EJECT__TMP="$PACKAGE_ROOT/relBinaries"

    ${checkIfReleaseIsBuilt}

    ${configureEsy}

    ${installEsy}

    ${download}

    ${pack}

    ${compressPack}

    ${decompressPack}

    ${buildPackages}

    ${compressBuiltPackages}

    ${decompressAndRelocateBuiltPackages}

    ${markReleaseAsBuilt}
  `;
}

function getSandboxEntryCommandName(packageName: string) {
  return `${packageName}-esy-sandbox`;
}

function getSandboxCommands(releaseType, releasePath, pkg) {
  const commands = [];

  const commandsToRelease = getCommandsToRelease(pkg);
  if (commandsToRelease) {
    for (let i = 0; i < commandsToRelease.length; i++) {
      const commandName = commandsToRelease[i];
      const destPath = path.join('.bin', commandName);
      commands.push({
        name: commandName,
        path: destPath,
        contents: createCommandWrapper(pkg, commandName),
      });
    }
  }

  // Generate sandbox entry command
  const sandboxEntryCommandName = getSandboxEntryCommandName(pkg.name);
  const destPath = path.join('.bin', sandboxEntryCommandName);
  commands.push({
    name: sandboxEntryCommandName,
    path: destPath,
    contents: createCommandWrapper(pkg, sandboxEntryCommandName),
  });

  return commands;
}

async function putExecutable(filename, contents) {
  await fs.writeFile(filename, contents);
  await fs.chmod(filename, /* octal 0755 */ 493);
}

async function readPackageJson(releaseType, filename) {
  const packageJson = await fs.readFile(filename);
  const pkg = JSON.parse(packageJson);

  // Perform normalizations
  if (pkg.dependencies == null) {
    pkg.dependencies = {};
  }
  if (pkg.devDependencies == null) {
    pkg.devDependencies = {};
  }
  if (pkg.scripts == null) {
    pkg.scripts = {};
  }
  if (pkg.esy == null) {
    pkg.esy = {};
  }
  if (pkg.esy.release == null) {
    pkg.esy.release = {};
  }

  // store current esy version which is going to be used for dev releases to
  // bootstrap the sandbox environment
  pkg.esy.release.esyDependency = currentEsyVersion;

  return pkg;
}

function getReleaseTag(config) {
  const tag = config.type === 'bin' ? `bin-${os.platform()}` : config.type;
  return tag;
}

/**
 * Builds the release from within the rootDirectory/package/ directory created
 * by `npm pack` command.
 */
export async function buildRelease(config: BuildReleaseConfig) {
  const releaseType = config.type;
  const releaseTag = getReleaseTag(config);

  const sandboxPath = config.sandboxPath;
  const releasePath = path.join(sandboxPath, RELEASE_TREE, releaseTag);
  const esyReleasePath = path.join(sandboxPath, RELEASE_TREE, releaseTag, 'rel');

  const tarFilename = await child_process.spawn('npm', ['pack'], {cwd: sandboxPath});
  await child_process.spawn('tar', ['xzf', tarFilename]);
  await fs.rmdir(releasePath);
  await fs.mkdirp(releasePath);
  await fs.rename(path.join(sandboxPath, 'package'), esyReleasePath);
  await fs.unlink(tarFilename);

  const pkg = await readPackageJson(
    releaseType,
    path.join(esyReleasePath, 'package.json'),
  );
  await verifyBinSetup(sandboxPath, pkg);

  console.log(`*** Creating ${releaseType}-type release for ${pkg.name}...`);

  const npmPackage = await deriveNpmReleasePackage(pkg, releasePath, releaseType);
  await putJson(path.join(releasePath, 'package.json'), npmPackage);

  const esyPackage = await deriveEsyReleasePackage(pkg, releasePath, releaseType);
  await fs.mkdirp(path.join(releasePath, 'rel'));
  await putJson(path.join(esyReleasePath, 'package.json'), esyPackage);

  await putExecutable(
    path.join(releasePath, 'prerelease.sh'),
    createInstallScript('forPreparingRelease', releaseType, pkg),
  );

  // Now run prerelease.sh, we reset $ESY__SANDBOX as it's going to call esy
  // recursively but leave $ESY__STORE & $ESY__LOCAL_STORE in place.
  const env = {...process.env};
  delete env.ESY__SANDBOX;
  await child_process.spawn('bash', ['./prerelease.sh'], {
    env,
    cwd: releasePath,
    stdio: 'inherit',
  });

  // Actual Release: We leave the *actual* postinstall script to be executed on the host.
  await putExecutable(
    path.join(releasePath, 'postinstall.sh'),
    createInstallScript('forClientInstallation', releaseType, pkg),
  );
}
