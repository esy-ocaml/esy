#!/bin/bash

set -e
set -o pipefail
set -u

# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  SCRIPTDIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$SCRIPTDIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
SCRIPTDIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

ESYCOMMAND="$SCRIPTDIR/../bin/esy"

export TEST_ROOT=""
export TEST_PROJECT=""
export TEST_NPM_PREFIX=""

export DEBUG="esy:*"
export DEBUG_HIDE_DATE="yes"

initFixture () {
  local name

  name="$1"
  TEST_ROOT=$(mktemp -d)
  TEST_PROJECT="$TEST_ROOT/project"

  cp -r "fixtures/$name" "$TEST_PROJECT"

  esy () {
    "$ESYCOMMAND" "$@"
  }

  pushd "$TEST_PROJECT"
}

initFixtureAsIfEsyReleased () {
  local name
  local releaseDir="$PWD/../dist"

  name="$1"
  TEST_ROOT=$(mktemp -d /tmp/esy.XXXX)
  TEST_PROJECT="$TEST_ROOT/project"
  TEST_NPM_PREFIX="$TEST_ROOT/npm"

  cp -r "fixtures/$name" "$TEST_PROJECT"
  mkdir -p "$TEST_NPM_PREFIX"

  function npmGlobal () {
    npm --prefix "$TEST_NPM_PREFIX" "$@"
  }

  esy () {
    "$TEST_NPM_PREFIX/bin/esy" "$@"
  }

  if [ ! -d "$releaseDir" ]; then
    exit 1
  else
    (cd "$releaseDir" && npm pack && mv esy-*.tgz esy.tgz)
  fi

  npmGlobal install --global "$releaseDir/esy.tgz"

  pushd "$TEST_PROJECT" > /dev/null
}
