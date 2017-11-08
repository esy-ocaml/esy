# Esy

`package.json` workflow for native development with Reason/OCaml.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [What](#what)
  - [For npm users](#for-npm-users)
  - [For OPAM users](#for-opam-users)
  - [In depth](#in-depth)
- [Install](#install)
- [Workflow](#workflow)
  - [Try An Example](#try-an-example)
  - [Configuring Your `package.json`](#configuring-your-packagejson)
    - [Specify Build & Install Commands](#specify-build--install-commands)
      - [`esy.build`](#esybuild)
      - [`esy.install`](#esyinstall)
    - [Enforcing Out Of Source Builds](#enforcing-out-of-source-builds)
    - [Exported Environment](#exported-environment)
  - [Esy Environment Reference](#esy-environment-reference)
    - [Build Environment](#build-environment)
    - [Command Environment](#command-environment)
  - [Esy Command Reference](#esy-command-reference)
- [How Esy Works](#how-esy-works)
  - [Build Steps](#build-steps)
  - [Directory Layout](#directory-layout)
    - [Global Cache](#global-cache)
    - [Top Level Project Build Artifacts](#top-level-project-build-artifacts)
- [Developing](#developing)
  - [Running Tests](#running-tests)
  - [Issues](#issues)
  - [Publishing Releases](#publishing-releases)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## What

### For npm users

For those familiar with [npm][], esy allows to work with Reason/OCaml projects
within the familiar npm-like workflow:

- Declare dependencies in `package.json`.

- Install and build with `esy install` and `esy build` commands. Dependencies'
  source code end up in `node_modules`.

- Share your work with other developers by publishing on npm registry and/or github.

- Access packages published on [OPAM][] (a package registry for OCaml) via
  `@opam` npm scope (for example `@opam/lwt` to pull `lwt` library from OPAM).

### For OPAM users

For those who familiar with [OPAM][], esy provides a powerful alternative:

- Manages OCaml compilers and dependencies on a per project basis.

- Sandboxes project environment by exposing only those packages which are
  defined as dependencies.

- Fast parallel builds which are agressively cached.

- Keeps the ability to use packages published on OPAM repository.

### In depth

- Project metadata is managed inside `package.json`.

- Parallel builds.

- Clean environment builds for reproducibility.

- Global build cache automatically shared across all projects — initializing new
  projects is often cheap.

- File system sandboxing to prevent builds from mutating locations they don't
  own.

- Solves environment variable pain. Native toolchains rely heavily on environment
  variables, and `esy` makes them behave predictably, and usually even gets them
  out of your way entirely.

- Allows symlink workflows for local development (by enforcing out-of-source
  builds). This allows you to work on several projects locally, make changes to
  one project and the projects that depend on it will automatically know they
  need to rebuild themselves.

- Run commands in project environment quickly `esy <anycommand>`.

- Makes sharing of native projects easier than ever by supporting "eject to `Makefile`".

  - Build dependency graph without network access.

  - Build dependency graph where `node` is not installed and where no package
    manager is installed.

## Install

```
% npm install --global esy
```

If you had installed esy previously:

```
% npm uninstall --global --update esy
```

## Workflow

Esy provides a single command called `esy`.

The typical workflow is to `cd` into a directory that contains a `package.json`
file, and then perform operations on that project.

### Try An Example

There are example projects:

- [OCaml + jbuilder project][esy-ocaml-project]
- [Reason + jbuilder project][esy-reason-project]

The typical workflow looks like this:

0. Install esy:
    ```
    % npm install -g esy
    ```

1. Clone the project:
    ```
    % git clone git@github.com:esy-ocaml/esy-ocaml-project.git
    % cd esy-ocaml-project
    ```

2. Install project's dependencies source code:
    ```
    % esy install
    ```

3. Perform an initial build of the project's dependencies and of the project
   itself:
    ```
    % esy build
    ```

4. Test the compiled executables inside the project's environment:
    ```
    % esy ./_build/default/bin/hello.exe
    ```

5. Hack on project's source code and rebuild the project:
    ```
    % esy build
    ```

Also:

6. It is possible to invoke any command from within the project's sandbox.
   For example build & run tests with:
    ```
    % esy make test
    ```
   You can run any command command inside the project environment by just
   prefixing it with `esy`:
    ```
    % esy <anycommand>
    ```

7. To shell into the project's sandbox:
    ```
    % esy shell
    ```

8. For more options:
    ```
    % esy help
    ```

### Configuring Your `package.json`

`esy` knows how to build your package and its dependencies by looking at the
`esy` config section in your `package.json`.

This is how it looks for a [jbuilder][] based project:

```
{
  "name": "example-package",
  "version": "1.0.0",

  "esy": {
    "build": [
      "jbuilder build bin/hello.exe"
    ],
    "install": [
      "jbuilder build @install",
      "jbuilder install --prefix=$cur__install"
    ],
    "buildsinsource": "_build"
  },

  "dependencies": {
    "anotherpackage": "1.0.0"
  }
}
```

#### Specify Build & Install Commands

The crucial pieces of configuration are `esy.build` and `esy.install` keys, then
specify how to build and install built artifacts.

##### `esy.build`

Describe how your project's default targets should be built by specifying
a list of commands with `esy.build` config key.

For example for a [jbuilder][] based project you'd want to call `jbuilder build`
command for each of the target which is going to be installed later:

```
{
  "esy": {
    "build": [
      "jbuilder build bin/hello.exe",
      "jbuilder build lib/MyLib.cmxa",
    ]
  }
}
```

Commands specified in `esy.build` are always executed for the root's project
when user calls `esy build` command.

##### `esy.install`

Describe how you project's built artifacts should be installed by specifying a
list of commands with `esy.install` config key.

For example for a [jbuilder][] based project you'd want to generate an `.install`
file and then call `jbuild install` command:


```
{
  "esy": {
    "build": [...],
    "install": [
      "jbuilder build @install",
      "jbuilder install --prefix $cur__install",
    ]
  }
}
```

Note the `$cur__install` variable which is used for an installation prefix. This
variable is a part of [build environment](#build-environment) provided by Esy.

#### Enforcing Out Of Source Builds

Esy requires packages to be built "out of source".

It allows Esy to separate source code from built artifacts and thus reuse the
same source code location with several projects/sandboxes.

There are three modes which are controlled by `esy.buildsInSource` config key:

```
{
  "esy": {
    "build": [...],
    "install": [...],
    "buildInSource": "_build" | false | true,
  }
}
```

Each mode changes how Esy executes [build commands](#esybuild). This is how
those modes work:

- `"_build"`

  Build commands can place artifacts inside the `_build` directory of the
  project's root (`$cur__root/_build` in terms of Esy [build
  environment](#build-environment)).

  This is what [jbuilder][] or [ocamlbuild][] (in its default configuration)
  users should be using as this matches those build systems' conventions.

- `false` (default if key is ommited)

  Build commands should use `$cur__target_dir` as the build directory.

- `true`

  Build commands cannot be configured to use a different directory than the
  projects root directory. In this case Esy will defensively copy project's root
  into `$cur__target_dir` and run build commands from there.

  This is the mode which should be used as the last resort as it degrades
  perfomance of the builds greatly by placing correctness as a priority.

#### Exported Environment

Packages can configure how they contribute to the environment of the packages
which depend on them.

To add a new environment variable to the Esy [build
environment](#build-environment) packages could specify `esy.exportedEnv` config
key:

```
{
  "name": "mylib",
  "esy": {
    ...,
    "exportedEnv": {
      "CAML_LD_LIBRARY_PATH": "$mylib__lib:$CAML_LD_LIBRARY_PATH",
      "scope": "global"
    }
  }
}
```

In the example above, the configuration *exports* (in this specific case it
*re-exports* it) an environment variable called `$CAML_LD_LIBRARY_PATH` by
appending `$mylib__lib` to its previous value.

### Esy Environment Reference

For each project Esy manages:

- *build environment* — an environment which is used to build the project

- *command environment* — an environment which is used running text editors/IDE
  and for general testing of the built artfiacts

#### Build Environment

The following environment variables are provided by Esy:

- `$SHELL`
- `$PATH`
- `$MAN_PATH`
- `$OCAMLPATH`
- `$OCAMLFIND_DESTDIR`
- `$OCAMLFIND_LDCONF`
- `$OCAMLFIND_COMMANDS`

The following environment variables are defined for each packages in the
project's dependency graph (where `NAME` is the normalized name of the package
as specified in `package.json`):

- `$NAME__install`
- `$NAME__target_dir`
- `$NAME__root`
- `$NAME__name`
- `$NAME__version`
- `$NAME__depends`
- `$NAME__bin`
- `$NAME__sbin`
- `$NAME__lib`
- `$NAME__man`
- `$NAME__doc`
- `$NAME__stublibs`
- `$NAME__toplevel`
- `$NAME__share`
- `$NAME__etc`

The following environment variables are related to the package which is
currently being built:

- `$cur__install`
- `$cur__target_dir`
- `$cur__root`
- `$cur__name`
- `$cur__version`
- `$cur__depends`
- `$cur__bin`
- `$cur__sbin`
- `$cur__lib`
- `$cur__man`
- `$cur__doc`
- `$cur__stublibs`
- `$cur__toplevel`
- `$cur__share`
- `$cur__etc`

This is based on [PJC][] spec.

#### Command Environment

Currently the command environment is identical to build environment sans the
`$SHELL` variable which is non-overriden and equals to the `$SHELL` value of a
user's environment.

### Esy Command Reference

```

Usage: esy <command> [--help] [--version]

install               Installs packages declared in package.json.
i

build                 Builds everything that needs to be built, caches
b                     results. Builds according to each package's "esy"
                      entry in package.json. Before building each package,
                      the environment is scrubbed clean then created according
                      to dependencies.

build <command>       Builds everything that needs to be build, caches
b <command>           results. Then runs a command inside the root package's
                      build environment.

shell                 The same as esy build-shell, but creates a "relaxed"
                      environment - meaning it also inherits your existing
                      shell.

add <package>         Add a specified package to dependencies and installs it.

release TYPE          Create a release of type TYPE ("dev", "pack" or "bin").

print-env             Prints esy environment on stdout.

build-shell [path]    Drops into a shell with environment matching your
                      package's build environment. If argument is provided
                      then it should point to the package inside the current
                      sandbox — that will initialize build shell for that
                      specified package.

build-eject           Creates node_modules/.cache/esy/build-eject/Makefile,
                      which is later can be used for building without the NodeJS
                      runtime.

                      Unsupported form: build-eject [cygwin | linux | darwin]
                      Ejects a build for the specific platform. This
                      build-eject form is not officially supported and will
                      be removed soon. It is currently here for debugging
                      purposes.

install-cache         Manage installation cache (similar to 'yarn cache'
                      command).

import-opam           Read a provided opam file and print esy-enabled
                      package.json conents on stdout. Example:

                        esy import-opam lwt 3.0.0 ./opam

config ls|get         Query esy configuration.

help                  Print this message.

version               Print esy version and exit

<command>             Executes <command> as if you had executed it inside of
                      esy shell.

```

## How Esy Works

### Build Steps

The `build` entry in the `esy` config object is an array of build steps executed in sequence.

There are many build in environment variables that are automatically available
to you in your build steps. Many of these have been adapted from other compiled
package managers such as OPAM or Cargo. They are detailed in the [PJC][] spec
which `esy` attempts to adhere to.

For example, the environment variables `$cur__target_dir` is an environment
variable set up which points to the location that `esy` expects you to place
your build artifacts into. `$cur__install` represents a directory that you are
expected to install your final artifacts into.

A typical configuration might build the artifacts into the special build
destination, and then copy the important artifacts into the final installation
location (which is the cache).

### Directory Layout

Here's a general overview of the directory layout created by various `esy`
commands.

#### Global Cache

When building projects, most globally cached artifacts are stored in `~/.esy`.

    ~/.esy/
     ├─ OtherStuffHereToo.md
     └─ 3___long_enough_padding_for_relocating_binaries___/
        ├── b # build dir
        ├── i # installation dir
        └── s # staging dir

The global store's `_build` directory contains the logs for each package that
is build (whether or not it was successful). The `_install` contains the final
compilation artifacts that should be retained.

#### Top Level Project Build Artifacts

Not all artifacts are cached globally. Build artifacts for any symlinked
dependencies (using `yarn link`) are stored in
`./node_modules/.cache/_esy/store` which is just like the global store, but for
your locally symlinked projects, and top level package.

This local cache doesn't have the dirtyling logic as the global store for
(non-symlinked) dependencies. Currently, both symlinked dependencies and your
top level package are both rebuilt every time you run `esy build`.

Your top level package is build within its source tree, not in a copy of the
source tree, but as always your package can (and should try to) respect the out
of source destination `$cur__target_dir`.

Cached environment computations (for commands such as `esy cmd`) are stored in
`./node_modules/.cache/_esy/bin/command-env`

Support for "ejecting" a build is computed and stored in
`./node_modules/.cache/_esy/build-eject`.

    ./node_modules/
     └─ .cache/
        └─ _esy/
           ├─ bin/
           │  ├─ build-env
           │  └─ command-env
           ├─ build-eject/
           │  ├─ Makefile
           │  ├─ ...
           │  ├─ eject-env
           │  └─ node_modules   # Perfect mirror
           │     └─ FlappyBird
           │        ├─ ...
           │        └─ eject-env
           └─ store/
              ├── ThisIsBuildCacheForSymlinked
              ├── b
              ├── i
              └── s

## Developing

To make changes to `esy` and test them locally:

```
% git clone git://github.com/esy-ocaml/esy.git
% cd esy
% make bootstrap
```

Run:

```
% make
```

to see the description of development workflow.

### Running Tests

```
% make test
```

### Issues

Issues are tracked at [esy-ocaml/esy][].

### Publishing Releases

On a clean branch off of `origin/master`, run:

```
% make bump-patch-version publish
```

to bump the patch version, tag the release in git repository and publish the
tarball on npm.

To publish under custom release tag:

```
% make RELEASE_TAG=next bump-patch-version publish
```

Release tag `next` is used to publish preview releases.

[esy-ocaml-project]: https://github.com/esy-ocaml/esy-ocaml-project
[esy-reason-project]: https://github.com/esy-ocaml/esy-ocaml-project
[esy-ocaml/esy]: https://github.com/esy-ocaml/esy
[OPAM]: https://opam.ocaml.org
[npm]: https://npmjs.org
[Reason]: https://reasonml.github.io
[OCaml]: https://ocaml.org
[jbuilder]: http://jbuilder.readthedocs.io
[ocamlbuild]: https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc
[PJC]: https://github.com/jordwalke/PackageJsonForCompilers
