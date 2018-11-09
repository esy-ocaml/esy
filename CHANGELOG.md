# CHANGELOG

## 0.4.0 @ next

- Switch from `node_modules` to Plug'n'Play (pnp) installations.

  esy now uses the same approach as yarn (see [pnp rfc]) and installs all
  package source code into a central source cache location.

  `esy build` command now uses this source cache location to perform builds
  from.

  `_esy/default/pnp.js` is created with yarn's pnp runtime so that we keep
  compatibility with JS ecosystem.

  This result in much faster installation in case cache is warm and in less disk
  space wasted on duplicated sources between the sandboxes.

  To invoke npm installed binaries (`"bin"` field in `package.json`) one must
  use:

  ```
  % esy webpack
  ```

  invocations. That means npm installed binaries are no in command environment
  path.

  To invoke `node` interpreter enhanced with pnp:

  ```
  % esy webpack
  ```

- New lock format.

  esy 0.4.0 comes with a new lock format. Previously we stored everything in a
  single JSON file `esy.lock.json`.

  But as esy needs to store significantly more metadata about used packages
  (opam metadata included and custom patches from overrides) single JSON file
  isn't very good choice - it's big (over 9K lines usually), it's hard to review
  during changes.

  Instead of a single `esy.lock.json` file esy now produces `esy.lock` directory
  with a JSON file `esy.lock/index.json` which keeps package graph and a set of
  files, one for each opam/override/patch file store.

- Added new command `esy show`.

  Such command can be used to query metadata about npm, opam or other packages
  (hosted on github for example).

  Example:

  ```
  % esy show react
  % esy show @opam/dune
  % esy show github:facebook/reason
  ```

  Thanks to @kazcw for the feature.

- Numerous fixes and improvements for Windows.

  Installation and build are much more robust and much more fast on Windows!

  Many thanks to @bryphe for that!

- Allow links only `"resolutions"`.

  Previously `link:` dependencies were allowed in `"dependencies"`
  configuration. That isn't correct as `"dependencies"` are constraints while
  `link:` declaration is a resolution (it's unifies only with itself as a
  constraint).

  Therefore we allow `link:` only `"resolutions"` now.

- Add `dune-project` to sandbox white list.

  This will make `esy b dune` not fail on fresh projects.

  Thanks to @rizo for fixing this.

- Support root packages without `"esy"` configuration.

  Such packages will still have their dependencies built.

- Make esy generate batch scripts with `.cmd` extensions for npm binaries
  (`"bin"` field in `package.json`).

[pnp rfc]: https://github.com/yarnpkg/rfcs/pull/101

## 0.3.4 @ latest

- One more fix for `esy import-build` which ensures we can run it on a
  completely fresh project.

## 0.3.3 @ next

- Fix `esy import-build` not to fail on a non initialized store, instead
  initialize it.

- Fixes to overrides where the override source is pointing to an opam sandbox.

## 0.3.2 @ next

- Tweak solver criteria to optimize for recent package version.

## 0.3.1 @ next

- Filter out opam dependencies marked with `doc` and `test`.

## 0.3.0 @ next

- Dependency solver now works on Windows (#471, #473, #495).

- A multitude of fixes for Windows support.

  esy now is being built with esy on Windows!

- Support multiple sandbox configurations per project (#445).

  Multiple sandboxes could be configured per project which then can be addressed
  via `@<sandbox-name>` syntax in esy invocations.

  Given that there's `compiler406.json` file in the project directory:

  ```
  % esy @compiler406
  ```

  The command above could be used to install and build the corresponding
  sandbox. The syntax used inside `compiler406.json` file follows `package.json`
  syntax.

  An override mechanism can be used to define new sandboxes which "inherit"
  configuration from other sandboxes:

  ```
  {
    "source": "./package.json",
    "override": {
      "devDependenciesOverride": {
        "ocaml": "4.6.x"
      }
    }
  }
  ```

  See [Multiple Project Sandbox](https://esy.sh/docs/en/multiple-sandboxes.html)
  guide for more info on the feature.

- Support metadata overrides in resolutions (#451).

  A new syntax is allowed when specifying a resolution:

  ```
  "resolutions": {
    "<package-name>": {
      "source": <package-source>,
      "override": <package-override>
    }
  }
  ```

  Where `<package-override>` could define overrides for the following metadata
  found in the `<package-source>` manifest:

  - Custom build/install commands
  - Build environment
  - Exported environment
  - Dependencies

  Example:

  ```
  "resolutions": {
    "package": {
      "source": "https://example.com/some.tgz",
      "override": {
        "build": [
          "./configure --prefix #{self.install}",
          "make"
        ],
        "install": [
          "make install"
        ],
        "exportedEnv": {
          "LIBRARY_PATH": {
            "val": "#{self.lib: $LIBRARY_PATH}",
            "scope": "global"
          }
        }
      }
    }
  }
  ```

  This could be used to "port" software into esy on the fly.

- Support metadata overrides in dependencies' manifests

  Now a dependency can be resolved to a manifest which contains an override:

  ```
  {
    "source": <package-source>,
    "override": <package-override>
  }
  ```

  An override chain can contain more than a single step.

- Allow to link to opam packages (#442, #446).

  Previously if one wanted to link to an opam package they needed to add
  `package.json` with esy specific metadata to a package sources.

  Now linking directly to opam packages is supported but the path to `*.opam`
  file must be specified:

  ```
  "resolutions": {
    "lwt": "link:../path/to/lwt/lwt.opam",
    "lwt_ppx": "link:../path/to/lwt/lwt_ppx.opam",
  }
  ```

  See [docs](https://esy.sh/docs/en/linking-workflow.html#with-opam-packages)
  for more info.

- Allow installing opam packages from GitHub or git sources (#442).

  It is now possible to fetch opam package sources directly from GitHub or git
  repositories:

  ```
  "resolutions": {
    "lwt": "ocsigen/lwt:lwt.opam",
    "lwt_ppx": "ocsigen/lwt:lwt_ppx.opam",
  }
  ```

  See [docs](https://esy.sh/docs/en/using-repo-sources-workflow.html#with-opam-packages<Paste>)
	for more info.

- Fixes to `path:` and `link:` resolution (#492, #497).

  Previously when such dependencies were appearing in a non root package the
  behaviour was unspecified. Now those are correctly resolved relatively to the
  origin.

- Various fixes to error reporting (#479, #486, #493).

- Fix solver to prefer recently released packages (#489)

  The criteria was configured wrong previously, now we define a special property
  "staleness" (number of releases before the latest release) which we optimize
  to a minimum.

- Add experimental `esy gc` command (#438).

	(GC stands for garbage collection)

	The command is used to remove all artifacts from stores but those used by the
	GC "roots" specified on the command line.

	The feature is useful to reduce the space taken by build store and is going to
	be suggested to be executed on CI to remove unused artifacts from esy store
	cache.

## 0.2.11 @ latest

- Bust cache to workaround buggy 0.2.9 poisoned build artifacts.

## 0.2.10 @ latest

- Fix bug with `optDependencies` not being processed correctly.

## 0.2.9 @ latest

- Support for installing dependencies specified via npm dist-tags:

  ```
  "dependencies": {
    "react": "alpha",
    ...
  }
  ```

- On big sandboxes (lots of dependencies) esy initialization time is improved
  (~20 seconds down to ~300ms on a test sandbox).

- Numerous improvements to native Windows support. Esy can build `merlin`
  package and many more.

- Support for installing compiler from sources other than npm registry.
  Previously compiler version wasn't correctly set in such cases.

- `esyi` executable is removed, all `esyi` subcommands are available through
  `esy` subcommands now, this includes `esy solve`, `esy fetch`, `esy install`,
  ...

## 0.2.8 @ latest

- Bring back windows binaries.

- Fix `esy install` to support links to `@opam/*` packages.

## 0.2.7 @ next

- Add `esy add PACKAGENAME...` command which allows to update `package.json`
  with new dependencies specified on a command line and perform installation
  (#346) (@zploskey).

- Apply resolutions for npm packages as well (#370) (@rauanmayemir).

- Do not fail on empty `bin` in `package.json` on npm packages installation
  (#370) (@rauanmayemir).

- Fix `esy ls-modules` command not to show private modules (modules with `__` in
  their names) (#361).

- Make dev environment more resilent to user environment, previously esy was
  failing if user environment contained variables with `%somename%` values
  (#363).

- Normalize paths when linking packages (#357).

- Improvements to handling opam metadata: more comprehensive set of opam
  variables is supported, automatically upgrade to opam 2 metadata format, ...
  (#351, #364).

- Fix for release wrappers to have the corrent arg0, some programs (ocamlmerlin
  notably) were failing b/c they used arg0 to locate helper programs.

## 0.2.6 @ latest

- Fix `esy` npm package to be compat with Node 4.

- Fix installing binaries of linked npm packages.

- Do not fail on binaries which are declared by npm package but do not exist.

## 0.2.5 @ latest

- `esy` invocation now does `esy install` and then `esy build` (@ulrikstrid).

- Make Windows executables be installed correctly (@bryphe).

- Add `----where` to bin wrappers produced with `esy release`.

- Fix to release installation (`esy release`) not to corrupt binary wrappers.

- Better error reporting for reading opam metadata.

- Refactored e2e test suite (@ulrikstrid, @andreypopp).

## 0.2.4 @ latest

- Fix for discovering dependencies for opam sandboxes.

## 0.2.3 @ latest

- Fix for handling opam's `depopts` metadata field.

## 0.2.2 @ latest

- Fix for sandboxes which use `esy legacy-install` command.

## 0.2.1 @ latest

- Windows binaries are included.

  Thanks to @bryphe.

- Improvements to `out-of-source` mode which makes it usable with `dune`:

  - `*.install` files now can be created in a project root by `dune`

  - symlink from `$cur__target_dir` to `$cur__root/_build` is created.

- Handling of `*.install` files was added to `esy`, now opam packages do not
  depend on `@esy-ocaml/esy-installer` package anymore.

  Built-in `esy-installer` command is now being used instead.

- Added support for `#{..}` syntax to user defined scripts.

- Added `esy.buildEnv` configuration which allows to specify build environment
  for the current package.

- Requirement on `rsync` is dropped.

- Improvements to `esy install` command:

  - `esy install` command now correctly handles disjunction in opam `depends`
    formulas. Previously it attempted to solve every term of the disjunction.

  - `esy install` commands was made more robust to invalid dependency formulas
    found on npm. Now `react-scripts` (CRA) could be installed with `esy install`
    and is fully functional.

  - `esy install` received fixes to dedupe logic, now `npm ls` mentions no missing
    packages on installations with `webpack` and `react-scipts`.

  - `esy install` now correctly copies permissions on files added to source
    tarball by opam repository.

  - `esy install` command now correctly invalidates lockfile on changes in
    `devDependencies.

- Further improvements to test suite.

  Thanks to @ulrikstrid.

## 0.2.0 @ latest

This is the same release as 0.1.33 promoted to `latest`.

## 0.1.33 @ preview

- Support for opam sandboxes.

  Now sandboxes with only opam metadata are supported directly:

  ```
  % esy install
  % esy build
  ```

  All dependencies mentioned in `depends` field of found opam files are
  installed.

  If multiple `*.opam` is found then builds commands defined in those opam files
  won't be executed via `esy build`, instead users should execute whatever
  build commands are used with this repository via `esy b`, for example:

  ```
  % esy b dune build
  ```

- Convert release wrappers from bash to native.

  This feature was implemented by @ulrikstrid.

- Experimental native Windows binaries of esy are shipped in this release.

  This feature was implemented by @bryphe.

- e2e test suite for `esy build` commands was rewritten using JS for
  portability. Previously it wass written in `/bin/bash`.

  This feature was implemented by @ulrikstrid.

## 0.1.32 @ preview

- More efficient installation layout for npm packages.

- Fix installation of circular npm dependencies.

## 0.1.31 @ preview

- Fix for converting opam `depends`.

## 0.1.30 @ preview

- Support resolving packages to multiple sources (main + mirrors).

  Currently only `@opam/*` packages take an advantage of that by:

  - Reading `mirrors` attribute of `url` files im opam repository.

  - Using `/opam-urls.txt` index.

- Add `--cache-tarballs-path` to `esy install` and `esy fetch` commands.

  This option can be used to implement offline workflow where packages sources
  are "vendored" along the sandbox code and installation can be performed while
  offline.

- Fix `esy legacy-install` command to use main opam repository.

  Previously it was accidentally using mingw overlay of opam repository.

## 0.1.29 @ preview

- Installation process now checks integrity of packages download from npm and
  opam registries.

- Speed up installation process.

- Fix `esy install` command output.

- Other improvements to `esy install`.

## 0.1.28 @ preview

- New implementation of opam support.

  esy now uses `opam-format` package from opam to understand `opam` file
  metadata. Both `esy` and `esyi` read directly `opam` files to parse `build`
  commands and `depends` formulas.

- Fix mystical "unable to stat" error.

  This was caused by sandbox staleness cache check which wasn't robust against
  removal of manifests from sandbox. This usually happens when you switch
  between branches.

- Windows Support (WIP)

  Bryan Phelps (@bryphe) started working on native Windows support for esy!

  It's not ready yet but hige progress has been made already:

  - Bootstrapped building of esy on Windows via OPAM
  - Enable esy install command on Windows
  - First round of fixes for esy build (#232, #233)

  Thanks @bryphe!

## 0.1.27 @ preview

- esyi: add support for `link:` package sources.

- esy releases are now built on CI automatically for all tagged commits. The
  release process is still manual via `make release` which downloads those built
  artifacts from CI.

## 0.1.26 @ preview

This release was broken and was unpublished, use 0.1.27 instead.

## 0.1.25 @ preview

- esyi: Fix updating copies of opam-repository and esy-opam-override
  repositories.

## 0.1.24 @ preview

- Unpack `*.zip` archives with `unzip`.

- Remove debug artifacts by produced by `esyi` in sandbox directory.

## 0.1.23 @ preview

- Fix resolving `git:` and `github:` package sources.

## 0.1.22 @ preview

- `esyi` now uses naive dependency solver for npm (non-esy) packages.

  npm (non-esy) packages are those without esy configuration defined in
  package.json.

  The naive dependency solver works as in npm/yarn it tries to match each
  dependency one-by-one preferring already resolved versions or the most recent
  versions. Never backtracks.

- New lockfile format.

- Various fixes to semver version/constraint parsing and matching. Things are
  more aligned with how node-semver works now.

## 0.1.21 @ preview

- Change `devDependencies` to be installed as regular dependencies of the root
  package. This allows to use `devDependencies` to specify a concrete version of
  an `ocaml` toolchain. The "isolated" mode to `devDependencies` will be
  re-added back later.

- Bump `fastreplacestring` which fixes a bug with `@opam/omake` installation
  (esy/esy#217).

- Fix opam conversion errors to be logged properly on terminal.

## 0.1.20 @ preview

- Support `devDependencies` with `esyi`.

  Development dependencies specified as `devDependencies` section of
  `package.json` are no supported by `esyi` command.

  They are installed as isolated "roots", which means that they are allowed to
  have conflicting versions with regular dependencies. When reusing a regular
  dependency is possible it is done.

## 0.1.19 @ preview

- Dependency solver now provides possible explanation in case of failures.

## 0.1.18 @ preview

- Parse scripts only for the top level package's manifest.

  We don't need dependencies' scripts ever and also we won't fail if they are
  incorrectly formatted.

## 0.1.17 @ preview

- Build Linux release using Ubuntu 14.04 LTS so it's compatible with older
  libc than previous.

## 0.1.16 @ preview

- New experimental installer exposed as `esyi` command!

  Usage:

  ```
  % esyi
  ```

  It will create `esyi.lock.json` and `node_modules` directories.

  Caveats & notes:

  - Only regular `"dependencies"` are installed, support for `"peerDependencies"`
    and `"buildDependencies"` will come soon.

  - `@opam/*` packages now use original opam versioning, this means that
    previously published packages which refet to `@opam/*` dependencies with
    npm-like versions will fail, for example `@esy-ocaml/reason`. The fix would
    be to add a field to `"resolutions"` which forces to use an opam version for
    the offended dependency:

    ```
    "resolutions": {"@opam/merlin-extend": "0.3"}
    ```

  - Some package sources like `git:*`, `path:*` and `link:*` are not yet
    supported.

  - There's no good error explanation if dep solver fails, this will be
    addressed soon.

## 0.1.15 @ preview

- Fix dependency on @esy-ocaml/esy-opam which was broken since 0.1.12.

- Bundle fastreplacestring with esy prebuilt. This removes the need for `g++` on
  users machines.

## 0.1.14 @ preview

This release was broken and was unpublished, use 0.1.15 instead.

## 0.1.13 @ preview

This release was broken and was unpublished, use 0.1.15 instead.

## 0.1.12 @ preview

- Fix building opam packages with `%{pkg1+pkg2:var}` syntax constructs in its
  opam files. Previously we didn't support such opam idiom but now with have
  `#{cond ? then : else}` which handles that. Packages such as `@opam/tyxml`
  are now buildable.

- Fix commands which operate on a single packages (like `esy build-shell`) to
  correctly resolve a package by a paclage path specified with a trailing
  slash. (Thanks @despairblue!)

## 0.1.11 @ preview

- Make npm releases generated with `esy release` command compatible with Node
  versions down 4.2.6.

- `esy` command now climbs up to the closest `package.json` from the current
  cwd. This makes it possible to invoke `esy` from the subdirectories of your
  esy projects.

  Note that `esy build` and `esy build ANYCOMMAND` are still being invoked
  from the source root.

## 0.1.10 @ preview

- New experimental installer esyi exposed as `esy install-next` command.
  Thanks to @jaredly!

- Bring back npm releases (`esy release` command).

## 0.1.9 @ preview

- Support for `"esy.sandboxEnv"` environment config.

  This sets environment variables for regular dependency (all dependencies
  excluding `"devDependencies"` and `"buildTimeDependencies"`).

  (Implementation by @rauanmayemir)

- Minor fixes to error reporting

## 0.1.8 @ preview

- Compile `esy` and `esy-build-package` commands using `ocamlopt`

  This also solves the issues with freezes of esy invocation apparently.

- Reimplement `esy import-build` and `esy export-build` commands in OCaml.

  This also allows to remove an entire bash runtime.

- Fix an interminent deadlock resulted from an incorrect implementation of a
  priority queue for lwt promises. The new implementation is based on
  `Lwt_pool`.

- Correctly resolve linked packages when running `esy build-shell` (and
  others) command.

- Allow to augment `$PATH`, `$MAN_PATH` and `$OCAMLPATH` via
  `"esy.exportedEnv"`.

- Add support for `"buildTimeDependencies"`.

  Packages declared as `"buildTimeDependencies"` in `package.json` are only
  added to the environment of their direct dependents. This allows to have
  multiple package versions of the same package as buildTimeDependencies
  within a single dependency graph.

- References `#{pkg-name.install}` now point to stage dir during build
  (consistent with `#{self.install}`).

## 0.1.7 @ preview

- `esy ls-libs` and `esy ls-modules` commands are implemented in Reason.

- `esy <anycmd>`, `esy x <anycmd>` and `esy b <anycmd>` now preserve
  exit code.

- Do not store mtime of the sources for the root package.

  See [#144](https://github.com/esy/esy/issues/144) for rationale.

- `esy build` now provides better formatting in case of command failures.

## 0.1.6 @ preview

- Fix how `#{self.*}` variables were treated inside `"esy.exportedEnv"` — they
  were expanded into `%store%/s/...` _stage paths_ while the correct way to expand
  them into `%store%/i/...` _install paths_ as dependent packages consume env
  when origin package is already built and installed.

## 0.1.5 @ preview

- Implement `bin/esy` entry point in OCaml.

  This makes `esy` command much faster — `esy <cmd>` is around 80ms (was
  180ms) before on MacBook Pro 2016.

- Implement `esy export-dependencies` and `esy import-dependencies`
  commands in OCaml.

- Include `devDependencies` in `esy x <anycommand>` environment.

  See [#137](https://github.com/esy/esy/issues/137) for rationale.

- Fix bug with overly aggressive caching of a command environment.

  Previously the environment was computed once and cached, this prevented
  passing environment variables from the outside, for example:

  ```
  % OCAMLRUNPARAM="b" esy ocamlrun ...
  ```

- Fix `esy import-opam` command which was broken in 0.1.4.

- Fix creating `~/.esy/3` symlink to a padded store path when initializing
  global store.

## 0.1.4 @ preview

- Reimplement `esy export-dependencies` and `esy import-dependencies`.

- Eject `command-env` shell source to `node_modules/.cache/_est`. This is
  needed for integration with OCaml language server.

- Fix bug with splitting `"esy.install"` and `"esy.build"` commands.

- Fix `{comamnd,sandbox}-env` commands to escape double quotes in var values.

- Cleanup `bin/esy` bash wrapper script to remove cruft which resulted in
  100ms faster invocations.

## 0.1.3 @ preview

- Do not run `esy-build-package` just to check if linked deps are changed, do
  it in the same process — this is faster.

- Fixes to parsing of `"esy.build"` and `"esy.install"` commands.

- Better error reporting for parsing commands and environment declarations.

- Show output for the root package build.

- Fix `esy build-package` command not to build dependencies twice.

## 0.1.2 @ preview

- Make things faster by adding caching to sandbox metadata.

- Build devDependencies in parallel with the root package.

- Fix a bug with `esy build <anycmd>`, `esy <anycmd>`, `esy build-shell` not
  checking if deps are built before starting.

- Restrict build concurrency by the number of CPU cores available.

- Fix various bugs: fd leaks, not flushing output channels and so on.

## 0.1.1 @ preview

- Fix a bug with how esy constructed command-env — the external `$PATH` was
  taking a precedence over sandboxed `$PATH`.

## 0.1.0 @ preview

- Release new esy core re-implementation in Reason/OCaml.

  A lot of code was replaced and rewritten. This is why we release it under
  `preview` npm tag and not even `next`. Though `esy@preview` already can
  support the workflow of building itself.

## 0.0.68 @ latest

- Pin dependency to `@esy-ocaml/ocamlrun` package.

## 0.0.67

- Broken release

## 0.0.66

- Report progress on console even if no tty is available.

  This keeps CI updated and prevent it from timing out thinking builds are stale
  while they are not.

## 0.0.65

- `esy install` now tries to fetch `@opam/*` packages from OPAM archive.

  This is made so esy is less dependent on tarballs hosted on author's servers.

  This only happens if there's no override specified in
  esy-ocaml/esy-opam-override repository.

- Build locks are more granular now and don't require `@esy-ocaml/flock` package
  which was fragile on some systems.

## 0.0.64

- Fix a bug with error reporting in 0.0.63.

## 0.0.63

- New command `esy create` to initialize new esy projects from templates.
  Implemented by @rauanmayemir.

- Source modification check for linked packages is now much faster as it is
  implemented in OCaml.

- New command `esy build-plan [dep]` which prints build task on stdout. Build
  task is a JSON data structure which holds all info needed to build the package
  (environment, commands, ...).

- New command `esy build-package` which builds build tasks produced with `esy build-plan` command:

  ```
  % esy build-plan > ./build.json
  % esy build-package build -B ./build.json
  ```

  or directly via stdout:

  ```
  % esy build-plan | esy build-package build -B -
  ```

  Run:

  ```
  % esy build-package --help
  ```

  for more info.

- Build devDependencies in parallel with the root build.

- Remove `dev` and `pack` release and keep only `bin` releases.

- Remove `esy build-eject` command.

## 0.0.62

- Allow to override `@opam/*` packages `url` and `checksum`.

## 0.0.61

- Add `esy ls-modules` command which shows a list of available OCaml modules for
  each of dependency. Implemented by @rauanmayemir.

- Add `$cur__original_root` to build environment which points to the original source
  location of the current package being built.

  Also add `#{self.original_root}` and `#{package_name.original_root}` bindings
  to `#{...}` interpolation expressions.

- Relax sandbox restrictions to allow write `.merlin` files into
  `$cur__original_root` location.

## 0.0.60

- Fix `esy import-build --from <filename>`. See #97 for details.

- Check if `package.json` or `esy.json` is not available in the current
  directory and print nice error message instead of failing with a stacktrace.

## 0.0.59

- Fix `esy build-shell` command to work with `devDependencies`.

- Acquire locks only when invocation is going to perform a build.

- Fixes to how symlink are handled when relocating installation directory
  between between staging and final directory and between stoes (export/import
  and releases).

## 0.0.58

- Esy prefix now can be configured via `.esyrc` by setting `esy-prefix-path`
  property. Example:

  ```
  esy-prefix-path: ./esytstore
  ```

  Esy looks for `.esyrc` in two locations:

  - Sandbox directory: `$ESY__SANDBOX/.esyrc`.
  - User home directory: `$HOME/.esyrc`.

- Fix passing command line arguments to `esy install` and `esy add` commands.

- Fix cloning OPAM and OPAM overrides repositories to respect `--offline` and
  `--prefer-offline` flags. Also make them check if the host is offline and fail
  with a descriptive error instead of hanging.

## 0.0.57

Broken release.

## 0.0.56

- Another bug fix for `#{...}` inside `esy.build` and `esy.install` commands.

## 0.0.55

- Fix bug with scope for `#{...}` inside `esy.build` and `esy.install` commands.

  It was using a `<storePath>/i` instead of `<storePath>/s` for bindings
  pointing to install location. See #89 for details.

## 0.0.54

- Fix sandbox environment to include root package's exported environment.

- Fix for packages which have dot (`.`) symbol in their package names.

## 0.0.53

- Add `esy ls-libs` command which shows a list of available OCaml libraries for
  each of the dependencies. Pass `--all` to see the entire dep tree along with
  OCaml libs. Implemented by @rauanmayemir.

- Command `esy import-build` now supports import builds using `--from/-f <list>`
  option:

  ```
  % esy import-build --from <(find _export -type f)
  ```

  The invocation above will import all builds which reside inside `_export`
  directory.

  That was added to circumvent script startup overhead when importing a large
  number of builds.

- Rename `esy build-ls` command to `esy ls-builds` command so that it is
  consistent with `esy ls-libs`.

- Make variables for the current package also available under `self` scope.

  Instead of using verbose and repetitive `#{package-name.lib}` we can now use
  `#{self.lib}`.

## 0.0.52

- Remove `$cur__target_dir` for builds which are either:

  - Immutable (persisted in the global store). We don't need incremental builds
    there and it's more safer to build from scratch.

  - In-source. We can't enable incremental builds for such builds even if they
    are not being put into global store.

## 0.0.51

- Fix binary releases not to produce single monolithic tarballs.

  So we don't hit GitHub releases limits.

## 0.0.50

- New variable substitution syntax is available for `esy.build`, `esy.install` and
  `esy.exportedEnv`.

  Example:

  ```
  "esy": {
    "exportedEnv": {
      "CAML_LD_LIBRARY_PATH": {
        "val": "#{pkg.lib / 'stublibs' : $CAML_LD_LIBRARY_PATH}"
      }
    }
  }
  ```

  Such variable substitution is performed before the build occurs.

- Automatically export `$CAML_LD_LIBRARY_PATH` variable with the
  `${pkg.stublibs : pkg.lib / 'stublibs' : $CAML_LD_LIBRARY_PATH}`
  value but only case package doesn't have `$CAML_LD_LIBRARY_PATH` in its
  `esy.exportedEnv` config.

- Environment ejected as shell scripts now has nicer format with comments
  indicating from which package the variables are originating from.

## 0.0.49

- Fixes to `esy install` command:

  - OPAM package conversion now convert `depopts` as `optDependencies` which are
    not handled by `esy install` (on purpose) but handled by `esy build`. That
    makes `optDependencies` a direct analogue of OPAM's `depopts`.

  - Fix OPAM package conversion to preevaluate package dependency formulas with
    `mirage-no-xen == true` and `mirage-no-solo5 == true`. This is a temporary
    measure to make a lot of popular packages build. A proper fix pending.

  - Better error reporting in case version constraint wasn't satisfied because of
    OCaml version constraint.

  - Better warning message in case custom resolution doesn't satisfy constraints
    imposed by other packages.

- `esy build` command is now aware of `optDependencies`.

## 0.0.48

- Fixes to `esy install` command:

  - Now it correctly handles OPAM version constraints with `v` prefix (example:
    `v0.9.0`).

    This will invalidate lockfiles which are happen to have records for packages
    with those versions.

  - Handle include files even for packages which doesn't have `url` OPAM meta
    (example: `conf-gmp`).

## 0.0.47

- Fixes to `esy install` command to allow overrides for patches and install
  commands for OPAM packages.

- Fixes to staleness for linked packages which prevent false positives.

## 0.0.46

- Fix `esy import-opam` not to print command header so the output can be piped
  to a `package.json`:

  ```
  % esy import-opam <name> <version> <path/to/opam/file> > package.json
  ```

## 0.0.45

- Fix to locking not to acquire a lock when one is already acquired.

## 0.0.44

- Fix too coarse locking.

  Now we lock only if we can possibly call into Node.

## 0.0.43

- Fix undefined variable reference in `$NODE_ENV`.

## 0.0.42

- Fixes 0.0.41 broken release by adding postinstall.sh script.

## 0.0.41

- Fixes 0.0.40 broken release by adding missing executables.

## 0.0.40

- Add a suite of commands to import and export builds to/from store.

  - `esy export-dependencies` - exports dependencies of the current sandbox.

    Example:

    ```
    % esy export-dependencies
    ```

    This command produces an `_export` directory with a set of gzipped tarballs
    for each of the current project's dependencies.

  - `esy import-dependencies <dir>` - imports dependencies of the current
    sandbox into a store.

    From a directory produced by the `esy export-dependencies` command:

    ```
    % esy import-dependencies ./_export
    ```

    From another Esy store:

    ```
    % esy import-dependencies /path/to/esy/store/i
    ```

- Enable incremental builds for linked dependencies which are configured with:

  ```
  "esy": {
    "buildsInSource": "_build",
    ...
  }
  ```

  (think of jbuilder and ocamlbuild)

- Make `esy x <anycommand>` invocation faster.

  Esy won't perform linked dependencies staleness checks and won't trigger a
  build process anymore. It assumes the project was fully built before.

  For the cases where we want always fresh build artifacts you can combine it
  with `esy b`:

  ```
  % esy b && esy x <anycommand>
  ```

- Do not use symlinks for `link:` dependencies.

  Instead use `_esylink` marker. That prevents linked package's dependencies
  leaking into sandbox.

- Add lock for esy invocations: only single esy command is allowed to run at the
  same time.

  Any other invocaton will be aborted with an error immediately upon startup.

  This ensures there's no corruption of build artifacts for linked dependencies.

- Fix a bug in dependency resolution which caused a wrong version of dependency
  to appear with mixed `esy.json` and `package.json` packages.

## 0.0.39

- Use OPAM version ordering when solving dependencies for `@opam/*` packages.

- Fixes to unpackacking OPAM packages' tarballs.

- Make `esy x <anycommand>` command invocaton to perform installation only once.

  That makes subsequent runs of `esy x <anycommand>` to be substantially faster.

- Add `command-exec` executable to ejected root builds. This is used by
  ocaml-language-server package to automatically configure itself to use Esy
  sandboxed environment.

  See freebroccolo/ocaml-language-server#68 for more info.

- Fix builds with dependency graphs with linked packages.

  Previously builds which depend on transient packages were put into a global
  store which is incorrect. Instead those builds are marked as transient too and
  being put into sandbox local store.

## 0.0.38

- Fixes a bug with error in case of build failure which shadowed the actual build
  failure (see #49).

## 0.0.37

- Fixes `0.0.36` release which was broken due to a missing `esx` executable in
  the distribution.

## 0.0.36

- Add `esy x <anycommand>` invocation which allows to execute `<anycommand>` as
  if the project is installed (executables are in `$PATH` and so on).

- New build progress reporter which is consistent with `esy install` command.

- `esy build` command now shows output of build commands on stdout.

- Fix a bug with how build hashes are computed.

- Add experimental `esx` command.

  This is analogue to `esx`. It allows to initialize ad-hoc snadboxes with
  needed packages and run commands right away:

  ```
  % esx -r ocaml -r @opam/reason rtop
  ```

  The command above will init a sandbox with `ocaml` and `@opam/reason` packages
  inside and run `rtop` command (provided by `@opam/reason`). Such sandboxes are
  cached so the next invocations have almost zero overhead.

## 0.0.35

- Add (undocumented yet) `esy build-ls` command.

  This prints the build tree with build info.

- Fix race condition between build process and build ejection (see #40).

- Fix build error when building linked packages (see #36).

- Fix `esy add` to update the correct manifest (see #36).

  Previously it was updating `package.json` even if `esy.json` was present.

- Fix reporting errors with log files residing in sandbox-local stores (see
  #38).

- Shell builder now clears build log before performing the build (see #31).

## 0.0.34

- Fix `esy add` to actually build after the install.

- Run `esy` command wrapper with with `-e` so that we fail on errors.

## 0.0.33

- Make `esy` invocation perform `esy install` and then `esy build`.

  This makes the workflow for starting a development on a project:

  ```
  % git clone project
  % cd project
  % esy
  ```

  Also if you change something in `package.json` you need to run:

  ```
  % esy
  ```

  Pretty simple and consistent with how Yarn behave.

- Make `esy add <pkg>` automatically execute `esy build` after the installation
  of the new package.

  Previously users were required to call `esy build` manually.

- Update OPAM package conversion to include `test`-filtered packages only
  `devDependencies` (see #33 for details).

## 0.0.32

- `esy shell` and `esy <anycommand>` now include dev-time dependencies (declared
  via `devDependencies` in `package.json`) in the environment.

  Examples of dev-time dependencies are `@opam/merlin`, `@opam/ocp-indent`
  packages. Those are only used during development and are not used during the
  build or runtime.

## 0.0.31

- Fix an issue with `esy build/shell/<anycommand>` not to react properly on
  build failure.

- Fix error reporting in ejected builds to report the actual log file contents.

- Use pretty paths to stores without paddings (a lot of underscores).

## 0.0.30

- Command `esy install` now uses `.esyrc` instead of `.yarnrc` for
  configuration.

  If you have `.yarnrc` file in your project which is used only for esy then you
  should do:

  ```
  mv .yarnrc .esyrc
  ```

- Fixed a bug with `esy install` which executed an unrelated `yarn` executable
  in some custom environment setups. Now `esy install` executes only own code.

- Fixed a bug with `esy install` which prevented the command run under `root`
  user. This was uncovered when running `esy install` under docker.

## 0.0.29

- `esy build` command was improved, more specifically:

  - There's new build mode which activates with:

    ```
      "esy": {
        "buildsInSource": "_build"
      }
    ```

    config in `package.json`.

    This mode configures root packages to build into `$cur__root/_build` without
    source relocation. Thus enabling fast incremental builds for projects based
    on jbuilder or ocamlbuild build systems.

    Note that linked packages with `"buildsInSource": "_build"` are still built
    byb relocating sources as it is unsafe to share `$cur__root/_build`
    directory between several sandboxes.

  - Packages now can describe installation commands separately from build
    commands, by using:

    ```
      "esy": {
        "install": ["make install"]
      }
    ```

    config in `package.json`.

    `esy build` invocation now only executes build steps (`"esy.build"` key in
    `package.json`) for the root package build.

  - `esy build` command now ejects a shell script for root build command &
    environment:

    ```
    node_modules/.cache/_esy/bin/build
    node_modules/.cache/_esy/bin/build-env
    ```

    On later invokations `esy build` will reuse ejected shell script to perform
    root project's build process thus enabling invoking builds without spawning
    Node runtime.

    Ejected script invalidates either on any change to `package.json`
    (implemented similarly to how ejected command env invalidates) or to changes
    to linked packages.

  - `esy build <anycommand>` is now supported.

    This works similar to `esy <anycommand>` but invokes `<anycommand>` in build
    environment rather than command environment.

    Currently there are minor changes between build environment and command
    environment but this is going to change soon.

- `esy <anycommand>` and `esy shell` commands implementations changed, more
  specifically:

  - Their environment doesn't include root package's path in `$PATH`,
    `$MAN_PATH` and `$OCAMLPATH`.

  - The location of ejected environment changed from:

    ```
    node_modules/.cache/_esy/command-env
    ```

    to:

    ```
    node_modules/.cache/_esy/bin/command-env
    ```

  - Now `esy build --dependencies-only --silent` is called to eject the command
    env. That means that if command environment is stale (any of `package.json`
    files were modified) then Esy will check if it needs to build dependencies.

- Fix `esy build-shell` command to have exactly the same environment as `esy build` operates in.

- Allow to initialize a build shell for any package in a sandbox. Specify
  a package by the path to its source:

      % esy build-shell ./node_modules/@opam/reason

## 0.0.28

- Support for installing packages with only `esy.json` available.

- Add suport for JSON5-encoded `esy.json` manifests and fix edgecases related to
  installation of packages with `esy.json`.

## 0.0.27

- Fix release installation not to ignore "too deep path" error silently.

- Fix a check for a "too deep path" error.

- Esy store version is now set to `3`.

  This is made so the Esy prefix can be 4 chars longer. This makes a difference
  for release installation locations as thise can be 4 chars longer too.

- Change the name of the direction with esy store inside esy releases to be `r`.

  The motivation is also to allow longer prefixes for release installation
  locations.

## 0.0.26

- `esy install` command now supports same arguments as `yarn install`

- Added `esy b` and `esy i` shortcuts for `esy build` and `esy install`
  correspondingly.

- Fix `esy add` command invocation.

  Previously it failed to resolve opam packages for patterns without
  constraints:

      % esy add @opam/reason

  Now it works correctly.

- Expose installation cache management via `esy install-cache` command.

  This works similar to `yarn cache` and in fact is based on it.

- Fix `esy import-opam` to produce `package.json` with dependencies on OCaml
  compiler published on npm registry.

## 0.0.25

- Support for `esy.json`.

  Now if a project (or any dependency) has `esy.json` file then it will take
  precedence over `package.json`.

  This allow to use the same project both as a regular npm-compatible project
  and an esy-compatible project.

- Change lockfile filename to be `esy.lock`.

  This is a soft breaking change. So it is advised to manually rename
  `yarn.lock` to `esy.lock` within Esy projects to keep the lockfile.

## 0.0.24

- `esy install` was improved to handle opam converted package more inline with
  the regular npm packages.

  For example offline mirror feature of Yarn is now fully supported for opam
  converted packages as well.

- `command-env` bash scripts was generated with an incorrect default value for
  a global store path.

## 0.0.23

- Fixes `0.0.22` failure on Linux due to incorrectly computed store path
  padding.

## 0.0.22

- Packages converted from opam now depend on `@esy-ocaml/esy-installer` and
  `@esy-ocaml/substs` packages from npm registry rather than on packages on
  github.

## 0.0.21

- Add `esy config` command.

  `esy config ls` prints esy configuration values

  `esy config get KEY` prints esy configuration value for a specific key

  Example:

  ```
  % esy config get store-path
  % esy config get sandbox-path
  ```

## 0.0.20

- Packages produced by `esy release` command now can be installed with Yarn.

## 0.0.19

- `@opam-alpha/*` namespaces for opam-converted packages is renamed to `@opam/*`
  namespace.

  This is a major breaking change and means that you need to fix your
  dependencies in `package.json` to use `@opam/*`:

      {
        "dependencies": {
          "@opam/reason": "*"
        }
      }

- Symlinks to install and build trees inside stores for a top level package now
  are called now `_esyinstall` and `_esybuild` correspondingly.

  This is not to clash with jbuilder and ocamlbuild which build into `_build` by
  default. See #4.

## 0.0.18

- Prioritize root's `bin/` and `lib/` in `$PATH` and `$OCAMLPATH`.

  Root's binaries and ocamlfind libs should take precedence over deps.

## 0.0.17

- Expose `$cur__lib` as part of the `$OCAMLPATH` in command env.

  That means `esy <anycommand>` will make installed ocamlfind artefacts visible
  for `<anycommand>`.

## 0.0.16

- Make `esy release` not require dependency on Esy:

  - For "dev"-releases we make them install the same version of Esy which was
    used for producing the release.

  - For "bin"-releases and "pack"-releases we don't need Esy installation at
    all.

- Command line interface improvements:

  - Add `esy version` command, same as `esy -v/--version`.

  - Add `esy help` command, same as `esy -h/--help`.

  - Fix `esy version` to print the version of the package but not the version of
    Esy specification.

  - Fix `esy release` invocation (with no arguments) to forward to the JS
    implementation.

- Fix `esy release` to handle releases with commands of the same name as the
  project itself.

  Previously such commands were shadowed by the sandbox entry point script. Now
  we generate sandbox entry point scripts as `<proejctname>-esy-sandbox`, for
  example `reason-cli-esy-sandbox`.

## 0.0.15

- Make `esy build` exit with process return code `1` in case of failures.

  Not sure how I missed that!

- More resilence when crteating symlinks for top level package from store
  (`_build` and `_install`).

  Previously we were seeing failures if for example there's `_build` directory
  created by the build process itself.

- Fix ejected builds to ignore `node_modules`, `_build`, `_install` and
  `_release` directories when copying sources over to `$cur__target_dir`
  directory for build.

- Fix `esy build` command to ignore `_build`, `_install` and
  `_release` directories when copying sources over to `$cur__target_dir`
  directory for build.

## 0.0.14

- Fix `esy install` to work on Node 4.x.

- Do not copy `node_modules`, `_build`, `_install`, `_release` directories over
  to `$cur__target_dir` for in-source builds. That means mich faster builds for
  top level packages.

- Defer creating `_build` symlink to `$cur__target_dir` for top level packages.

  That prevented `jbuilder` to work for top level builds.

## 0.0.13

- Generate readable targets for packages in ejected builds.

  For example:

      make build.sandbox/node_modules/packagename
      make shell.sandbox/node_modules/packagename

- `esy install` command now uses its own cache directory. Previously it used
  Yarn's cache directory.

- `esy import-opam` command now tries to guess the correct version for OCaml
  compiler to add to `"devDependencies"`.

- Fixes to convertation of opam versions into npm's semver versions.

  Handle `v\d.\d.\d` correctly and tags which contain `.`.

## 0.0.12

- Fix invocation of `esy-install` command.

## 0.0.11

- Fix bug with `esy install` which didn't invalidate lockfile entries based on
  OCaml compiler version.

- Allow to override `peerDependencies` for `@opam-alpha/*` packages.

## 0.0.10

- Rename package to `esy`:

  Use `npm install -g esy` to install esy now.

- Pin `@esy-opam/esy-install` package to an exact version.

## 0.0.9

- Make escaping shell commands more robust.

## 0.0.8

- Support for converting opam package from opam repository directly.

  Previously we shipped preconverted metadata for opam packages. Now if you
  request `@opam-alpha/*` package we will convert it directly from opam
  repository.
