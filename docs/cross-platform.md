# Cross-Platform Development with `esy`

## Introduction

David Allsopp has an _excellent_ talk on the challenges of cross-platform development here: https://youtu.be/1DAuSSljLFI

This should be required viewing for anyone interested in cross-platform development with Reason / EOL.

## Goal

The goal for `esy` is to work equally well on Linux, Windows, and OSX - without needing special cases or hacks.

The development workflow for `esy` should be the same for all of these platforms.

In addition, because the development workflow is the same for all platforms, it means that setting up your projects to validate on CI is straightforward - the install/build/test process can look something like this:
- `esy install`
- `esy build`
- `esy b jbuilder @runtest`
- `esy release`

## Challenges

The primary challenge to making the OCaml ecosystem work and build on Windows is that build scripts rely heavily on 'bashisms', like `make`, bash shell scripts, etc.

Those tools aren't available out-of-the-box on Windows. WSL is starting to offer more capabilities around this,
but there isn't a seamless install story. An important piece of meeting our goal stated above is that the installation process
for `esy` is simple - `npm install -g esy`.

## Architecture

To provide a seamless build experience for Windows, we use `cygwin` on provide a transparent layer for handling these 'bashisms'. You don't even have to know about it - we install it silently, keep it in a sandbox, and delegate commands to it under the hood - but you don't need to touch it for `esy`. As package authors improve their native Windows support, we may not even need this `cygwin` layer long-term, but it helps get us started with a seamless windows experience.

The good news is that the majority of OCaml build tools do support building from a `cygwin` environment - and often, that is the recommended environment for building on Windows.

Note that, even though our build commands on Windows are executed in a `cygwin` shell, the __binaries that are produced are truly native__. This applies both to all the packages we build, as well as the resultant binary for your project itself. Cygwin provides two compilers - a cygwin `gcc` compiler, and a `mingw` cross-compiler that compiles to native windows executables. We leverage the `mingw` cross-compiler in our builds - the result is `esy build` on Windows gives you native executables. This means that you can use `esy` to build apps natively for _any platform_!

## Platform Differences

### Path considerations

A major difference with Windows vs OSX/Linux is the path separator - `\` vs `/`. Windows can actually work with _either_ path separator (for the most part). 

The challenge, though, is `\` is also the __escape character__ - so we can hit double-escaping issues when we render the strings.

For the time being, we'll __normalize all paths to use a forward slash__ - this keeps us from hitting the 'double-escaping issues' with backslash.

One other complexity is that we use Cygwin for running our build tasks, and the canonical paths for Cygwin are POSIX style, like:
- `C:\temp` -> `/cygdrive/c/temp`

The majority of Cygwin utilities can work with either style of paths - therefore, we will prefer the __Windows path, normalized with a forward slash__, for shell invocations.

Some utilities that Cygwin ships with do not support any style of Windows path (either forward or back slash). For those utilities - like `rsync` - we will need to convert the path to a cygwin-style path. Cygwin includes the `cygpath` utility for this, which can convert paths of the form `C:/temp` or `C:\\temp` to cygwin-style paths like `/cygdrive/c/temp`. These exceptions should be few, and primarily encountered in bootstrapping the `esy` environment internally, as opposed to present in the build scripts.

### Cross-platform environment variables

Another difference in Windows vs OSX/Linux is how the environment variables separate individual paths. On POSIX systems, it's a colon - `:` - on Windows, its `;`

Because our __built artifacts are native__, we must use native Windows paths in our environment variables (normalized with forward slash).

### Symlinks

Symlinks on Windows are not well-supported today. To create symlinks, a Windows user either needs to be running as _administrator_, or set up a special developer mode - neither which is the default behavior on the system.

Therefore, at this time, we should avoid using symlinks in cross-platform code.

### File Permission Model

The file permission models between Windows and OSX/Linux are significantly different - the OCaml API attempts to emulate some of the behavior of `stat` and `chmod`, but use caution when getting / setting file permissions in cross-platform code.

### PATH limit

- Windows has a `PATH` limit of `260`. Newer APIs no longer have this limit, but they are opt-in. You can also set a registry key to fix this, but again, it's opt-in. Something to be mindful of if your tool creates lots of nested paths!

## Long-term plans

As `esy` matures, we can minimize our dependency on `cygwin` for Windows - we can use `esy`'s DSL for specifying build commands to directly target native-windows executables, bypassing the `cygwin` layer. In parallel, we can start to migrate packages to more fully support cross-platform builds.

### Building on Windows

The primary challenge of building on Windows today is that `esy` depends on `esy` itself to build - but we don't have a fully functional `esy` on Windows yet that supports this!

Therefore, for now, we need to 'bootstrap' an `esy` build - that means:
- Install Windows-specific dependencies (esy-bash, FastReplaceString)
- Install OPAM + dependencies need for `esy`
- Run a `bootstrapped` build, where we run `jbuilder` directly on the targets.

To see the latest, up-to-date build steps for Windows - check the [`appveyor.yml`](https://github.com/esy/esy/blob/master/appveyor.yml).

Longer-term, the build for `esy` should be as simple as:
- `npm install -g esy`
- `esy install`
- `esy build`

## Developer considerations

There are currently some additional challenges when working as a developer, building cross-platform code with OCaml - even using the pure standard libraries.

Long-term, we'd like to provide a cross-platform abstraction layer that frees the developer from the cognitive load of these challenges - something that Node/Javascript was very successful at. One thing the NPM/Node ecosystem was successful at is making it _simple_ to build cross-platform solutions, with minimal effort from the developer.

This section is the first stage of such a cross-platform abstraction layer - identifying pain points in cross-platform development today, so that we can iron those out.

### Binary Mode vs Text Mode channels

A common source of problems is in the differing behavior of input / output channels. 

Specifically, on Linux / OSX, the default is for channels to be in `binary` mode. On Windows, however, several APIs default to a `text mode` channel, which does some OS-specific conversions.

This can be summed up in the following table:

| Command  | Windows  | OSX / Linux  |
|---|---|---|
|`open_in`   | Text   | Binary   |
|`open_in_bin`   | Binary   | Binary |
|`open_out`  | Text  | Binary   |
|`open_out_bin` | Binary | Binary |

> __RECOMMENDATION:__ Always use the `open_in_bin` and `open_out_bin` methods by default. In general, binary mode is what you want anyway - especially when using marshalling functions like `input_value` and `output_value`.

This also impacts the default `stdin` / `stdout` channels on Windows. By default, again, these are _text mode_ channels, which will be problematic when used by another process that assumes binary (again, in the marshalling case).

> __RECOMMENDATION:__ Set `stdin` / `stdout` to use binary mode explicitly to ensure consistent cross-platform behavior.

These particular issues are examples:
- https://github.com/facebook/reason/pull/2256 - `ocamlmerlin-reason`: An error bubbled up as `output_value: not a binary channel` on Windows, due to the default stdout-text-channel behavior. Fix was to explicitly set the `stdout` channel to binary mode.
- https://gitlab.inria.fr/fpottier/menhir/issues/16 - `menhir`: Menhir attempted to write to a file with `open_out` and then use `output_value`, which fails on Windows due to the channel not being binary. Fix was to use `open_out_bin.`
