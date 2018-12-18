---
id: version-0.3.4-getting-started
title: Getting started
original_id: getting-started
---

Esy provides a single command called `esy`.

The typical workflow is to `cd` into a directory that contains a `package.json`
file, and then perform operations on that project.

There are example projects:

- [hello-reason](https://github.com/esy-ocaml/hello-reason), an example Reason
  project which uses [dune][] build system.
- [hello-ocaml](https://github.com/esy-ocaml/hello-ocaml), an example OCaml
  project which uses [dune][] build system.


## Install esy

```shell
npm install -g esy
```

If you had installed esy previously:

```shell
npm uninstall --global --update esy
```

## Clone & initialize the project

Clone the project source code

```shell
git clone https://github.com/esy-ocaml/hello-reason.git
cd hello-reason
```

Install project's dependencies source code and perform an initial build of the
project's dependencies and of the project itself:

```shell
esy
```

## Run compiled executables

Test the compiled executables inside the project's environment:

```shell
esy ./_build/default/bin/Hello.exe
```

Alternatively you can use `esy x <anycommand>` invocation which constructs and
environment where the root project is installed:

```shell
esy x Hello.exe
```

## Rebuild the project

Hack on project's source code and rebuild the project:

```shell
esy build
```

## Other useful commands

It is possible to invoke any command from within the project's sandbox.  For
example build & run tests with:

```shell
esy make test
```

You can run any command inside the project environment by just prefixing it with
`esy`:

```shell
esy <anycommand>
```

To shell into the project's sandbox:

```shell
esy shell
```

For more options:

```shell
esy help
```

[dune]: https://github.com/ocaml/dune
