open EsyPackageConfig;

module Repr = {
  [@deriving yojson]
  type opamcommands = {opamcommands: list(opamcommand)}
  [@deriving yojson]
  and opamcommand = {
    args: list(arg),
    commandfilter: option(string),
  }
  [@deriving yojson]
  and arg = {
    arg: string,
    argfilter: option(string),
  };

  type commands =
    | EsyCommands(CommandList.t)
    | OpamCommands(opamcommands)
    | NoCommands;

  let commands_to_yojson =
    fun
    | NoCommands => `Null
    | EsyCommands(commands) => CommandList.to_yojson(commands)
    | OpamCommands(commands) => opamcommands_to_yojson(commands);

  let commands_of_yojson =
    Result.Syntax.(
      fun
      | `Null => return(NoCommands)
      | `List(_) as json => {
          let%map commands = CommandList.of_yojson(json);
          EsyCommands(commands);
        }
      | `Assoc(_) as json => {
          let%map commands = opamcommands_of_yojson(json);
          OpamCommands(commands);
        }
      | _ => error("invalid commands: expected null, list or object")
    );

  [@deriving yojson]
  type patch = {
    path: Path.t,
    filter: option(string),
  };

  [@deriving yojson]
  type t = {
    ocamlPkgName: string,
    packageId: PackageId.t,
    build,
    platform: System.Platform.t,
    arch: System.Arch.t,
    sandboxEnv: SandboxEnv.t,
    dependencies: list(string),
  }
  and build = {
    name: option(string),
    version: option(Version.t),
    mode: BuildSpec.mode,
    buildType: BuildType.t,
    buildCommands: commands,
    installCommands: commands,
    patches: list(patch),
    substs: list(Path.t),
    exportedEnv: ExportedEnv.t,
    buildEnv: BuildEnv.t,
  };

  let convFilter = filter =>
    switch (filter) {
    | None => None
    | Some(filter) => Some(OpamFilter.to_string(filter))
    };

  let convOpamSimpleArg = (simpleArg: OpamTypes.simple_arg) =>
    switch (simpleArg) {
    | OpamTypes.CString(s) => Filename.quote(s)
    | OpamTypes.CIdent(s) => s
    };

  let convOpamArg = ((simpleArg, filter): OpamTypes.arg) => {
    arg: convOpamSimpleArg(simpleArg),
    argfilter: convFilter(filter),
  };

  let convOpamCommand = ((args, filter): OpamTypes.command) => {
    let args = List.map(~f=convOpamArg, args);
    {args, commandfilter: convFilter(filter)};
  };

  let convCommands = commands =>
    switch (commands) {
    | BuildManifest.EsyCommands(commands) => EsyCommands(commands)
    | BuildManifest.OpamCommands(commands) =>
      OpamCommands({opamcommands: List.map(~f=convOpamCommand, commands)})
    | BuildManifest.NoCommands => NoCommands
    };

  let convPatch = ((path, filter)) => {
    path,
    filter: Option.map(~f=OpamFilter.to_string, filter),
  };

  let convPatches = patches => List.map(~f=convPatch, patches);

  let make =
      (
        ~ocamlPkgName,
        ~packageId,
        ~build,
        ~platform,
        ~arch,
        ~sandboxEnv,
        ~mode,
        ~dependencies,
        ~buildCommands,
        (),
      ) => {
    /* include ids of dependencies */
    let dependencies = List.sort(~cmp=String.compare, dependencies);

    let build = {
      let {
        BuildManifest.name,
        version,
        buildType,
        build: _,
        buildDev: _,
        install,
        patches,
        substs,
        exportedEnv,
        buildEnv,
      } = build;
      {
        name,
        version,
        buildType,
        patches: convPatches(patches),
        substs,
        mode,
        buildCommands: convCommands(buildCommands),
        installCommands: convCommands(install),
        exportedEnv,
        buildEnv,
      };
    };

    {
      ocamlPkgName,
      packageId,
      build,
      platform,
      arch,
      sandboxEnv,
      dependencies,
    };
  };

  let toString = repr => {
    let {
      ocamlPkgName,
      packageId,
      sandboxEnv,
      platform,
      arch,
      build,
      dependencies,
    } = repr;

    let hash = {
      /* include parts of the current package metadata which contribute to the
       * build commands/environment */
      let self = build |> build_to_yojson |> Yojson.Safe.to_string;

      let sandboxEnv =
        sandboxEnv |> SandboxEnv.to_yojson |> Yojson.Safe.to_string;

      String.concat(
        "__",
        [
          PackageId.show(packageId),
          System.Platform.show(platform),
          System.Arch.show(arch),
          sandboxEnv,
          self,
          ...dependencies,
        ],
      )
      |> Digest.string
      |> Digest.to_hex
      |> (hash => String.sub(hash, 0, 8));
    };

    let name = PackageId.name(packageId);
    let version = PackageId.version(packageId);

    switch (name == ocamlPkgName, version) {
    | (true, _)
    | (_, Version.Source(_)) =>
      Printf.sprintf("%s-%s", Path.safeSeg(name), hash)
    | (_, Version.Npm(_))
    | (_, Version.Opam(_)) =>
      Printf.sprintf(
        "%s-%s-%s",
        Path.safeSeg(name),
        Path.safePath(Version.show(version)),
        hash,
      )
    };
  };
};

type t = string;

let make =
    (
      ~ocamlPkgName,
      ~packageId,
      ~build,
      ~mode,
      ~platform,
      ~arch,
      ~sandboxEnv,
      ~dependencies,
      ~buildCommands,
      (),
    ) => {
  let repr =
    Repr.make(
      ~ocamlPkgName,
      ~packageId,
      ~build,
      ~mode,
      ~platform,
      ~arch,
      ~sandboxEnv,
      ~dependencies,
      ~buildCommands,
      (),
    );

  (Repr.toString(repr), repr);
};

let pp = Fmt.string;
let show = v => v;
let compare = String.compare;
let to_yojson = Json.Encode.string;
let of_yojson = Json.Decode.string;

module Set = StringSet;
