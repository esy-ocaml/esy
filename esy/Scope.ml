(** Scope exported by a package. *)
module PackageScope : sig
  type t

  val make :
    id:string
    -> sourceType:Manifest.SourceType.t
    -> buildIsInProgress:bool
    -> Sandbox.Package.t
    -> t

  val id : t -> string
  val name : t -> string
  val version : t -> string
  val sourceType : t -> Manifest.SourceType.t

  val buildIsInProgress : t -> bool
  val storePath : t -> Sandbox.Path.t
  val rootPath : t -> Sandbox.Path.t
  val sourcePath : t -> Sandbox.Path.t
  val buildPath : t -> Sandbox.Path.t
  val buildInfoPath : t -> Sandbox.Path.t
  val stagePath : t -> Sandbox.Path.t
  val installPath : t -> Sandbox.Path.t
  val logPath : t -> Sandbox.Path.t

  val buildEnv : t -> (string * string) list
  val exportedEnvLocal : t -> (string * string) list
  val exportedEnvGlobal : t -> (string * string) list

  val var : t -> string -> EsyCommandExpression.Value.t option

end = struct
  type t = {
    id: string;
    pkg : Sandbox.Package.t;
    sourceType : Manifest.SourceType.t;
    buildIsInProgress : bool;
    exportedEnvLocal : (string * string) list;
    exportedEnvGlobal : (string * string) list;
  }

  let make ~id ~sourceType ~buildIsInProgress (pkg : Sandbox.Package.t) =
    let exportedEnvGlobal, exportedEnvLocal =
      let injectCamlLdLibraryPath, exportedEnvGlobal, exportedEnvLocal =
        let f
          (injectCamlLdLibraryPath, exportedEnvGlobal, exportedEnvLocal)
          Manifest.ExportedEnv.{name; scope = envScope; value; exclusive = _}
          =
          match envScope with
          | Manifest.ExportedEnv.Global ->
            let injectCamlLdLibraryPath =
              name <> "CAML_LD_LIBRARY_PATH" && injectCamlLdLibraryPath
            in
            let exportedEnvGlobal = (name, value)::exportedEnvGlobal in
            injectCamlLdLibraryPath, exportedEnvGlobal, exportedEnvLocal
          | Manifest.ExportedEnv.Local ->
            let exportedEnvLocal = (name, value)::exportedEnvLocal in
            injectCamlLdLibraryPath, exportedEnvGlobal, exportedEnvLocal
        in
        List.fold_left ~f ~init:(true, [], []) pkg.build.exportedEnv
      in

      let exportedEnvGlobal =
        if injectCamlLdLibraryPath
        then
          let name = "CAML_LD_LIBRARY_PATH" in
          let value = "#{self.stublibs : self.lib / 'stublibs' : $CAML_LD_LIBRARY_PATH}" in
          (name, value)::exportedEnvGlobal
        else
          exportedEnvGlobal
      in

      let exportedEnvGlobal =
        let path = "PATH", "#{self.bin : $PATH}" in
        let manPath = "MAN_PATH", "#{self.man : $MAN_PATH}" in
        let ocamlpath = "OCAMLPATH", "#{self.lib : $OCAMLPATH}" in
        path::manPath::ocamlpath::exportedEnvGlobal
      in

      exportedEnvGlobal, exportedEnvLocal
    in

    {id; sourceType; pkg; exportedEnvLocal; exportedEnvGlobal; buildIsInProgress}

  let id scope = scope.id
  let name scope = scope.pkg.name
  let version scope = scope.pkg.version
  let sourceType scope = scope.sourceType
  let buildIsInProgress scope = scope.buildIsInProgress

  let sourcePath scope =
    scope.pkg.sourcePath

  let storePath scope =
    match scope.sourceType with
    | Manifest.SourceType.Immutable -> Sandbox.Path.store
    | Manifest.SourceType.Transient -> Sandbox.Path.localStore

  let buildPath scope =
    let storePath = storePath scope in
    Sandbox.Path.(storePath / Store.buildTree / scope.id)

  let buildInfoPath scope =
    let storePath = storePath scope in
    let name = scope.id ^ ".info" in
    Sandbox.Path.(storePath / Store.buildTree / name)

  let stagePath scope =
    let storePath = storePath scope in
    Sandbox.Path.(storePath / Store.stageTree / scope.id)

  let installPath scope =
    let storePath = storePath scope in
    Sandbox.Path.(storePath / Store.installTree / scope.id)

  let logPath scope =
    let storePath = storePath scope in
    let basename = scope.id ^ ".log" in
    Sandbox.Path.(storePath / Store.buildTree / basename)

  let rootPath scope =
    match scope.pkg.build.buildType, scope.sourceType with
    | InSource, _  -> buildPath scope
    | JbuilderLike, Immutable -> buildPath scope
    | JbuilderLike, Transient -> scope.pkg.sourcePath
    | OutOfSource, _ -> scope.pkg.sourcePath
    | Unsafe, Immutable  -> buildPath scope
    | Unsafe, _  -> scope.pkg.sourcePath

  let exportedEnvLocal scope = scope.exportedEnvLocal
  let exportedEnvGlobal scope = scope.exportedEnvGlobal

  let var scope id =
    let b v = Some (EsyCommandExpression.bool v) in
    let s v = Some (EsyCommandExpression.string v) in
    let p v = Some (EsyCommandExpression.string (Sandbox.Value.show (Sandbox.Path.toValue v))) in
    let installPath =
      if scope.buildIsInProgress
      then stagePath scope
      else installPath scope
    in
    match id with
    | "id" -> s scope.id
    | "name" -> s scope.pkg.name
    | "version" -> s scope.pkg.version
    | "root" -> p (rootPath scope)
    | "original_root" -> p (sourcePath scope)
    | "target_dir" -> p (buildPath scope)
    | "install" -> p installPath
    | "bin" -> p Sandbox.Path.(installPath / "bin")
    | "sbin" -> p Sandbox.Path.(installPath / "sbin")
    | "lib" -> p Sandbox.Path.(installPath / "lib")
    | "man" -> p Sandbox.Path.(installPath / "man")
    | "doc" -> p Sandbox.Path.(installPath / "doc")
    | "stublibs" -> p Sandbox.Path.(installPath / "stublibs")
    | "toplevel" -> p Sandbox.Path.(installPath / "toplevel")
    | "share" -> p Sandbox.Path.(installPath / "share")
    | "etc" -> p Sandbox.Path.(installPath / "etc")
    | "dev" -> b (
      match scope.sourceType with
      | EsyBuildPackage.SourceType.Immutable -> false
      | EsyBuildPackage.SourceType.Transient -> true)
    | _ -> None

  let buildEnv scope =
    let installPath =
      if scope.buildIsInProgress
      then stagePath scope
      else installPath scope
    in

    let p v = Sandbox.Value.show (Sandbox.Path.toValue v) in

    (* add builtins *)
    let env =
      [
        "cur__name", scope.pkg.name;
        "cur__version", scope.pkg.version;
        "cur__root", (p (rootPath scope));
        "cur__original_root", (p (sourcePath scope));
        "cur__target_dir", (p (buildPath scope));
        "cur__install", (p installPath);
        "cur__bin", (p Sandbox.Path.(installPath / "bin"));
        "cur__sbin", (p Sandbox.Path.(installPath / "sbin"));
        "cur__lib", (p Sandbox.Path.(installPath / "lib"));
        "cur__man", (p Sandbox.Path.(installPath / "man"));
        "cur__doc", (p Sandbox.Path.(installPath / "doc"));
        "cur__stublibs", (p Sandbox.Path.(installPath / "stublibs"));
        "cur__toplevel", (p Sandbox.Path.(installPath / "toplevel"));
        "cur__share", (p Sandbox.Path.(installPath / "share"));
        "cur__etc", (p Sandbox.Path.(installPath / "etc"));
        "OCAMLFIND_DESTDIR", (p Sandbox.Path.(installPath / "lib"));
        "OCAMLFIND_LDCONF", "ignore";
        "OCAMLFIND_COMMANDS", "ocamlc=ocamlc.opt ocamldep=ocamldep.opt ocamldoc=ocamldoc.opt ocamllex=ocamllex.opt ocamlopt=ocamlopt.opt";
      ]
    in

    let env =
      let f env {Manifest.Env. name; value;} = (name, value)::env in
      List.fold_left ~f ~init:env scope.pkg.build.buildEnv
    in

    let env =
      match scope.pkg.build.buildType with
      | Manifest.BuildType.OutOfSource -> ("DUNE_BUILD_DIR", p (buildPath scope))::env
      | _ -> env
    in

    env

end

type t = {
  platform : System.Platform.t;
  self : PackageScope.t;
  dependencies : t list;
  directDependencies : t StringMap.t;

  sandboxEnv : Sandbox.Environment.Bindings.t;
  finalEnv : Sandbox.Environment.Bindings.t;
}

let make ~platform ~sandboxEnv ~id ~sourceType ~buildIsInProgress pkg =
  let self =
    PackageScope.make
      ~id
      ~sourceType
      ~buildIsInProgress
      pkg
  in
  {
    platform;
    sandboxEnv;
    dependencies = [];
    directDependencies = StringMap.empty;
    self;
    finalEnv = (
      let defaultPath =
          match platform with
          | Windows -> "$PATH;/usr/local/bin;/usr/bin;/bin;/usr/sbin;/sbin"
          | _ -> "$PATH:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
      in
      Sandbox.[
        Environment.Bindings.value "PATH" (Value.v defaultPath);
        Environment.Bindings.value "SHELL" (Value.v "env -i /bin/bash --norc --noprofile");
      ]
    );
  }

let add ~direct ~dep scope =
  let name = PackageScope.name dep.self in
  let directDependencies =
    if direct
    then StringMap.add name dep scope.directDependencies
    else scope.directDependencies
  in
  let dependencies = dep::scope.dependencies in
  {scope with directDependencies; dependencies;}

let storePath scope = PackageScope.storePath scope.self
let rootPath scope = PackageScope.rootPath scope.self
let sourcePath scope = PackageScope.sourcePath scope.self
let buildPath scope = PackageScope.buildPath scope.self
let buildInfoPath scope = PackageScope.buildInfoPath scope.self
let stagePath scope = PackageScope.stagePath scope.self
let installPath scope = PackageScope.installPath scope.self
let logPath scope = PackageScope.logPath scope.self

let exposeUserEnvWith makeBinding name scope =
  let finalEnv =
    match Sys.getenv name with
    | exception Not_found -> scope.finalEnv
    | v ->
      let binding = makeBinding name (Sandbox.Value.v v) in
      binding::scope.finalEnv
  in
  {scope with finalEnv}

let renderCommandExpr ?environmentVariableName scope expr =
  let pathSep =
    match scope.platform with
    | System.Platform.Unknown
    | System.Platform.Darwin
    | System.Platform.Linux
    | System.Platform.Unix
    | System.Platform.Windows
    | System.Platform.Cygwin -> "/"
  in
  let envSep =
    System.Environment.sep ~platform:scope.platform ?name:environmentVariableName ()
  in
  let lookup (namespace, name) =
    match namespace, name with
    | Some "self", name -> PackageScope.var scope.self name
    | Some namespace, name ->
      if namespace = PackageScope.name scope.self
      then PackageScope.var scope.self name
      else
        begin match StringMap.find_opt namespace scope.directDependencies, name with
        | Some _, "installed" -> Some (EsyCommandExpression.bool true)
        | Some scope, name -> PackageScope.var scope.self name
        | None, "installed" -> Some (EsyCommandExpression.bool false)
        | None, _ -> None
        end
    | None, "os" -> Some (EsyCommandExpression.string (System.Platform.show scope.platform))
    | None, _ -> None
  in
  Run.ofStringError (EsyCommandExpression.render ~pathSep ~colon:envSep ~scope:lookup expr)

let makeEnvBindings bindings scope =
  let open Run.Syntax in
  let origin =
    let name = PackageScope.name scope.self in
    let version = PackageScope.version scope.self in
    Printf.sprintf "%s@%s" name version
  in
  let f (name, value) =
    let%bind value =
      Run.contextf
        (renderCommandExpr ~environmentVariableName:name scope value)
        "processing exportedEnv $%s" name
    in
    return (Sandbox.Environment.Bindings.value ~origin name (Sandbox.Value.v value))
  in
  Result.List.map ~f bindings

let buildEnv scope =
  let open Run.Syntax in
  let bindings = PackageScope.buildEnv scope.self in
  let%bind env = makeEnvBindings bindings scope in
  return env

let exportedEnvGlobal scope =
  let open Run.Syntax in
  let bindings = PackageScope.exportedEnvGlobal scope.self in
  let%bind env = makeEnvBindings bindings scope in
  return env

let exportedEnvLocal scope =
  let open Run.Syntax in
  let bindings = PackageScope.exportedEnvLocal scope.self in
  let%bind env = makeEnvBindings bindings scope in
  return env

let env ~includeBuildEnv scope =
  let open Run.Syntax in

  let%bind dependenciesEnv =
    let f env dep =
      let name = PackageScope.name dep.self in
      if StringMap.mem name scope.directDependencies
      then
        let%bind g = exportedEnvGlobal dep in
        let%bind l = exportedEnvLocal dep in
        return (env @ g @ l)
      else
        let%bind g = exportedEnvGlobal dep in
        return (env @ g)
    in
    Run.List.foldLeft ~f ~init:[] scope.dependencies
  in

  let%bind buildEnv =
    buildEnv scope
  in

  return (List.rev (
    scope.finalEnv
    @ (if includeBuildEnv then buildEnv else [])
    @ dependenciesEnv
    @ scope.sandboxEnv
  ))

let toOpamEnv ~ocamlVersion (scope : t) (name : OpamVariable.Full.t) =
  let open OpamVariable in

  let opamArch = System.Arch.(show host) in

  let opamOs =
    match scope.platform with
    | System.Platform.Darwin -> "macos"
    | System.Platform.Linux -> "linux"
    | System.Platform.Cygwin -> "cygwin"
    | System.Platform.Windows -> "win32"
    | System.Platform.Unix -> "unix"
    | System.Platform.Unknown -> "unknown"
  in

  let configPath v = string (Sandbox.Value.show (Sandbox.Path.toValue v)) in

  let opamOsFamily = opamOs in
  let opamOsDistribution = opamOs in

  let opamname (scope : PackageScope.t) =
    let name = PackageScope.name scope in
    match Astring.String.cut ~sep:"@opam/" name with
    | Some ("", name) -> name
    | _ -> name
  in

  let opamPackageScope ?namespace (scope : PackageScope.t) name =
    let opamname = opamname scope in
    let installPath =
      if PackageScope.buildIsInProgress scope
      then PackageScope.stagePath scope
      else PackageScope.installPath scope
    in
    match namespace, name with

    (* some specials for ocaml *)
    | Some "ocaml", "native" -> Some (bool true)
    | Some "ocaml", "native-dynlink" -> Some (bool true)
    | Some "ocaml", "version" ->
      let open Option.Syntax in
      let%bind ocamlVersion = ocamlVersion in
      Some (string ocamlVersion)

    | _, "hash" -> Some (string "")
    | _, "name" -> Some (string opamname)
    | _, "version" -> Some (string (PackageScope.version scope))
    | _, "build-id" -> Some (string (PackageScope.id scope))
    | _, "dev" -> Some (bool (
      match PackageScope.sourceType scope with
      | Manifest.SourceType.Immutable -> false
      | Manifest.SourceType.Transient -> true))
    | _, "prefix" -> Some (configPath installPath)
    | _, "bin" -> Some (configPath Sandbox.Path.(installPath / "bin"))
    | _, "sbin" -> Some (configPath Sandbox.Path.(installPath / "sbin"))
    | _, "etc" -> Some (configPath Sandbox.Path.(installPath / "etc" / opamname))
    | _, "doc" -> Some (configPath Sandbox.Path.(installPath / "doc" / opamname))
    | _, "man" -> Some (configPath Sandbox.Path.(installPath / "man"))
    | _, "share" -> Some (configPath Sandbox.Path.(installPath / "share" / opamname))
    | _, "share_root" -> Some (configPath Sandbox.Path.(installPath / "share"))
    | _, "stublibs" -> Some (configPath Sandbox.Path.(installPath / "stublibs"))
    | _, "toplevel" -> Some (configPath Sandbox.Path.(installPath / "toplevel"))
    | _, "lib" -> Some (configPath Sandbox.Path.(installPath / "lib" / opamname))
    | _, "lib_root" -> Some (configPath Sandbox.Path.(installPath / "lib"))
    | _, "libexec" -> Some (configPath Sandbox.Path.(installPath / "lib" / opamname))
    | _, "libexec_root" -> Some (configPath Sandbox.Path.(installPath / "lib"))
    | _, "build" -> Some (configPath (PackageScope.buildPath scope))
    | _ -> None
  in

  let installPath =
    if PackageScope.buildIsInProgress scope.self
    then PackageScope.stagePath scope.self
    else PackageScope.installPath scope.self
  in
  match Full.scope name, to_string (Full.variable name) with
  | Full.Global, "os" -> Some (string opamOs)
  | Full.Global, "os-family" -> Some (string opamOsFamily)
  | Full.Global, "os-distribution" -> Some (string opamOsDistribution)
  | Full.Global, "os-version" -> Some (string "")
  | Full.Global, "arch" -> Some (string opamArch)
  | Full.Global, "opam-version" -> Some (string "2")
  | Full.Global, "make" -> Some (string "make")
  | Full.Global, "jobs" -> Some (string "4")
  | Full.Global, "pinned" -> Some (bool false)

  | Full.Global, "prefix" -> Some (configPath installPath)
  | Full.Global, "bin" -> Some (configPath Sandbox.Path.(installPath / "bin"))
  | Full.Global, "sbin" -> Some (configPath Sandbox.Path.(installPath / "sbin"))
  | Full.Global, "etc" -> Some (configPath Sandbox.Path.(installPath / "etc"))
  | Full.Global, "doc" -> Some (configPath Sandbox.Path.(installPath / "doc"))
  | Full.Global, "man" -> Some (configPath Sandbox.Path.(installPath / "man"))
  | Full.Global, "share" -> Some (configPath Sandbox.Path.(installPath / "share"))
  | Full.Global, "stublibs" -> Some (configPath Sandbox.Path.(installPath / "stublibs"))
  | Full.Global, "toplevel" -> Some (configPath Sandbox.Path.(installPath / "toplevel"))
  | Full.Global, "lib" -> Some (configPath Sandbox.Path.(installPath / "lib"))
  | Full.Global, "libexec" -> Some (configPath Sandbox.Path.(installPath / "lib"))
  | Full.Global, "version" -> Some (string (PackageScope.version scope.self))
  | Full.Global, "name" -> Some (string (opamname scope.self))

  | Full.Global, _ -> None

  | Full.Self, "enable" -> Some (bool true)
  | Full.Self, "installed" -> Some (bool true)
  | Full.Self, name -> opamPackageScope scope.self name

  | Full.Package namespace, name ->
    let namespace =
      match OpamPackage.Name.to_string namespace with
      | "ocaml" -> "ocaml"
      | namespace -> "@opam/" ^ namespace
    in
    begin match name with
    | "installed" ->
      let installed = StringMap.mem namespace scope.directDependencies in
      Some (bool installed)
    | "enabled" ->
      begin match StringMap.mem namespace scope.directDependencies with
      | true -> Some (string "enable")
      | false -> Some (string "disable")
      end
    | name ->
      if namespace = PackageScope.name scope.self
      then opamPackageScope ~namespace scope.self name
      else
        begin match StringMap.find_opt namespace scope.directDependencies with
        | Some scope -> opamPackageScope ~namespace scope.self name
        | None -> None
        end
    end
