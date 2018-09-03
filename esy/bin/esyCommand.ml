open Esy

(**
 * This module encapsulates info about esy runtime - its version, current
 * working directory and so on.
 *
 * XXX: Probably needs to be merged with Config
 *)
module EsyRuntime = struct

  let currentWorkingDir = Path.v (Sys.getcwd ())
  let currentExecutable = Path.v Sys.executable_name

  let resolve req =
    let open RunAsync.Syntax in
    let%bind currentFilename = Fs.realpath currentExecutable in
    let currentDirname = Path.parent currentFilename in
    let%bind cmd =
      match NodeResolution.resolve req currentDirname with
      | Ok (Some path) -> return path
      | Ok (None) ->
        let msg =
          Printf.sprintf
          "unable to resolve %s from %s"
          req
          (Path.toString currentDirname)
        in
        RunAsync.error msg
      | Error (`Msg err) -> RunAsync.error err
    in return cmd

  let resolveCommand req =
    let open RunAsync.Syntax in
    let%bind path = resolve req in
    return (path |> Cmd.p)

  let fastreplacestringCommand =
    resolveCommand "../../../../bin/fastreplacestring"

  let esyBuildPackageCommand =
    resolveCommand "../../esy-build-package/bin/esyBuildPackageCommand.exe"

  let esyInstallRelease =
    resolve "../../../../bin/esyInstallRelease.js"

  module EsyPackageJson = struct
    type t = {
      version : string
    } [@@deriving of_yojson { strict = false }]

    let read () =
      let pkgJson =
        let open RunAsync.Syntax in
        let%bind filename = resolve "../../../../package.json" in
        let%bind data = Fs.readFile filename in
        Lwt.return (Json.parseStringWith of_yojson data)
      in Lwt_main.run pkgJson
  end

  let version =
    match EsyPackageJson.read () with
    | Ok pkgJson -> pkgJson.EsyPackageJson.version
    | Error err ->
      let msg =
        let err = Run.formatError err in
        Printf.sprintf "invalid esy installation: cannot read package.json %s" err in
      failwith msg

  let concurrency =
    (** TODO: handle more platforms, right now this is tested only on macOS and Linux *)
    let cmd = Bos.Cmd.(v "getconf" % "_NPROCESSORS_ONLN") in
    match Bos.OS.Cmd.(run_out cmd |> to_string) with
    | Ok out ->
      begin match out |> String.trim |> int_of_string_opt with
      | Some n -> n
      | None -> 1
      end
    | Error _ -> 1
end

module CommonOptions = struct
  open Cmdliner

  type t = {
    cfg : Config.t;
    installation : EsyInstall.Sandbox.t EsyInstall.Project.project
  }

  let docs = Manpage.s_common_options

  let prefixPath =
    let doc = "Specifies esy prefix path." in
    let env = Arg.env_var "ESY__PREFIX" ~doc in
    Arg.(
      value
      & opt (some Cli.pathConv) None
      & info ["prefix-path"; "P"] ~env ~docs ~doc
    )

  let sandboxPath =
    let doc = "Specifies esy sandbox path." in
    let env = Arg.env_var "ESY__SANDBOX" ~doc in
    Arg.(
      value
      & opt (some Cli.pathConv) None
      & info ["sandbox-path"; "S"] ~env ~docs ~doc
    )

  let opamRepositoryArg =
    let doc = "Specifies an opam repository to use." in
    let docv = "REMOTE[:LOCAL]" in
    let env = Arg.env_var "ESYI__OPAM_REPOSITORY" ~doc in
    Arg.(
      value
      & opt (some Cli.checkoutConv) None
      & (info ["opam-repository"] ~env ~doc ~docv)
    )

  let esyOpamOverrideArg =
    let doc = "Specifies an opam override repository to use." in
    let docv = "REMOTE[:LOCAL]" in
    let env = Arg.env_var "ESYI__OPAM_OVERRIDE"  ~doc in
    Arg.(
      value
      & opt (some Cli.checkoutConv) None
      & info ["opam-override-repository"] ~env ~doc ~docv
    )

  let cacheTarballsPath =
    let doc = "Specifies tarballs cache directory." in
    Arg.(
      value
      & opt (some Cli.pathConv) None
      & info ["cache-tarballs-path"] ~doc
    )

  let npmRegistryArg =
    let doc = "Specifies npm registry to use." in
    let env = Arg.env_var "NPM_CONFIG_REGISTRY" ~doc in
    Arg.(
      value
      & opt (some string) None
      & info ["npm-registry"] ~env ~doc
    )

  let solveTimeoutArg =
    let doc = "Specifies timeout for running depsolver." in
    Arg.(
      value
      & opt (some float) None
      & info ["solve-timeout"] ~doc
    )

  let skipRepositoryUpdateArg =
    let doc = "Skip updating opam-repository and esy-opam-overrides repositories." in
    Arg.(
      value
      & flag
      & info ["skip-repository-update"] ~doc
    )

  let cachePathArg =
    let doc = "Specifies cache directory.." in
    let env = Arg.env_var "ESYI__CACHE" ~doc in
    Arg.(
      value
      & opt (some Cli.pathConv) None
      & info ["cache-path"] ~env ~doc
    )

  let resolveSandoxPath () =
    let open RunAsync.Syntax in

    let%bind currentPath = RunAsync.ofRun (Path.current ()) in

    let rec climb path =
      if%bind Sandbox.isSandbox path
      then return path
      else
        let parent = Path.parent path in
        if not (Path.equal path parent)
        then climb (Path.parent path)
        else
          let%bind msg = RunAsync.ofRun (
            let open Run.Syntax in
            let%bind currentPath = Path.toPrettyString currentPath in
            let msg = Printf.sprintf "No sandbox found (from %s and up)" currentPath in
            return msg
          ) in error msg
    in
    let%bind sandboxPath = climb currentPath in
    return sandboxPath

  let term =
    let parse
      prefixPath
      sandboxPath
      cachePath
      cacheTarballsPath
      opamRepository
      esyOpamOverride
      npmRegistry
      solveTimeout
      skipRepositoryUpdate
      =
      let copts =
        let open RunAsync.Syntax in
        let%bind sandboxPath =
          match sandboxPath with
          | Some v -> return v
          | None -> resolveSandoxPath ()
        in
        let%bind prefixPath = match prefixPath with
          | Some prefixPath -> return (Some prefixPath)
          | None ->
            let%bind rc = EsyRc.ofPath sandboxPath in
            return rc.EsyRc.prefixPath
        in

        let%bind cfg =
          let%bind esyBuildPackageCommand =
            let%bind cmd = EsyRuntime.esyBuildPackageCommand in
            return (Cmd.v cmd)
          in
          let%bind fastreplacestringCommand =
            let%bind cmd = EsyRuntime.fastreplacestringCommand in
            return (Cmd.v cmd)
          in
          RunAsync.ofRun (
            Config.create
              ~esyBuildPackageCommand
              ~fastreplacestringCommand
              ~esyVersion:EsyRuntime.version ~prefixPath sandboxPath
          )
        in

        let%bind installation =
          let open EsyInstall in
          let createProgressReporter ~name () =
            let progress msg =
              let status = Format.asprintf ".... %s %s" name msg in
              Cli.Progress.setStatus status
            in
            let finish () =
              let%lwt () = Cli.Progress.clearStatus () in
              Logs_lwt.app (fun m -> m "%s: done" name)
            in
            (progress, finish)
          in
          let%bind esySolveCmd =
            match System.Platform.host with
            | Windows ->
              return (Cmd.v "esy-solve-cudf/esySolveCudfCommand.exe")
            | _ ->
              let%bind cmd = EsyRuntime.resolve "esy-solve-cudf/esySolveCudfCommand.exe" in
              return Cmd.(v (p cmd))
          in
          let%bind cfg =
            Config.make
              ~esySolveCmd
              ~createProgressReporter
              ~skipRepositoryUpdate
              ?cachePath
              ?cacheTarballsPath
              ?npmRegistry
              ?opamRepository
              ?esyOpamOverride
              ?solveTimeout
              ()
          in
          match%bind Project.ofDir sandboxPath with
          | Some desc -> Project.initWith (Sandbox.make ~cfg) desc
          | None -> error "no sandboxes found"
        in
        let%bind () = Config.init cfg in
        return {cfg; installation}
      in
      match Lwt_main.run copts with
      | Ok v -> `Ok v
      | Error msg -> `Error (false, Run.formatError msg)
    in
    Term.(ret (
      const parse
      $ prefixPath
      $ sandboxPath
      $ cachePathArg
      $ cacheTarballsPath
      $ opamRepositoryArg
      $ esyOpamOverrideArg
      $ npmRegistryArg
      $ solveTimeoutArg
      $ skipRepositoryUpdateArg
    ))

end

let resolvedPathTerm =
  let open Cmdliner in
  let parse v =
    match Path.ofString v with
    | Ok path ->
      if Path.isAbs path then
        Ok path
      else
        Ok Path.(EsyRuntime.currentWorkingDir // path |> normalize)
    | err -> err
  in
  let print = Path.pp in
  Arg.conv ~docv:"PATH" (parse, print)

let pkgPathTerm =
  let open Cmdliner in
  let doc = "Path to package." in
  Arg.(
    value
    & pos 0  (some resolvedPathTerm) None
    & info [] ~doc
  )

let withBuildTaskByPath
    ~(info : SandboxInfo.t)
    packagePath
    f =
  let open RunAsync.Syntax in
  match packagePath with
  | Some packagePath ->
    let resolvedPath = packagePath |> Path.remEmptySeg |> Path.toString in
    let findByPath (task : Task.t) =
      let pkg = Task.pkg task in
      String.equal resolvedPath pkg.id
    in
    begin match Task.Graph.find ~f:findByPath info.task with
      | None ->
        let msg = Printf.sprintf "No package found at %s" resolvedPath in
        error msg
      | Some pkg -> f pkg
    end
  | None -> f info.task

let buildPlan {CommonOptions. cfg; _} packagePath () =
  let open RunAsync.Syntax in

  let%bind info = SandboxInfo.ofConfig cfg in

  let f task =
    let json = EsyBuildPackage.Plan.to_yojson (Task.plan task) in
    let data = Yojson.Safe.pretty_to_string json in
    print_endline data;
    return ()
  in
  withBuildTaskByPath ~info packagePath f

let buildShell {CommonOptions. cfg; _} packagePath () =
  let open RunAsync.Syntax in

  let%bind info = SandboxInfo.ofConfig cfg in

  let f task =
    let%bind () = Build.buildDependencies ~concurrency:EsyRuntime.concurrency cfg task in
    match%bind PackageBuilder.buildShell cfg task with
    | Unix.WEXITED 0 -> return ()
    | Unix.WEXITED n
    | Unix.WSTOPPED n
    | Unix.WSIGNALED n -> exit n
  in withBuildTaskByPath ~info packagePath f

let buildPackage {CommonOptions. cfg; _} packagePath () =
  let open RunAsync.Syntax in

  let%bind info = SandboxInfo.ofConfig cfg in

  let f task =
    Build.buildAll ~concurrency:EsyRuntime.concurrency ~force:`ForRoot cfg task
  in withBuildTaskByPath ~info packagePath f

let build ?(buildOnly=true) {CommonOptions. cfg; _} cmd () =
  let open RunAsync.Syntax in
  let%bind {SandboxInfo. task; _} = SandboxInfo.ofConfig cfg in

  (** TODO: figure out API to build devDeps in parallel with the root *)

  match cmd with
  | None ->
    let%bind () =
      Build.buildDependencies
        ~concurrency:EsyRuntime.concurrency
        ~force:`ForRoot
        cfg task
    in Build.buildTask ~force:true ~stderrout:`Keep ~quiet:true ~buildOnly cfg task

  | Some cmd ->
    let%bind () = Build.buildDependencies ~concurrency:EsyRuntime.concurrency cfg task in
    match%bind PackageBuilder.buildExec cfg task cmd with
    | Unix.WEXITED 0 -> return ()
    | Unix.WEXITED n
    | Unix.WSTOPPED n
    | Unix.WSIGNALED n -> exit n

let makeEnvCommand ~computeEnv ~header {CommonOptions. cfg; _} asJson packagePath () =
  let open RunAsync.Syntax in

  let%bind info = SandboxInfo.ofConfig cfg in

  let f (task : Task.t) =
    let%bind source = RunAsync.ofRun (
      let open Run.Syntax in
      let%bind env = computeEnv cfg task in
      let pkg = Task.pkg task in
      let header = header pkg in
      if asJson
      then
        let%bind env = Run.ofStringError (Environment.Bindings.eval env) in
        Ok (
          env
          |> Environment.to_yojson
          |> Yojson.Safe.pretty_to_string)
      else
        Environment.renderToShellSource
          ~header
          env
      ) in
    let%lwt () = Lwt_io.print source in
    return ()
  in withBuildTaskByPath ~info packagePath f

let buildEnv =
  let open Run.Syntax in
  let header (pkg : Package.t) =
    Printf.sprintf "# Build environment for %s@%s" pkg.name pkg.version
  in
  let computeEnv cfg task =
    let%bind env = Task.buildEnv task in
    let env = Config.Environment.Bindings.render cfg.Config.buildConfig env in
    return env
  in
  makeEnvCommand ~computeEnv ~header

let commandEnv =
  let open Run.Syntax in
  let header (pkg : Package.t) =
    Printf.sprintf "# Command environment for %s@%s" pkg.name pkg.version
  in
  let computeEnv cfg task =
    let%bind env = Task.commandEnv task in
    let env = Config.Environment.Bindings.render cfg.Config.buildConfig env in
    return (Environment.current @ env)
  in
  makeEnvCommand ~computeEnv ~header

let sandboxEnv =
  let open Run.Syntax in
  let header (pkg : Package.t) =
    Printf.sprintf "# Sandbox environment for %s@%s" pkg.name pkg.version
  in
  let computeEnv cfg task =
    let%bind env = Task.sandboxEnv task in
    let env = Config.Environment.Bindings.render cfg.Config.buildConfig env in
    Ok (Environment.current @ env)
  in
  makeEnvCommand ~computeEnv ~header

let makeExecCommand
    ?(checkIfDependenciesAreBuilt=false)
    ~env
    ~cfg
    ~info
    cmd
    ()
  =
  let open RunAsync.Syntax in
  let {SandboxInfo. task; commandEnv; sandboxEnv; _} = info in

  let%bind () =
    if checkIfDependenciesAreBuilt
    then Build.buildDependencies ~concurrency:EsyRuntime.concurrency cfg task
    else return ()
  in

  let%bind env = RunAsync.ofStringError (
    let open Result.Syntax in
    let env = match env with
      | `CommandEnv -> commandEnv
      | `SandboxEnv -> sandboxEnv
    in
    let env = Environment.current @ env in
    let%bind env = Environment.Bindings.eval env in
    return (`CustomEnv env)
  ) in

  let cmd =
    let tool, args = Cmd.getToolAndArgs cmd in
    match tool with
    | "esy" -> Cmd.(v (p EsyRuntime.currentExecutable) |> addArgs args)
    | _ -> cmd
  in

  let%bind status =
    ChildProcess.runToStatus
      ~env
      ~resolveProgramInEnv:true
      ~stderr:(`FD_copy Unix.stderr)
      ~stdout:(`FD_copy Unix.stdout)
      ~stdin:(`FD_copy Unix.stdin)
      cmd
  in
  match status with
  | Unix.WEXITED n
  | Unix.WSTOPPED n
  | Unix.WSIGNALED n -> exit n

let exec (copts : CommonOptions.t) cmd () =
  let open RunAsync.Syntax in
  let%bind info = SandboxInfo.ofConfig copts.cfg in
  let%bind () =
    let installPath =
      Config.Path.toPath
        copts.cfg.buildConfig
        (Task.installPath info.SandboxInfo.task)
    in
    if%bind Fs.exists installPath then
      return ()
    else
      build ~buildOnly:false copts None ()
  in
  makeExecCommand
    ~env:`SandboxEnv
    ~cfg:copts.cfg
    ~info
    cmd
    ()

let devExec {CommonOptions. cfg; _} cmd () =
  let open RunAsync.Syntax in
  let%bind info = SandboxInfo.ofConfig cfg in
  let%bind cmd = RunAsync.ofRun (
    let open Run.Syntax in
    let tool, args = Cmd.getToolAndArgs cmd in
    let script =
      Manifest.Scripts.find
        tool
        info.SandboxInfo.sandbox.scripts
    in
    let renderCommand (cmd : Manifest.CommandList.Command.t) =
      match cmd with
      | Manifest.CommandList.Command.Parsed args ->
        let%bind args = Result.List.map ~f:(Task.renderExpression ~cfg ~task:info.task) args in
        return (Cmd.ofListExn args)
      | Manifest.CommandList.Command.Unparsed line ->
        let%bind string = Task.renderExpression ~cfg ~task:info.task line in
        let%bind args = ShellSplit.split string in
        return (Cmd.ofListExn args)
    in
    match script with
    | None -> return cmd
    | Some {command; _} ->
      let%bind command = renderCommand command in
      return (Cmd.addArgs args command)
  ) in
  makeExecCommand
    ~checkIfDependenciesAreBuilt:true
    ~env:`CommandEnv
    ~cfg
    ~info
    cmd
    ()

let devShell {CommonOptions. cfg; _} () =
  let open RunAsync.Syntax in
  let shell =
    try Sys.getenv "SHELL"
    with Not_found -> "/bin/bash"
  in
  let%bind info = SandboxInfo.ofConfig cfg in
  makeExecCommand
    ~env:`CommandEnv
    ~cfg
    ~info
    (Cmd.v shell)
    ()

let makeLsCommand ~computeTermNode ~includeTransitive cfg (info: SandboxInfo.t) =
  let open RunAsync.Syntax in

  let seen = ref StringSet.empty in

  let f ~foldDependencies _prev (task : Task.t) =
    let id = Task.id task in
    if StringSet.mem id !seen then
      return None
    else (
      seen := StringSet.add id !seen;
      let%bind children =
        if not includeTransitive && id <> (Task.id info.task) then
          return []
        else
          foldDependencies ()
          |> List.map ~f:(fun (_, v) -> v)
          |> RunAsync.List.joinAll
      in
      let children = children |> List.filterNone in
      computeTermNode ~cfg task children
    )
  in

  match%bind Task.Graph.fold ~f ~init:(return None) info.task with
  | Some tree -> return (print_endline (TermTree.toString tree))
  | None -> return ()

let formatPackageInfo ~built:(built : bool)  (task : Task.t) =
  let open RunAsync.Syntax in
  let pkg = Task.pkg task in
  let version = Chalk.grey ("@" ^ pkg.version) in
  let status =
    match (Task.sourceType task), built with
    | Manifest.SourceType.Immutable, true ->
      Chalk.green "[built]"
    | _, _ ->
      Chalk.blue "[build pending]"
  in
  let line = Printf.sprintf "%s%s %s" pkg.name version status in
  return line

let lsBuilds {CommonOptions. cfg; _} includeTransitive () =
  let open RunAsync.Syntax in

  let%bind (info : SandboxInfo.t) = SandboxInfo.ofConfig cfg in

  let computeTermNode ~cfg task children =
    let%bind built = Task.isBuilt ~cfg task in
    let%bind line = formatPackageInfo ~built task in
    return (Some (TermTree.Node { line; children; }))
  in
  makeLsCommand ~computeTermNode ~includeTransitive cfg info

let lsLibs {CommonOptions. cfg; _} includeTransitive () =
  let open RunAsync.Syntax in

  let%bind (info : SandboxInfo.t) = SandboxInfo.ofConfig cfg in

  let%bind ocamlfind =
    let%bind p = SandboxInfo.ocamlfind ~cfg info in
    return Path.(p / "bin" / "ocamlfind")
  in
  let%bind builtIns = SandboxInfo.libraries ~cfg ~ocamlfind () in

  let computeTermNode ~cfg (task: Task.t) children =
    let%bind built = Task.isBuilt ~cfg task in
    let%bind line = formatPackageInfo ~built task in

    let%bind libs =
      if built then
        SandboxInfo.libraries ~cfg ~ocamlfind ~builtIns ~task ()
      else
        return []
    in

    let libs =
      libs
      |> List.map ~f:(fun lib ->
          let line = Chalk.yellow(lib) in
          TermTree.Node { line; children = []; }
        )
    in

    return (Some (TermTree.Node { line; children = libs @ children; }))
  in
  makeLsCommand ~computeTermNode ~includeTransitive cfg info

let lsModules {CommonOptions. cfg; _} only () =
  let open RunAsync.Syntax in

  let%bind (info : SandboxInfo.t) = SandboxInfo.ofConfig cfg in

  let%bind ocamlfind =
    let%bind p = SandboxInfo.ocamlfind ~cfg info in
    return Path.(p / "bin" / "ocamlfind")
  in
  let%bind ocamlobjinfo =
    let%bind p = SandboxInfo.ocaml ~cfg info in
    return Path.(p / "bin" / "ocamlobjinfo")
  in
  let%bind builtIns = SandboxInfo.libraries ~cfg ~ocamlfind () in

  let formatLibraryModules ~cfg ~task lib =
    let%bind meta = SandboxInfo.Findlib.query ~cfg ~ocamlfind ~task lib in
    let open SandboxInfo.Findlib in

    if String.length(meta.archive) == 0 then
      let description = Chalk.dim(meta.description) in
      return [TermTree.Node { line=description; children=[]; }]
    else begin
      Path.ofString (meta.location ^ Path.dirSep ^ meta.archive) |> function
      | Ok archive ->
        if%bind Fs.exists archive then begin
          let archive = Path.toString archive in
          let%bind lines =
            SandboxInfo.modules ~ocamlobjinfo archive
          in

          let modules =
            let isPublicModule name =
              not (Astring.String.is_infix ~affix:"__" name)
            in
            let toTermNode name =
              let line = Chalk.cyan name in
              TermTree.Node { line; children=[]; }
            in
            lines
            |> List.filter ~f:isPublicModule
            |> List.map ~f:toTermNode
          in

          return modules
        end else
          return []
      | Error `Msg msg -> error msg
    end
  in

  let computeTermNode ~cfg (task: Task.t) children =
    let%bind built = Task.isBuilt ~cfg task in
    let%bind line = formatPackageInfo ~built task in

    let%bind libs =
      if built then
        SandboxInfo.libraries ~cfg ~ocamlfind ~builtIns ~task ()
      else
        return []
    in

    let isNotRoot = Task.id task <> Task.id info.task in
    let constraintsSet = List.length only <> 0 in
    let noMatchedLibs = List.length (List.intersect only libs) = 0 in

    if isNotRoot && constraintsSet && noMatchedLibs then
      return None
    else
      let%bind libs =
        libs
        |> List.filter ~f:(fun lib ->
            if List.length only = 0 then
              true
            else
              List.mem lib ~set:only
          )
        |> List.map ~f:(fun lib ->
            let line = Chalk.yellow(lib) in
            let%bind children =
              formatLibraryModules ~cfg ~task lib
            in
            return (TermTree.Node { line; children; })
          )
        |> RunAsync.List.joinAll
      in

      return (Some (TermTree.Node { line; children = libs @ children; }))
  in
  makeLsCommand ~computeTermNode ~includeTransitive:false cfg info

(* let lockfilePath (sandbox : EsyInstall.Sandbox.t) = *)
(*   let open RunAsync.Syntax in *)
(*   let filename = Path.(sandbox.path / "esyi.lock.json") in *)
(*   if%bind Fs.exists filename *)
(*   then *)
(*     let%lwt () = *)
(*       Logs_lwt.warn *)
(*         (fun m -> m "found esyi.lock.json, please rename it to esy.lock.json") in *)
(*     return filename *)
(*   else *)
(*     return Path.(sandbox.path / "esy.lock.json") *)

let getSandboxSolution installSandbox =
  let open EsyInstall in
  let open RunAsync.Syntax in
  let%bind solution = Solver.solve installSandbox in
  let%bind lockfilePath = Sandbox.lockfilePath installSandbox in
  let%bind () =
    Solution.LockfileV1.toFile ~sandbox:installSandbox ~solution lockfilePath
  in
  return solution

let solveSandbox sandbox =
  let open RunAsync.Syntax in
  let%bind _ : EsyInstall.Solution.t = getSandboxSolution sandbox in
  return ()

let solve {CommonOptions. installation; _} () =
  let f _name sandbox = solveSandbox sandbox in
  EsyInstall.Project.forEach f installation

let fetchSandbox sandbox =
  let open EsyInstall in
  let open RunAsync.Syntax in
  let%bind lockfilePath = Sandbox.lockfilePath sandbox in
  match%bind Solution.LockfileV1.ofFile ~sandbox lockfilePath with
  | Some solution -> Fetch.fetch ~sandbox solution
  | None -> error "no lockfile found, run 'esy solve' first"

let fetch {CommonOptions. installation; _} () =
  let open EsyInstall in
  let f _name sandbox = fetchSandbox sandbox in
  Project.forEach f installation

let solveAndFetch {CommonOptions. installation; _} () =
  let open EsyInstall in
  let open RunAsync.Syntax in
  let f _name sandbox =
    let%bind lockfilePath = Sandbox.lockfilePath sandbox in
    match%bind Solution.LockfileV1.ofFile ~sandbox lockfilePath with
    | Some solution ->
      if%bind Fetch.isInstalled ~sandbox solution
      then return ()
      else fetchSandbox sandbox
    | None ->
      let%bind () = solveSandbox sandbox in
      let%bind () = fetchSandbox sandbox in
      return ()
  in
  Project.forEach f installation

(* let add ({CommonOptions. installSandbox; _} as copts) (packages : string list) () = *)
(*   let open EsyInstall in *)
(*   let open RunAsync.Syntax in *)
(*   let module NpmDependencies = Package.NpmDependencies in *)
(*   let aggOpamErrorMsg = *)
(*     "The esy add command doesn't work with opam sandboxes. " *)
(*     ^ "Please send a pull request to fix this!" *)
(*   in *)
(*   let makeReqs ?(specFun= fun _ -> "") names = *)
(*     names *)
(*     |> Result.List.map *)
(*         ~f:(fun name -> *)
(*               let spec = specFun name in *)
(*               Package.Req.make ~name ~spec) *)
(*     |> RunAsync.ofStringError *)
(*   in *)

(*   let%bind reqs = makeReqs packages in *)

(*   let addReqs origDeps = *)
(*     let open Package.Dependencies in *)
(*     match origDeps with *)
(*     | NpmFormula prevReqs -> return (NpmFormula (reqs @ prevReqs)) *)
(*     | OpamFormula _ -> error aggOpamErrorMsg *)
(*   in *)

(*   let%bind installSandbox = *)
(*     let%bind combinedDeps = addReqs installSandbox.root.dependencies in *)
(*     let%bind sbDeps = addReqs installSandbox.dependencies in *)
(*     let root = { installSandbox.root with dependencies = combinedDeps } in *)
(*     return { installSandbox with root; dependencies = sbDeps } *)
(*   in *)

(*   let copts = {copts with installSandbox} in *)

(*   let%bind solution = solveSandbox installSandbox in *)
(*   let%bind () = fetch copts () in *)

(*   let%bind addedDependencies, configPath = *)
(*     let records = *)
(*       let f (record : Solution.Record.t) map = *)
(*         StringMap.add record.name record map *)
(*       in *)
(*       Solution.Record.Set.fold f (Solution.records solution) StringMap.empty *)
(*     in *)
(*     let addedDependencies = *)
(*       let f name = *)
(*         match StringMap.find name records with *)
(*         | Some record -> *)
(*           let constr = *)
(*             match record.Solution.Record.version with *)
(*             | Package.Version.Npm version -> *)
(*               SemverVersion.Formula.DNF.toString *)
(*                 (SemverVersion.caretRangeOfVersion version) *)
(*             | Package.Version.Opam version -> *)
(*               OpamPackage.Version.to_string version *)
(*             | Package.Version.Source _ -> *)
(*               Package.Version.toString record.Solution.Record.version *)
(*           in *)
(*           name, `String constr *)
(*         | None -> assert false *)
(*       in *)
(*       List.map ~f packages *)
(*     in *)
(*     let%bind path = *)
(*       match copts.installSandbox.Sandbox.origin with *)
(*       | Esy path -> return path *)
(*       | Opam _ -> error aggOpamErrorMsg *)
(*       | AggregatedOpam _ -> error aggOpamErrorMsg *)
(*       in *)
(*       return (addedDependencies, path) *)
(*     in *)
(*     let%bind json = *)
(*       let keyToUpdate = "dependencies" in *)
(*       let%bind json = Fs.readJsonFile configPath in *)
(*         let%bind json = *)
(*           RunAsync.ofStringError ( *)
(*             let open Result.Syntax in *)
(*             let%bind items = Json.Parse.assoc json in *)
(*             let%bind items = *)
(*               let f (key, json) = *)
(*                 if key = keyToUpdate *)
(*                 then *)
(*                     let%bind dependencies = *)
(*                       Json.Parse.assoc json in *)
(*                     let dependencies = *)
(*                       Json.mergeAssoc dependencies *)
(*                         addedDependencies in *)
(*                     return *)
(*                       (key, (`Assoc dependencies)) *)
(*                 else return (key, json) *)
(*               in *)
(*               Result.List.map ~f items *)
(*             in *)
(*             let json = `Assoc items *)
(*             in return json *)
(*           ) in *)
(*         return json *)
(*       in *)
(*       let%bind () = Fs.writeJsonFile ~json configPath in *)
(*       return () *)

let dependenciesForExport (task : Task.t) =
  let f deps dep = match dep with
    | Task.Dependency depTask
    | Task.BuildTimeDependency depTask ->
      begin match Task.sourceType depTask with
      | Manifest.SourceType.Immutable -> (depTask, dep)::deps
      | _ -> deps
      end
    | Task.DevDependency _ -> deps
  in
  Task.dependencies task
  |> List.fold_left ~f ~init:[]
  |> List.rev

let exportBuild {CommonOptions. cfg; _} buildPath () =
  let outputPrefixPath = Path.(EsyRuntime.currentWorkingDir / "_export") in
  Task.exportBuild ~outputPrefixPath ~cfg buildPath

let exportDependencies {CommonOptions. cfg; _} () =
  let open RunAsync.Syntax in

  let%bind {SandboxInfo. task = rootTask; _} = SandboxInfo.ofConfig cfg in

  let tasks =
    rootTask
    |> Task.Graph.traverse ~traverse:dependenciesForExport
    |> List.filter ~f:(fun (task : Task.t) -> not (Task.id task = Task.id rootTask))
  in

  let queue = LwtTaskQueue.create ~concurrency:8 () in

  let exportBuild (task : Task.t) =
    let pkg = Task.pkg task in
    let aux () =
      let%lwt () = Logs_lwt.app (fun m -> m "Exporting %s@%s" pkg.name pkg.version) in
      let buildPath = Config.Path.toPath cfg.Config.buildConfig (Task.installPath task) in
      if%bind Fs.exists buildPath
      then
        let outputPrefixPath = Path.(EsyRuntime.currentWorkingDir / "_export") in
        Task.exportBuild ~outputPrefixPath ~cfg buildPath
      else (
        errorf
          "%s@%s was not built, run 'esy build' first"
          pkg.name
          pkg.version
      )
    in LwtTaskQueue.submit queue aux
  in

  tasks
  |> List.map ~f:exportBuild
  |> RunAsync.List.waitAll

let importBuild {CommonOptions. cfg; _} fromPath buildPaths () =
  let open RunAsync.Syntax in
  let%bind buildPaths = match fromPath with
  | Some fromPath ->
    let%bind lines = Fs.readFile fromPath in
    return (
      buildPaths @ (
      lines
      |> String.split_on_char '\n'
      |> List.filter ~f:(fun line -> String.trim line <> "")
      |> List.map ~f:(fun line -> Path.v line))
    )
  | None -> return buildPaths
  in
  let queue = LwtTaskQueue.create ~concurrency:8 () in
  buildPaths
  |> List.map ~f:(fun path -> LwtTaskQueue.submit queue (fun () -> Task.importBuild cfg path))
  |> RunAsync.List.waitAll

let importDependencies {CommonOptions. cfg; _} fromPath () =
  let open RunAsync.Syntax in

  let%bind {SandboxInfo. task = rootTask; _} = SandboxInfo.ofConfig cfg in

  let fromPath = match fromPath with
    | Some fromPath -> fromPath
    | None -> Path.(cfg.Config.buildConfig.sandboxPath / "_export")
  in

  let pkgs =
    rootTask
    |> Task.Graph.traverse ~traverse:dependenciesForExport
    |> List.filter ~f:(fun (task : Task.t) -> not (Task.id task = Task.id rootTask))
  in

  let queue = LwtTaskQueue.create ~concurrency:16 () in

  let importBuild (task : Task.t) =
    let aux () =
      let installPath = Config.Path.toPath cfg.buildConfig (Task.installPath task) in
      if%bind Fs.exists installPath
      then return ()
      else (
        let id = Task.id task in
        let pathDir = Path.(fromPath / id) in
        let pathTgz = Path.(fromPath / (id ^ ".tar.gz")) in
        if%bind Fs.exists pathDir
        then Task.importBuild cfg pathDir
        else if%bind Fs.exists pathTgz
        then Task.importBuild cfg pathTgz
        else
          let%lwt () =
            Logs_lwt.warn(fun m -> m "no prebuilt artifact found for %s" id)
          in return ()
      )
    in LwtTaskQueue.submit queue aux
  in

  pkgs
  |> List.map ~f:importBuild
  |> RunAsync.List.waitAll

let release ({CommonOptions. cfg; _} as copts) () =
  let open RunAsync.Syntax in
  let%bind info = SandboxInfo.ofConfig cfg in

  let%bind outputPath =
    let outputDir = "_release" in
    let outputPath = Path.(cfg.Config.buildConfig.sandboxPath / outputDir) in
    let%bind () = Fs.rmPath outputPath in
    return outputPath
  in

  let%bind () = build copts None () in

  let%bind esyInstallRelease = EsyRuntime.esyInstallRelease in

  let%bind ocamlopt =
    let%bind p = SandboxInfo.ocaml ~cfg info in
    return Path.(p / "bin" / "ocamlopt")
  in

  NpmRelease.make
    ~ocamlopt
    ~esyInstallRelease
    ~outputPath
    ~concurrency:EsyRuntime.concurrency
    ~cfg
    ~sandbox:info.SandboxInfo.sandbox

let makeCommand
  ?(header=`Standard)
  ?(sdocs=Cmdliner.Manpage.s_common_options)
  ?docs
  ?doc
  ?(version=EsyRuntime.version)
  ?(exits=Cmdliner.Term.default_exits)
  ~name
  cmd =
  let info =
    Cmdliner.Term.info
      ~exits
      ~sdocs
      ?docs
      ?doc
      ~version
      name
  in

  let printHeader () =
    match header with
    | `Standard -> Logs_lwt.app (fun m -> m "%s %s" name version);
    | `No -> Lwt.return ()
  in

  let cmd =
    let f comp =
      let res =
        printHeader ();%lwt
        comp
      in
      match Lwt_main.run res with
      | Ok () -> `Ok ()
      | Error error ->
        let msg = Run.formatError error in
        let msg = Printf.sprintf "error, exiting...\n%s" msg in
        `Error (false, msg)
    in
    Cmdliner.Term.(ret (app (const f) cmd))
  in

  cmd, info

let makeAlias command alias =
  let term, info = command in
  let name = Cmdliner.Term.name info in
  let doc = Printf.sprintf "An alias for $(b,%s) command" name in
  term, Cmdliner.Term.info alias ~version:EsyRuntime.version ~doc

let () =
  let open Cmdliner in

  let defaultCommand =
    let run copts cmd () =
      let open RunAsync.Syntax in
      match cmd with
      | Some cmd ->
        devExec copts cmd ()
      | None ->
        Logs_lwt.app (fun m -> m "esy %s" EsyRuntime.version);%lwt
        let%bind () = solveAndFetch copts () in
        build copts None ()
    in
    let cmdTerm =
      Cli.cmdOptionTerm
        ~doc:"Command to execute within the sandbox environment."
        ~docv:"COMMAND"
    in
    makeCommand
      ~header:`No
      ~name:"esy"
      ~doc:"package.json workflow for native development with Reason/OCaml"
      Term.(const run $ CommonOptions.term $ cmdTerm $ Cli.setupLogTerm)
  in

  let commands =

    let buildCommand =

      let run copts cmd () =
        let%lwt () =
          match cmd with
          | None -> Logs_lwt.app (fun m -> m "esy build %s" EsyRuntime.version)
          | Some _ -> Lwt.return ()
        in
        build ~buildOnly:true copts cmd ()
      in

      makeCommand
        ~header:`No
        ~name:"build"
        ~doc:"Build the entire sandbox"
        Term.(
          const run
          $ CommonOptions.term
          $ Cli.cmdOptionTerm
              ~doc:"Command to execute within the build environment."
              ~docv:"COMMAND"
          $ Cli.setupLogTerm
        )
    in

    let installCommand =
      makeCommand
        ~name:"install"
        ~doc:"Solve & fetch dependencies"
        Term.(
          const solveAndFetch
          $ CommonOptions.term
          $ Cli.setupLogTerm
        )
    in

    [
    (* commands *)

    installCommand;
    buildCommand;

    makeCommand
      ~header:`No
      ~name:"build-plan"
      ~doc:"Print build plan to stdout"
      Term.(
        const buildPlan
        $ CommonOptions.term
        $ pkgPathTerm
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"build-shell"
      ~doc:"Enter the build shell"
      Term.(
        const buildShell
        $ CommonOptions.term
        $ pkgPathTerm
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"build-package"
      ~doc:"Build specified package"
      Term.(
        const buildPackage
        $ CommonOptions.term
        $ pkgPathTerm
        $ Cli.setupLogTerm
      );

    makeCommand
      ~header:`No
      ~name:"shell"
      ~doc:"Enter esy sandbox shell"
      Term.(
        const devShell
        $ CommonOptions.term
        $ Cli.setupLogTerm
      );

    makeCommand
      ~header:`No
      ~name:"build-env"
      ~doc:"Print build environment to stdout"
      Term.(
        const buildEnv
        $ CommonOptions.term
        $ Arg.(value & flag & info ["json"]  ~doc:"Format output as JSON")
        $ pkgPathTerm
        $ Cli.setupLogTerm
      );

    makeCommand
      ~header:`No
      ~name:"command-env"
      ~doc:"Print command environment to stdout"
      Term.(
        const commandEnv
        $ CommonOptions.term
        $ Arg.(value & flag & info ["json"]  ~doc:"Format output as JSON")
        $ pkgPathTerm
        $ Cli.setupLogTerm
      );

    makeCommand
      ~header:`No
      ~name:"sandbox-env"
      ~doc:"Print sandbox environment to stdout"
      Term.(
        const sandboxEnv
        $ CommonOptions.term
        $ Arg.(value & flag & info ["json"]  ~doc:"Format output as JSON")
        $ pkgPathTerm
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"ls-builds"
      ~doc:"Output a tree of packages in the sandbox along with their status"
      Term.(
        const lsBuilds
        $ CommonOptions.term
        $ Arg.(
            value
            & flag
            & info ["T"; "include-transitive"] ~doc:"Include transitive dependencies")
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"ls-libs"
      ~doc:"Output a tree of packages along with the set of libraries made available by each package dependency."
      Term.(
        const lsLibs
        $ CommonOptions.term
        $ Arg.(
            value
            & flag
            & info ["T"; "include-transitive"] ~doc:"Include transitive dependencies")
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"ls-modules"
      ~doc:"Output a tree of packages along with the set of libraries and modules made available by each package dependency."
      Term.(
        const lsModules
        $ CommonOptions.term
        $ Arg.(
            value
            & (pos_all string [])
            & info [] ~docv:"LIB" ~doc:"Output modules only for specified lib(s)")
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"export-dependencies"
      ~doc:"Export sandbox dependendencies as prebuilt artifacts"
      Term.(
        const exportDependencies
        $ CommonOptions.term
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"import-dependencies"
      ~doc:"Import sandbox dependencies"
      Term.(
        const importDependencies
        $ CommonOptions.term
        $ Arg.(
            value
            & pos 0  (some resolvedPathTerm) None
            & info [] ~doc:"Path with builds."
          )
        $ Cli.setupLogTerm
      );

    makeCommand
      ~header:`No
      ~name:"x"
      ~doc:"Execute command as if the package is installed"
      Term.(
        const exec
        $ CommonOptions.term
        $ Cli.cmdTerm
            ~doc:"Command to execute within the sandbox environment."
            ~docv:"COMMAND"
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"export-build"
      ~doc:"Export build from the store"
      Term.(
        const exportBuild
        $ CommonOptions.term
        $ Arg.(
            required
            & pos 0  (some resolvedPathTerm) None
            & info [] ~doc:"Path with builds."
          )
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"import-build"
      ~doc:"Import build into the store"
      Term.(
        const importBuild
        $ CommonOptions.term
        $ Arg.(
            value
            & opt (some resolvedPathTerm) None
            & info ["from"; "f"] ~docv:"FROM"
          )
        $ Arg.(
            value
            & pos_all resolvedPathTerm []
            & info [] ~docv:"BUILD"
          )
        $ Cli.setupLogTerm
      );

    (* makeCommand *)
    (*   ~name:"add" *)
    (*   ~doc:"Add a new dependency" *)
    (*   Term.( *)
    (*     const add *)
    (*     $ CommonOptions.term *)
    (*     $ Arg.( *)
    (*         non_empty *)
    (*         & pos_all string [] *)
    (*         & info [] ~docv:"PACKAGE" ~doc:"Package to install" *)
    (*       ) *)
    (*     $ Cli.setupLogTerm *)
    (*   ); *)

    makeCommand
      ~name:"solve"
      ~doc:"Solve dependencies and store the solution as a lockfile"
      Term.(
        const solve
        $ CommonOptions.term
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"fetch"
      ~doc:"Fetch dependencies using the solution in a lockfile"
      Term.(
        const fetch
        $ CommonOptions.term
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"release"
      ~doc:"Produce npm package with prebuilt artifacts"
      Term.(
        const release
        $ CommonOptions.term
        $ Cli.setupLogTerm
      );

    makeCommand
      ~name:"help"
      ~doc:"Show this message and exit"
      Term.(ret (
        const (fun () -> `Help (`Auto, None))
        $ const ()
      ));

    makeCommand
      ~name:"version"
      ~doc:"Print esy version and exit"
      Term.(
        const (fun () -> print_endline EsyRuntime.version; RunAsync.return())
        $ const ()
      );

    (* aliases *)
    makeAlias buildCommand "b";
    makeAlias installCommand "i";
  ] in

  let hasCommand name =
    List.exists
      ~f:(fun (_cmd, info) -> Term.name info = name)
      commands
  in

  let runCmdliner argv =
    Term.(exit @@ eval_choice ~argv defaultCommand commands);
  in

  let commandName =
    let open Option.Syntax in
    let%bind commandName =
      try Some Sys.argv.(1)
      with Invalid_argument _ -> None
    in
    if String.get commandName 0 = '-'
    then None
    else Some commandName
  in

  match commandName with

  (*
   * Fixup invocations for commands which pass their arguments through to other
   * executables.
   *
   * TODO: currently this is implemented in a way which prevents common options
   * (like --sandbox-path or --prefix-path) from working for these commands.
   * This should be fixed.
   *)
  | Some "x"
  | Some "b"
  | Some "build" ->
    let argv =
      match Array.to_list Sys.argv with
      | (_prg::_command::"--help"::[]) as argv -> argv
      | prg::command::rest -> prg::command::"--"::rest
      | argv -> argv
    in
    let argv = Array.of_list argv in
    runCmdliner argv

  | Some "" ->
    runCmdliner Sys.argv

  (*
   * Fix
   *
   *   esy <anycommand>
   *
   * for cmdliner by injecting "--" so that users are not requied to do that.
   *)
  | Some commandName ->
    if hasCommand commandName
    then runCmdliner Sys.argv
    else
      let argv =
        match Array.to_list Sys.argv with
        | prg::rest -> prg::"--"::rest
        | argv -> argv
      in
      let argv = Array.of_list argv in
      runCmdliner argv

  | _ ->
    runCmdliner Sys.argv
