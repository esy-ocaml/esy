module Overrides = Package.Overrides
module String = Astring.String

module NpmPackageJson : sig
  type t

  type lifecycle = {
    postinstall : string option;
    install : string option;
  }

  val ofDir : Path.t -> t option RunAsync.t

  val bin : sourcePath:Path.t -> t -> (string * Path.t) list
  val lifecycle : t -> lifecycle option

end = struct

  module Lifecycle = struct
    type t = {
      postinstall : (string option [@default None]);
      install : (string option [@default None]);
    }
    [@@deriving of_yojson { strict = false }]
  end

  module Bin = struct
    type t =
      | Empty
      | One of string
      | Many of string StringMap.t

    let of_yojson =
      let open Result.Syntax in
      function
      | `String cmd ->
        let cmd = String.trim cmd in
        begin match cmd with
        | "" -> return Empty
        | cmd -> return (One cmd)
        end
      | `Assoc items ->
        let%bind items =
          let f cmds (name, json) =
            match json with
            | `String cmd -> return (StringMap.add name cmd cmds)
            | _ -> error "expected a string"
          in
          Result.List.foldLeft ~f ~init:StringMap.empty items
        in
        return (Many items)
      | _ -> error "expected a string or an object"
  end

  type t = {
    name : string option [@default None];
    bin : (Bin.t [@default Bin.Empty]);
    scripts : (Lifecycle.t option [@default None]);
    esy : (Json.t option [@default None]);
  } [@@deriving of_yojson { strict = false }]

  type lifecycle = Lifecycle.t = {
    postinstall : string option;
    install : string option;
  }

  let ofDir path =
    let open RunAsync.Syntax in
    if%bind Fs.exists Path.(path / "esy.json")
    then return None
    else
      let filename = Path.(path / "package.json") in
      if%bind Fs.exists filename
      then
        let%bind json = Fs.readJsonFile filename in
        let%bind manifest = RunAsync.ofRun (Json.parseJsonWith of_yojson json) in
        if Option.isSome manifest.esy
        then return None
        else return (Some manifest)
      else
        return None

  let bin ~sourcePath pkgJson =
    let makePathToCmd cmdPath = Path.(sourcePath // v cmdPath |> normalize) in
    match pkgJson.bin, pkgJson.name with
    | Bin.One cmd, Some name ->
      [name, makePathToCmd cmd]
    | Bin.One cmd, None ->
      let cmd = makePathToCmd cmd in
      let name = Path.basename cmd in
      [name, cmd]
    | Bin.Many cmds, _ ->
      let f name cmd cmds = (name, makePathToCmd cmd)::cmds in
      (StringMap.fold f cmds [])
    | Bin.Empty, _ -> []

  let lifecycle pkgJson =
    match pkgJson.scripts with
    | Some {Lifecycle. postinstall = None; install = None;} -> None
    | lifecycle -> lifecycle

end

module PackagePaths = struct

  let key pkg =
    let suffix =
      match pkg.Solution.Package.version with
      | Version.Npm v -> SemverVersion.Version.show v
      | Version.Opam v -> "opam-" ^ OpamPackageVersion.Version.show v
      | Version.Source source -> Digest.(to_hex (string (Source.show source)))
    in
    Path.safeSeg pkg.Solution.Package.name ^ "--" ^ Path.safeSeg suffix

  let stagePath sandbox pkg =
    (* We are getting EACCESS error on Windows if we try to rename directory
     * from stage to install after we read a file from there. It seems we are
     * leaking fds and Windows prevent rename from working.
     *
     * For now we are unpacking and running lifecycle directly in a final
     * directory and in case of an error we do a cleanup by removing the
     * install directory (so that subsequent installation attempts try to do
     * install again).
     *)
    match System.Platform.host with
    | Windows -> Path.(sandbox.Sandbox.cfg.sourceInstallPath / key pkg)
    | _ -> Path.(sandbox.Sandbox.cfg.sourceStagePath / key pkg)

  let installPath sandbox pkg =
    match pkg.Solution.Package.source with
    | Package.Link { path; manifest = _; } ->
      DistPath.toPath sandbox.Sandbox.spec.path path
    | Package.Install _ ->
      Path.(sandbox.Sandbox.cfg.sourceInstallPath / key pkg)

  let commit ~needRewrite stagePath installPath =
    let open RunAsync.Syntax in
    (* See distStagePath for details *)
    match System.Platform.host with
    | Windows -> RunAsync.return ()
    | _ ->
      let%bind () =
        if needRewrite
        then
          RewritePrefix.rewritePrefix
            ~origPrefix:stagePath
            ~destPrefix:installPath
            stagePath
        else
          return ()
      in
      Fs.rename ~src:stagePath installPath

end

module FetchPackage : sig

  type fetch

  type installation = {
    pkgJson : NpmPackageJson.t option;
    path : Path.t;
  }

  val fetch : Sandbox.t -> Solution.Package.t -> fetch RunAsync.t
  val install : (Path.t -> unit RunAsync.t) -> Sandbox.t -> fetch -> installation RunAsync.t

end = struct

  type fetch = Solution.Package.t * kind

  and kind =
    | Fetched of DistStorage.fetchedDist
    | Installed of Path.t
    | Linked of Path.t

  type installation = {
    pkgJson : NpmPackageJson.t option;
    path : Path.t;
  }


  (* fetch any of the dists for the package *)
  let fetch' sandbox pkg dists =
    let open RunAsync.Syntax in

    let rec fetchAny errs alternatives =
      match alternatives with
      | dist::rest ->
        let fetched =
          DistStorage.fetch
            ~cfg:sandbox.Sandbox.cfg
            ~sandbox:sandbox.spec
            dist
        in
        begin match%lwt fetched with
        | Ok fetched -> return fetched
        | Error err -> fetchAny ((dist, err)::errs) rest
        end
      | [] ->
        Logs_lwt.err (fun m ->
          let ppErr fmt (source, err) =
            Fmt.pf fmt
              "source: %a@\nerror: %a"
              Dist.pp source
              Run.ppError err
          in
          m "unable to fetch %a:@[<v 2>@\n%a@]"
            Solution.Package.pp pkg
            Fmt.(list ~sep:(unit "@\n") ppErr) errs
        );%lwt
        error "installation error"
    in

    fetchAny [] dists

  let fetch sandbox pkg =
    (** TODO: need to sync here so no two same tasks are running at the same time *)
    let open RunAsync.Syntax in

    RunAsync.contextf (
      match pkg.Solution.Package.source with
      | Package.Link {path; _} ->
        let path = DistPath.toPath sandbox.Sandbox.spec.path path in
        return (pkg, Linked path)
      | Package.Install { source = main, mirrors; opam = _; } ->
        let path = PackagePaths.installPath sandbox pkg in
        if%bind Fs.exists path
        then
          return (pkg, Installed path)
        else
          let dists = main::mirrors in
          let%bind dist = fetch' sandbox pkg dists in
          return (pkg, Fetched dist)
    ) "fetching %a" Solution.Package.pp pkg


  module Lifecycle = struct

    let runScript ?env ~lifecycleName pkg sourcePath script =
      let%lwt () = Logs_lwt.app
        (fun m ->
          m "%a: running %a lifecycle"
          Solution.Package.pp pkg
          Fmt.(styled `Bold string) lifecycleName
        )
      in

      let readAndCloseChan ic =
        Lwt.finalize
          (fun () -> Lwt_io.read ic)
          (fun () -> Lwt_io.close ic)
      in

      let f p =
        let%lwt stdout = readAndCloseChan p#stdout
        and stderr = readAndCloseChan p#stderr in
        match%lwt p#status with
        | Unix.WEXITED 0 ->
          RunAsync.return ()
        | _ ->
          Logs_lwt.err (fun m -> m
            "@[<v>command failed: %s@\nstderr:@[<v 2>@\n%s@]@\nstdout:@[<v 2>@\n%s@]@]"
            script stderr stdout
          );%lwt
          RunAsync.error "error running command"
      in

      try%lwt
        (* We don't need to wrap the install path on Windows in quotes *)
        let installationPath =
          match System.Platform.host with
          | Windows -> Path.show sourcePath
          | _ -> Filename.quote (Path.show sourcePath)
        in
        (* On Windows, cd by itself won't switch between drives *)
        (* We'll add the /d flag to allow switching drives - *)
        let changeDirCommand = match System.Platform.host with
          | Windows -> "/d"
          | _ -> ""
        in
        let script =
          Printf.sprintf
            "cd %s %s && %s"
            changeDirCommand
            installationPath
            script
        in
        let cmd =
          match System.Platform.host with
          | Windows -> ("", [|"cmd.exe";("/c " ^ script)|])
          | _ -> ("/bin/bash", [|"/bin/bash";"-c";script|])
        in
        let env =
          let open Option.Syntax in
          let%bind env = env in
          let%bind _, env = ChildProcess.prepareEnv env in
          return env
        in
        Lwt_process.with_process_full ?env cmd f
      with
      | Unix.Unix_error (err, _, _) ->
        let msg = Unix.error_message err in
        RunAsync.error msg
      | _ ->
        RunAsync.error "error running subprocess"

    let run pkg sourcePath lifecycle =
      let open RunAsync.Syntax in
      let%bind env =
        let path = Path.(show (sourcePath / "_esy"))::System.Environment.path in
        let sep = System.Environment.sep ~name:"PATH" () in
        let override = Astring.String.Map.(add "PATH" (String.concat ~sep path) empty) in
        return (ChildProcess.CurrentEnvOverride override)
      in

      let%bind () =
        match lifecycle.NpmPackageJson.install with
        | Some cmd -> runScript ~env ~lifecycleName:"install" pkg sourcePath cmd
        | None -> return ()
      in

      let%bind () =
        match lifecycle.NpmPackageJson.postinstall with
        | Some cmd -> runScript ~env ~lifecycleName:"postinstall" pkg sourcePath cmd
        | None -> return ()
      in

      return ()
  end

  let copyFiles sandbox pkg path =
    let open RunAsync.Syntax in

    let%bind filesOfOpam = Solution.Package.readOpamFiles pkg in
    let%bind filesOfOverride =
      Package.Overrides.files
        ~cfg:sandbox.Sandbox.cfg
        ~sandbox:sandbox.Sandbox.spec
        pkg.Solution.Package.overrides
    in

    RunAsync.List.mapAndWait
      ~f:(File.placeAt path)
      (filesOfOpam @ filesOfOverride)

  let install' onBeforeLifecycle sandbox pkg fetched =
    let open RunAsync.Syntax in

    let installPath = PackagePaths.installPath sandbox pkg in

    let%bind stagePath =
      let path = PackagePaths.stagePath sandbox pkg in
      let%bind () = Fs.rmPath path in
      return path
    in

    let%bind () =
      DistStorage.unpack fetched stagePath
    in

    let%bind () = copyFiles sandbox pkg stagePath in
    let%bind pkgJson = NpmPackageJson.ofDir stagePath in

    let%bind () =
      match Option.bind ~f:NpmPackageJson.lifecycle pkgJson with
      | Some lifecycle ->
        let%bind () = onBeforeLifecycle stagePath in
        let%bind () = Lifecycle.run pkg stagePath lifecycle in
        let%bind () = PackagePaths.commit ~needRewrite:true stagePath installPath in
        return ()
      | None ->
        let%bind () = PackagePaths.commit ~needRewrite:false stagePath installPath in
        return ()
    in

    return {path = installPath; pkgJson}

  let install onBeforeLifecycle sandbox (pkg, fetch) =
    let open RunAsync.Syntax in

    RunAsync.contextf (
      match fetch with
      | Linked path
      | Installed path ->
        let%bind pkgJson = NpmPackageJson.ofDir path in
        return {path; pkgJson;}
      | Fetched fetched ->
        install' onBeforeLifecycle sandbox pkg fetched
    ) "installing %a" Solution.Package.pp pkg
end

module LinkBin = struct

  let installNodeBinWrapper binPath (name, origPath) =
    let data, path =
      match System.Platform.host with
      | Windows ->
        let data =
        Format.asprintf
          {|@ECHO off
  @SETLOCAL
  node "%a" %%*
            |} Path.pp origPath
        in
        data, Path.(binPath / name |> addExt ".cmd")
      | _ ->
        let data =
          Format.asprintf
            {|#!/bin/sh
  exec node "%a" "$@"
              |} Path.pp origPath
        in
        data, Path.(binPath / name)
    in
    Fs.writeFile ~perm:0o755 ~data path

  let installBinWrapper binPath (name, origPath) =
    let open RunAsync.Syntax in
    Logs_lwt.debug (fun m ->
      m "Fetch:installBinWrapper: %a / %s -> %a"
      Path.pp origPath name Path.pp binPath
    );%lwt
    if%bind Fs.exists origPath
    then (
      if Path.hasExt ".js" origPath
      then installNodeBinWrapper binPath (name, origPath)
      else (
        let%bind () = Fs.chmod 0o777 origPath in
        let destPath = Path.(binPath / name) in
        if%bind Fs.exists destPath
        then return ()
        else Fs.symlink ~src:origPath destPath
      )
    ) else (
      Logs_lwt.warn (fun m -> m "missing %a defined as binary" Path.pp origPath);%lwt
      return ()
    )

  let link binPath installation =
    match installation.FetchPackage.pkgJson with
    | Some pkgJson ->
      let bin = NpmPackageJson.bin ~sourcePath:installation.path pkgJson in
      RunAsync.List.mapAndWait ~f:(installBinWrapper binPath) bin
    | None -> RunAsync.return ()
end

let collectPackagesOfSolution solution =
  let pkgs, root =
    let root = Solution.root solution in

    let rec collect (seen, topo) pkg =
      if Solution.Package.Set.mem pkg seen
      then seen, topo
      else
        let seen = Solution.Package.Set.add pkg seen in
        let seen, topo = collectDependencies (seen, topo) pkg in
        let topo = pkg::topo in
        seen, topo

    and collectDependencies (seen, topo) pkg =
      let isRoot = Solution.Package.compare root pkg = 0 in
      let dependencies =
        let traverse =
          if isRoot
          then Solution.traverseWithDevDependencies
          else Solution.traverse
        in
        Solution.dependencies ~traverse pkg solution
      in
      List.fold_left ~f:collect ~init:(seen, topo) dependencies
    in

    let _, topo = collectDependencies (Solution.Package.Set.empty, []) root in
    (List.rev topo), root
  in

  pkgs, root

(** This installs pnp enabled node wrapper. *)
let installNodeWrapper ~binPath ~pnpJsPath () =
  let open RunAsync.Syntax in
  match Cmd.resolveCmd System.Environment.path "node" with
  | Ok nodeCmd ->
    let%bind binPath =
      let%bind () = Fs.createDir binPath in
      return binPath
    in
    let data, path =
      match System.Platform.host with
      | Windows ->
        let data =
        Format.asprintf
          {|@ECHO off
@SETLOCAL
@SET ESY__NODE_BIN_PATH=%%%a%%
"%s" -r "%a" %%*
            |} Path.pp binPath nodeCmd Path.pp pnpJsPath
        in
        data, Path.(binPath / "node.cmd")
      | _ ->
        let data =
          Format.asprintf
            {|#!/bin/sh
export ESY__NODE_BIN_PATH="%a"
exec "%s" -r "%a" "$@"
              |} Path.pp binPath nodeCmd Path.pp pnpJsPath
        in
        data, Path.(binPath / "node")
    in
    Fs.writeFile ~perm:0o755 ~data path
  | Error _ ->
    (* no node available in $PATH, just skip this then *)
    return ()

let isInstalled ~(sandbox : Sandbox.t) (solution : Solution.t) =
  let open RunAsync.Syntax in
  let installationPath = SandboxSpec.installationPath sandbox.spec in
  match%lwt Installation.ofPath installationPath with
  | Error _
  | Ok None -> return false
  | Ok Some installation ->
    let rec check = function
      | [] -> return true
      | pkg::pkgs ->
        begin match Installation.find (Solution.Package.id pkg) installation with
        | None -> return false
        | Some path ->
          if%bind Fs.exists path
          then check pkgs
          else return false
        end
    in
    let pkgs, _root = collectPackagesOfSolution solution in
    check pkgs

let fetch sandbox solution =
  let open RunAsync.Syntax in

  (* Collect packages which from the solution *)
  let pkgs, root = collectPackagesOfSolution solution in

  (* Fetch all packages. *)
  let%bind fetched =
    let report, finish = Cli.createProgressReporter ~name:"fetching" () in
    let%bind items =
      let f pkg =
        let%lwt () =
          let msg = Format.asprintf "%a" PackageId.pp (Solution.Package.id pkg) in
          report msg
        in
        let%bind fetch = FetchPackage.fetch sandbox pkg in
        return (pkg, fetch)
      in
      let%bind items = RunAsync.List.mapAndJoin ~concurrency:40 ~f pkgs in
      finish ();%lwt
      return items
    in
    let fetched =
      let f map (pkg, fetch) =
        let id = Solution.Package.id pkg in
        PackageId.Map.add id fetch map
      in
      List.fold_left ~f ~init:PackageId.Map.empty items
    in
    return fetched
  in

  (* Produce _esy/<sandbox>/installation.json *)
  let%bind installation =
    let installation =
      let f installation pkg =
        let id = Solution.Package.id pkg in
        let path = PackagePaths.installPath sandbox pkg in
        Installation.add id path installation
      in
      let init =
        Installation.empty
        |> Installation.add
            (Solution.Package.id root)
            sandbox.spec.path;
      in
      List.fold_left ~f ~init pkgs
    in

    let%bind () =
      Fs.writeJsonFile
        ~json:(Installation.to_yojson installation)
        (SandboxSpec.installationPath sandbox.spec)
    in

    return installation
  in

  (* Install all packages. *)
  let%bind () =

    let report, finish = Cli.createProgressReporter ~name:"installing" () in
    let queue = LwtTaskQueue.create ~concurrency:40 () in

    let tasks = Memoize.make () in

    let install pkg dependencies =
      let open RunAsync.Syntax in
      let f () =

        let id = Solution.Package.id pkg in

        let onBeforeLifecycle path =
          (*
            * This creates <install>/_esy and populates it with a custom
            * per-package pnp.js (which allows to resolve dependencies out of
            * stage directory and a node wrapper which uses this pnp.js.
            *)
          let binPath = Path.(path / "_esy") in
          let%bind () = Fs.createDir binPath in

          let%bind () =
            RunAsync.List.mapAndWait
              ~f:(LinkBin.link binPath)
              dependencies
          in

          let%bind () =
            let pnpJsPath = Path.(binPath / "pnp.js") in
            let installation = Installation.add id path installation in
            let data = PnpJs.render
              ~basePath:binPath
              ~rootPath:path
              ~rootId:id
              ~solution
              ~installation
              ()
            in
            let%bind () = Fs.writeFile ~data pnpJsPath in
            installNodeWrapper
              ~binPath
              ~pnpJsPath
              ()
          in

          return ()
        in

        let fetched = PackageId.Map.find id fetched in
        FetchPackage.install onBeforeLifecycle sandbox fetched
      in
      LwtTaskQueue.submit queue f
    in

    let rec visit' seen pkg =
      let%bind dependencies =
        RunAsync.List.mapAndJoin
          ~f:(visit seen)
          (Solution.dependencies pkg solution)
      in
      let%lwt () =
        let id = Solution.Package.id pkg in
        let msg = Format.asprintf "%a" PackageId.pp id in
        report msg
      in
      install pkg (List.filterNone dependencies)

    and visit seen pkg =
      let id = Solution.Package.id pkg in
      if not (PackageId.Set.mem id seen)
      then
        let seen = PackageId.Set.add id seen in
        let%bind installation = Memoize.compute tasks id (fun () -> visit' seen pkg) in
        return (Some installation)
      else return None
    in

    let%bind rootDependencies =
      RunAsync.List.mapAndJoin
        ~f:(visit PackageId.Set.empty)
        (Solution.dependencies ~traverse:Solution.traverseWithDevDependencies root solution)
    in

    let%bind () =
      let binPath = SandboxSpec.binPath sandbox.spec in
      let%bind () = Fs.createDir binPath in
      RunAsync.List.mapAndWait
        ~f:(LinkBin.link binPath)
        (List.filterNone rootDependencies)
    in

    let%lwt () = finish () in
    return ()
  in

  (* Produce _esy/<sandbox>/pnp.js *)
  let%bind () =
    let path = SandboxSpec.pnpJsPath sandbox.Sandbox.spec in
    let data = PnpJs.render
      ~basePath:(Path.parent (SandboxSpec.pnpJsPath sandbox.spec))
      ~rootPath:sandbox.spec.path
      ~rootId:(Solution.Package.id (Solution.root solution))
      ~solution
      ~installation
      ()
    in
    Fs.writeFile ~data path
  in

  (* place <binPath>/node executable with pnp enabled *)
  let%bind () =
    installNodeWrapper
      ~binPath:(SandboxSpec.binPath sandbox.Sandbox.spec)
      ~pnpJsPath:(SandboxSpec.pnpJsPath sandbox.spec)
      ()
  in

  let%bind () = Fs.rmPath (SandboxSpec.distPath sandbox.Sandbox.spec) in

  return ()
