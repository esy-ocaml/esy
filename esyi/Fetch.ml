module Overrides = Package.Overrides
module Package = Solution.Package
module Dist = FetchStorage.Dist

(** This installs pnp enabled node wrapper. *)
let installNodeWrapper sandbox =
  let open RunAsync.Syntax in
  match Cmd.resolveCmd System.Environment.path "node" with
  | Ok nodeCmd ->
    let%bind binPath =
      let binPath = SandboxSpec.binPath sandbox.spec in
      let%bind () = Fs.createDir binPath in
      return binPath
    in
    let pnpJs = SandboxSpec.pnpJsPath sandbox.Sandbox.spec in
    let data, path =
      match System.Platform.host with
      | Windows ->
        let data =
        Format.asprintf
          {|@ECHO off
@SETLOCAL
@SET ESY__NODE_BIN_PATH=%%%a%%
"%s" -r "%a" %%*
            |} Path.pp binPath nodeCmd Path.pp pnpJs
        in
        data, Path.(binPath / "node.cmd")
      | _ ->
        let data =
          Format.asprintf
            {|#!/bin/sh
export ESY__NODE_BIN_PATH="%a"
exec "%s" -r "%a" "$@"
              |} Path.pp binPath nodeCmd Path.pp pnpJs
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
    let f pkg _deps isInstalled =
      if%bind isInstalled
      then
        match Installation.find (Solution.Package.id pkg) installation with
        | Some path -> Fs.exists path
        | None -> return false
      else
        return false
    in
    Solution.fold ~f ~init:(return true) solution

let fetch ~(sandbox : Sandbox.t) (solution : Solution.t) =
  let open RunAsync.Syntax in

  (* Collect packages which from the solution *)
  let nodeModulesPath = SandboxSpec.nodeModulesPath sandbox.spec in

  let%bind () = Fs.rmPath nodeModulesPath in
  let%bind () = Fs.createDir nodeModulesPath in

  let%bind pkgs, root =
    let root = Solution.root solution in
    let all =
      let f pkg _ pkgs = Package.Set.add pkg pkgs in
      Solution.fold ~f ~init:Package.Set.empty solution
    in
    return (Package.Set.remove root all, root)
  in

  (* Fetch all package distributions *)
  let%bind dists =
    let report, finish = Cli.createProgressReporter ~name:"fetching" () in

    let%bind dists =
      let fetch pkg =
        let%lwt () =
          let status = Format.asprintf "%a" Package.pp pkg in
          report status
        in
        FetchStorage.fetch ~sandbox pkg
      in
      RunAsync.List.mapAndJoin
        ~concurrency:40
        ~f:fetch
        (Package.Set.elements pkgs)
    in

    let%lwt () = finish () in

    let dists =
      let f dists dist = PackageId.Map.add (Dist.id dist) dist dists in
      List.fold_left ~f ~init:PackageId.Map.empty dists
    in

    return dists
  in

  (* Produce _esy/<sandbox>/installation.json *)
  let%bind installation =
    let installation =
      let f id dist installation =
        Installation.add id (Dist.sourceInstallPath dist) installation
      in
      let init =
        Installation.empty
        |> Installation.add
            (Package.id root)
            sandbox.spec.path;
      in
      PackageId.Map.fold f dists init
    in

    let%bind () =
      Fs.writeJsonFile
        ~json:(Installation.to_yojson installation)
        (SandboxSpec.installationPath sandbox.spec)
    in

    return installation
  in

  (* Produce _esy/<sandbox>/pnp.js *)
  let%bind () =
    let path = SandboxSpec.pnpJsPath sandbox.spec in
    let data = PnpJs.render ~solution ~installation ~sandbox:sandbox.spec () in
    Fs.writeFile ~data path
  in

  (* place <binPath>/node executable with pnp enabled *)
  let%bind () = installNodeWrapper sandbox in

  let%bind () =

    let seen = ref Package.Set.empty in
    let queue = LwtTaskQueue.create ~concurrency:15 () in

    let install dist =
      LwtTaskQueue.submit queue (fun () -> FetchStorage.install dist)
    in

    let rec visit pkg =
      if Package.Set.mem pkg !seen
      then return ()
      else (
        seen := Package.Set.add pkg !seen;
        let isRoot = Package.compare root pkg = 0 in
        let dependendencies =
          let traverse =
            if isRoot
            then Solution.traverseWithDevDependencies
            else Solution.traverse
          in
          Solution.dependencies ~traverse pkg solution
        in
        let%bind () =
          RunAsync.List.mapAndWait
            ~f:visit
            dependendencies
        in

        match isRoot, PackageId.Map.find_opt (Solution.Package.id pkg) dists with
        | false, Some dist -> install dist
        | false, None -> errorf "dist not found: %a" Package.pp pkg
        | true, _ -> return ()
      )
    in

    visit root
  in

  (* Produce _esy/<sandbox>/bin *)
  let%bind () =
    let path = SandboxSpec.pnpJsPath sandbox.spec in
    let data = PnpJs.render ~solution ~installation ~sandbox:sandbox.spec () in
    Fs.writeFile ~data path
  in

  return ()
