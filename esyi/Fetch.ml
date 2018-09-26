module Record = Solution.Record
module Dist = FetchStorage.Dist

module Manifest = struct

  module Scripts = struct
    type t = {
      postinstall : (string option [@default None]);
      install : (string option [@default None]);
    }
    [@@deriving of_yojson { strict = false }]

    let empty = {postinstall = None; install = None}
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
    name : string;
    bin : (Bin.t [@default Bin.Empty]);
    scripts : (Scripts.t [@default Scripts.empty]);
    esy : (Json.t option [@default None]);
  } [@@deriving of_yojson { strict = false }]

  let ofDir path =
    let open RunAsync.Syntax in
    let filename = Path.(path / "package.json") in
    if%bind Fs.exists filename
    then
      let%bind json = Fs.readJsonFile filename in
      let%bind manifest = RunAsync.ofRun (Json.parseJsonWith of_yojson json) in
      return (Some manifest)
    else
      return None

  let packageCommands (sourcePath : Path.t) manifest =
    let makePathToCmd cmdPath = Path.(sourcePath // v cmdPath |> normalize) in
    match manifest.bin with
    | Bin.One cmd ->
      [manifest.name, makePathToCmd cmd]
    | Bin.Many cmds ->
      let f name cmd cmds = (name, makePathToCmd cmd)::cmds in
      (StringMap.fold f cmds [])
    | Bin.Empty -> []

end

(*
 * This describes the physical layout of a solution on filesystem.
 *
 * TODO: redesign to allow safe parallel operations - no it's a plain list but
 * maybe we need a nested structure so that different installation do not race
 * with each other. Though the current method of installation (tar xzf) doesn't
 * uncover any issues with that yet.
 *)
module Layout = struct

  type t = installation list

  and installation = {
    path : Path.t;
    sourcePath : Path.t;
    record : Record.t;
    isDirectDependencyOfRoot : bool;
  }

  let pp_installation fmt installation =
    Fmt.pf fmt "%a at %a"
      Record.pp installation.record
      Path.pp installation.path

  let pp =
    Fmt.(list ~sep:(unit "@\n") pp_installation)

  let ofSolution ~nodeModulesPath sandboxPath (sol : Solution.t) =
    match Solution.root sol with
    | None -> []
    | Some root ->
      let isDirectDependencyOfRoot =
        let directDependencies = Solution.dependencies root sol in
        fun record -> Record.Set.mem record directDependencies
      in

      (* Go through breadcrumb till the insertion point and mark each modules as
       * occupied for the record. This will prevent inserting other versions of
       * the same record at such places. *)
      let markAsOccupied insertion breadcrumb record =
        let _, insertionPath = insertion in
        let rec aux = function
          | (modules, path)::rest ->
            Hashtbl.replace modules record.Record.name record;
            if Path.compare insertionPath path = 0
            then ()
            else aux rest
          | [] -> ()
        in
        aux breadcrumb
      in

      let rec findInsertion record breacrumb =
        match breacrumb with
        | [] -> `None
        | ((modules, _) as here)::upTheTree ->
          begin match Hashtbl.find_opt modules record.Record.name with
          | Some r ->
            if Record.compare r record = 0
            then `Done (here, here::upTheTree)
            else `None
          | None ->
            begin match findInsertion record upTheTree with
            | `Ok nextItem -> `Ok nextItem
            | `Done there -> `Done there
            | `None -> `Ok (here, here::upTheTree)
            end
          end
      in

      (* layout record at breadcrumb, return layout and a new breadcrumb which
       * is then used to layout record's dependencies *)
      let layoutRecord ~this ~breadcrumb ~layout record =

        let insert ((_modules, path) as here) =
          markAsOccupied here (this::breadcrumb) record;
          let path = Path.(path // v record.Record.name) in
          let sourcePath =
            let main, _ = record.Record.source in
            match main with
            | Source.Archive _
            | Source.Git _
            | Source.Github _
            | Source.LocalPath _
            | Source.NoSource -> path
            | Source.LocalPathLink {path; manifest = _;} -> Path.(sandboxPath // path)
          in
          let installation = {
            path;
            sourcePath;
            record;
            isDirectDependencyOfRoot = isDirectDependencyOfRoot record;
          } in
          installation::layout
        in

        match findInsertion record breadcrumb with
        | `Done (there, _) ->
          markAsOccupied there (this::breadcrumb) record;
          None
        | `Ok (here, breadcrumb) ->
          Some (insert here, breadcrumb)
        | `None ->
          Some (insert this, this::breadcrumb)
      in

      let rec layoutDependencies ~seen ~breadcrumb ~layout record =

        let this =
          let modules = Hashtbl.create 100 in
          let path =
            match breadcrumb with
            | (_modules, path)::_ -> Path.(path // v record.Record.name / "node_modules")
            | [] -> nodeModulesPath
          in
          modules, path
        in

        let dependencies = Solution.dependencies record sol in

        (* layout direct dependencies first, they can be relocated so this is
         * why get dependenciesWithBreadcrumbs as a result *)
        let layout, dependenciesWithBreadcrumbs =
          let f r (layout, dependenciesWithBreadcrumbs) =
            match layoutRecord ~this ~breadcrumb ~layout r with
            | Some (layout, breadcrumb) -> layout, (r, breadcrumb)::dependenciesWithBreadcrumbs
            | None -> layout, dependenciesWithBreadcrumbs
          in
          Record.Set.fold f dependencies (layout, [])
        in

        (* now layout dependencies of dependencies *)
        let layout =
          let seen = Record.Set.add record seen in
          let f layout (r, breadcrumb) =
              match Record.Set.mem r seen with
              | true -> layout
              | false -> layoutDependencies ~seen ~breadcrumb ~layout r
          in
          List.fold_left ~f ~init:layout dependenciesWithBreadcrumbs
        in

        layout
      in

      let layout =
        layoutDependencies ~seen:Record.Set.empty ~breadcrumb:[] ~layout:[] root
      in

      (* Sort the layout so we can have stable order of operations *)
      let layout =
        let cmp a b = Path.compare a.path b.path in
        List.sort ~cmp layout
      in

      (layout : t)

  let%test_module "Layout" = (module struct

    let parseVersionExn v =
      match SemverVersion.Version.parse v with
      | Ok v -> v
      | Error msg -> failwith msg

    let r name version = ({
      Record.
      name;
      version = Version.Npm (parseVersionExn version);
      source = Source.NoSource, [];
      overrides = Package.Overrides.empty;
      files = [];
      opam = None;
    } : Record.t)

    let id name version =
      let version = version ^ ".0.0" in
      let version = Version.Npm (parseVersionExn version) in
      ((name, version) : Solution.Id.t)

    let expect layout expectation =
      let convert =
        let f (installation : installation) =
          Format.asprintf "%a" Record.pp installation.record,
          Path.show installation.path
        in
        List.map ~f layout
      in
      if Pervasives.compare convert expectation = 0
      then true
      else begin
        Format.printf "Got:@[<v 2>@\n%a@]@\n" pp layout;
        false
      end

    let%test "simple" =
      let sol = Solution.(
        empty
        |> add ~record:(r "a" "1") ~dependencies:[]
        |> add ~record:(r "b" "1") ~dependencies:[]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "b@1.0.0", "./node_modules/b";
      ]

    let%test "simple2" =
      let sol = Solution.(
        empty
        |> add ~record:(r "a" "1") ~dependencies:[]
        |> add ~record:(r "b" "1") ~dependencies:[id "a" "1"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "b@1.0.0", "./node_modules/b";
      ]

    let%test "simple3" =
      let sol = Solution.(
        empty
        |> add ~record:(r "c" "1") ~dependencies:[]
        |> add ~record:(r "a" "1") ~dependencies:[id "c" "1"]
        |> add ~record:(r "b" "1") ~dependencies:[id "c" "1"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "b@1.0.0", "./node_modules/b";
        "c@1.0.0", "./node_modules/c";
      ]

    let%test "conflict" =
      let sol = Solution.(
        empty
        |> add ~record:(r "a" "1") ~dependencies:[]
        |> add ~record:(r "a" "2") ~dependencies:[]
        |> add ~record:(r "b" "1") ~dependencies:[id "a" "2"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "b@1.0.0", "./node_modules/b";
        "a@2.0.0", "./node_modules/b/node_modules/a";
      ]

    let%test "conflict2" =
      let sol = Solution.(
        empty
        |> add ~record:(r "shared" "1") ~dependencies:[]
        |> add ~record:(r "a" "1") ~dependencies:[id "shared" "1"]
        |> add ~record:(r "a" "2") ~dependencies:[id "shared" "1"]
        |> add ~record:(r "b" "1") ~dependencies:[id "a" "2"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "b@1.0.0", "./node_modules/b";
        "a@2.0.0", "./node_modules/b/node_modules/a";
        "shared@1.0.0", "./node_modules/shared";
      ]

    let%test "conflict3" =
      let sol = Solution.(
        empty
        |> add ~record:(r "shared" "1") ~dependencies:[]
        |> add ~record:(r "shared" "2") ~dependencies:[]
        |> add ~record:(r "a" "1") ~dependencies:[id "shared" "1"]
        |> add ~record:(r "a" "2") ~dependencies:[id "shared" "2"]
        |> add ~record:(r "b" "1") ~dependencies:[id "a" "2"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "shared@1.0.0", "./node_modules/a/node_modules/shared";
        "b@1.0.0", "./node_modules/b";
        "a@2.0.0", "./node_modules/b/node_modules/a";
        "shared@2.0.0", "./node_modules/shared";
      ]

    let%test "conflict4" =
      let sol = Solution.(
        empty
        |> add ~record:(r "c" "1") ~dependencies:[]
        |> add ~record:(r "c" "2") ~dependencies:[]
        |> add ~record:(r "a" "1") ~dependencies:[id "c" "1"]
        |> add ~record:(r "b" "1") ~dependencies:[id "c" "2"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "c@1.0.0", "./node_modules/a/node_modules/c";
        "b@1.0.0", "./node_modules/b";
        "c@2.0.0", "./node_modules/c";
      ]

    let%test "nested" =
      let sol = Solution.(
        empty
        |> add ~record:(r "d" "1") ~dependencies:[]
        |> add ~record:(r "c" "1") ~dependencies:[id "d" "1"]
        |> add ~record:(r "a" "1") ~dependencies:[id "c" "1"]
        |> add ~record:(r "b" "1") ~dependencies:[id "c" "1"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "b@1.0.0", "./node_modules/b";
        "c@1.0.0", "./node_modules/c";
        "d@1.0.0", "./node_modules/d";
      ]

    let%test "nested conflict" =
      let sol = Solution.(
        empty
        |> add ~record:(r "d" "1") ~dependencies:[]
        |> add ~record:(r "c" "1") ~dependencies:[id "d" "1"]
        |> add ~record:(r "c" "2") ~dependencies:[id "d" "1"]
        |> add ~record:(r "a" "1") ~dependencies:[id "c" "1"]
        |> add ~record:(r "b" "1") ~dependencies:[id "c" "2"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "c@1.0.0", "./node_modules/a/node_modules/c";
        "b@1.0.0", "./node_modules/b";
        "c@2.0.0", "./node_modules/c";
        "d@1.0.0", "./node_modules/d";
      ]

    let%test "nested conflict 2" =
      let sol = Solution.(
        empty
        |> add ~record:(r "d" "1") ~dependencies:[]
        |> add ~record:(r "d" "2") ~dependencies:[]
        |> add ~record:(r "c" "1") ~dependencies:[id "d" "1"]
        |> add ~record:(r "c" "2") ~dependencies:[id "d" "2"]
        |> add ~record:(r "a" "1") ~dependencies:[id "c" "1"]
        |> add ~record:(r "b" "1") ~dependencies:[id "c" "2"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "c@1.0.0", "./node_modules/a/node_modules/c";
        "d@1.0.0", "./node_modules/a/node_modules/d";
        "b@1.0.0", "./node_modules/b";
        "c@2.0.0", "./node_modules/c";
        "d@2.0.0", "./node_modules/d";
      ]

    let%test "nested conflict 3" =
      let sol = Solution.(
        empty
        |> add ~record:(r "d" "1") ~dependencies:[]
        |> add ~record:(r "d" "2") ~dependencies:[]
        |> add ~record:(r "b" "1") ~dependencies:[id "d" "1"]
        |> add ~record:(r "a" "1") ~dependencies:[id "b" "1"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "d" "2"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "b@1.0.0", "./node_modules/b";
        "d@1.0.0", "./node_modules/b/node_modules/d";
        "d@2.0.0", "./node_modules/d";
      ]

    let%test "nested conflict 4" =
      let sol = Solution.(
        empty
        |> add ~record:(r "d" "1") ~dependencies:[]
        |> add ~record:(r "c" "1") ~dependencies:[id "d" "1"]
        |> add ~record:(r "c" "2") ~dependencies:[id "d" "1"]
        |> add ~record:(r "b" "1") ~dependencies:[id "c" "2"]
        |> add ~record:(r "a" "1") ~dependencies:[id "c" "1"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "c@1.0.0", "./node_modules/a/node_modules/c";
        "b@1.0.0", "./node_modules/b";
        "c@2.0.0", "./node_modules/c";
        "d@1.0.0", "./node_modules/d";
      ]

    let%test "nested conflict 5" =
      let sol = Solution.(
        empty
        |> add ~record:(r "d" "1") ~dependencies:[]
        |> add ~record:(r "d" "2") ~dependencies:[]
        |> add ~record:(r "c" "1") ~dependencies:[id "d" "1"]
        |> add ~record:(r "c" "2") ~dependencies:[id "d" "2"]
        |> add ~record:(r "b" "1") ~dependencies:[id "c" "2"]
        |> add ~record:(r "a" "1") ~dependencies:[id "c" "1"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "c@1.0.0", "./node_modules/a/node_modules/c";
        "d@1.0.0", "./node_modules/a/node_modules/d";
        "b@1.0.0", "./node_modules/b";
        "c@2.0.0", "./node_modules/c";
        "d@2.0.0", "./node_modules/d";
      ]

    let%test "nested conflict 6" =
      let sol = Solution.(
        empty
        |> add ~record:(r "punycode" "1") ~dependencies:[]
        |> add ~record:(r "punycode" "2") ~dependencies:[]
        |> add ~record:(r "url" "1") ~dependencies:[id "punycode" "2"]
        |> add ~record:(r "browserify" "1") ~dependencies:[id "punycode" "1"; id "url" "1"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "browserify" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "browserify@1.0.0", "./node_modules/browserify";
        "punycode@1.0.0", "./node_modules/punycode";
        "url@1.0.0", "./node_modules/url";
        "punycode@2.0.0", "./node_modules/url/node_modules/punycode";
      ]

    let%test "loop 1" =
      let sol = Solution.(
        empty
        |> add ~record:(r "b" "1") ~dependencies:[id "a" "1"]
        |> add ~record:(r "a" "1") ~dependencies:[id "b" "1"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "b@1.0.0", "./node_modules/b";
      ]

    let%test "loop 2" =
      let sol = Solution.(
        empty
        |> add ~record:(r "c" "1") ~dependencies:[]
        |> add ~record:(r "b" "1") ~dependencies:[id "a" "1"; id "c" "1"]
        |> add ~record:(r "a" "1") ~dependencies:[id "b" "1"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "b@1.0.0", "./node_modules/b";
        "c@1.0.0", "./node_modules/c";
      ]

    let%test "loop 3" =
      let sol = Solution.(
        empty
        |> add ~record:(r "c" "1") ~dependencies:[id "a" "1"]
        |> add ~record:(r "b" "1") ~dependencies:[id "a" "1"]
        |> add ~record:(r "a" "1") ~dependencies:[id "b" "1"; id "c" "1"]
        |> addRoot ~record:(r "root" "1") ~dependencies:[id "a" "1"; id "b" "1"]
      ) in
      let layout = ofSolution ~nodeModulesPath:(Path.v "./node_modules") (Path.v ".") sol in
      expect layout [
        "a@1.0.0", "./node_modules/a";
        "b@1.0.0", "./node_modules/b";
        "c@1.0.0", "./node_modules/c";
      ]

  end)
end

let runLifecycleScript ~installation ~name script =
  let%lwt () = Logs_lwt.app
    (fun m ->
      m "%a: running %a lifecycle"
      Record.pp installation.Layout.record
      Fmt.(styled `Bold string) name
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
    let script =
      Printf.sprintf
        "cd %s && %s"
        (Filename.quote (Path.show installation.path))
        script
    in
    (* TODO(windows): use cmd here *)
    let cmd = "/bin/bash", [|"/bin/bash"; "-c"; script|] in
    Lwt_process.with_process_full cmd f
  with
  | Unix.Unix_error (err, _, _) ->
    let msg = Unix.error_message err in
    RunAsync.error msg
  | _ ->
    RunAsync.error "error running subprocess"

let runLifecycle ~installation ~(manifest : Manifest.t) () =
  let open RunAsync.Syntax in
  let%bind () =
    match manifest.scripts.install with
    | Some cmd -> runLifecycleScript ~installation ~name:"install" cmd
    | None -> return ()
  in
  let%bind () =
    match manifest.scripts.postinstall with
    | Some cmd -> runLifecycleScript ~installation ~name:"postinstall" cmd
    | None -> return ()
  in
  return ()

let isInstalled ~(sandbox : Sandbox.t) (solution : Solution.t) =
  let open RunAsync.Syntax in
  let nodeModulesPath = SandboxSpec.nodeModulesPath sandbox.spec in
  let layout = Layout.ofSolution ~nodeModulesPath sandbox.spec.path solution in
  let f installed {Layout.path;_} =
    if not installed
    then return installed
    else Fs.exists path
  in
  RunAsync.List.foldLeft ~f ~init:true layout

let fetch ~(sandbox : Sandbox.t) (solution : Solution.t) =
  let open RunAsync.Syntax in

  (* Collect packages which from the solution *)
  let nodeModulesPath = SandboxSpec.nodeModulesPath sandbox.spec in

  let%bind () = Fs.rmPath nodeModulesPath in
  let%bind () = Fs.createDir nodeModulesPath in

  let records =
    match Solution.root solution with
    | Some root ->
      let all = Solution.records solution in
      Record.Set.remove root all
    | None ->
      Solution.records solution
  in

  (* Fetch all records *)

  let%bind dists =
    let queue = LwtTaskQueue.create ~concurrency:8 () in
    let report, finish = sandbox.cfg.Config.createProgressReporter ~name:"fetching" () in
    let%bind items =
      let fetch record =
        let%bind dist =
          LwtTaskQueue.submit queue
          (fun () ->
            let%lwt () =
              let status = Format.asprintf "%a" Record.pp record in
              report status
            in
            FetchStorage.fetch ~cfg:sandbox.cfg record)
        in
        return (record, dist)
      in
      records
      |> Record.Set.elements
      |> List.map ~f:fetch
      |> RunAsync.List.joinAll
    in
    let%lwt () = finish () in
    let map =
      let f map (record, dist) = Record.Map.add record dist map in
      List.fold_left ~f ~init:Record.Map.empty items
    in
    return map
  in

  (* Layout all dists into node_modules *)

  let%bind installed =
    let queue = LwtTaskQueue.create ~concurrency:4 () in
    let report, finish = sandbox.cfg.Config.createProgressReporter ~name:"installing" () in
    let f ({Layout.path; sourcePath;record;_} as installation) () =
      match Record.Map.find_opt record dists with
      | Some dist ->
        let%lwt () =
          let status = Format.asprintf "%a" Dist.pp dist in
          report status
        in
        let%bind () =
          FetchStorage.install ~cfg:sandbox.cfg ~path dist
        in
        let%bind manifest = Manifest.ofDir sourcePath in
        return (installation, manifest)
      | None ->
        let msg =
          Printf.sprintf
            "inconsistent state: no dist were fetched for %s@%s at %s"
            record.Record.name
            (Version.show record.Record.version)
            (Path.show path)
        in
        failwith msg
    in

    let layout =
      Layout.ofSolution
        ~nodeModulesPath
        sandbox.spec.path
        solution
    in

    let%bind installed =
      let install installation =
        RunAsync.contextf
          (LwtTaskQueue.submit queue (f installation))
          "installing %a"
          Layout.pp_installation installation
      in
      layout
      |> List.map ~f:install
      |> RunAsync.List.joinAll
    in

    let%lwt () = finish () in

    return installed
  in

  (* run lifecycle scripts *)

  let%bind () =
    let queue = LwtTaskQueue.create ~concurrency:1 () in

    let f = function
      | (installation, Some ({Manifest. esy = None; _} as manifest)) ->
        RunAsync.contextf
          (LwtTaskQueue.submit
            queue
            (runLifecycle ~installation ~manifest))
          "running lifecycle %a"
          Layout.pp_installation installation
      | (_installation, Some {Manifest. esy = Some _; _})
      | (_installation, None) -> return ()
    in

    let%bind () =
      installed
      |> List.map ~f
      |> RunAsync.List.waitAll
    in

    return ()
  in

  (* populate node_modules/.bin with scripts defined for the direct dependencies *)

  let%bind () =
    let nodeModulesPath = SandboxSpec.nodeModulesPath sandbox.spec in
    let binPath = Path.(nodeModulesPath / ".bin") in
    let%bind () = Fs.createDir binPath in

    let installBinWrapper (name, path) =
      if%bind Fs.exists path
      then (
        let%bind () = Fs.chmod 0o777 path in
        Fs.symlink ~src:path Path.(binPath / name)
      ) else (
        Logs_lwt.warn (fun m -> m "missing %a defined as binary" Path.pp path);%lwt
        return ()
      )
    in

    let installBinWrappersForPkg = function
      | (installation, Some manifest) ->
        Manifest.packageCommands installation.Layout.sourcePath manifest
        |> List.map ~f:installBinWrapper
        |> RunAsync.List.waitAll
      | (_installation, None) -> return ()
    in

    installed
    |> List.filter ~f:(fun (installation, _) -> installation.Layout.isDirectDependencyOfRoot)
    |> List.map ~f:installBinWrappersForPkg
    |> RunAsync.List.waitAll
  in

  (* link default sandbox node_modules to <projectPath>/node_modules *)

  let%bind () =
    if SandboxSpec.isDefault sandbox.spec
    then
      let nodeModulesPath = SandboxSpec.nodeModulesPath sandbox.spec in
      let targetPath = Path.(sandbox.spec.path / "node_modules") in
      let%bind () = Fs.rmPath targetPath in
      let%bind () = Fs.symlink ~src:nodeModulesPath targetPath in
      return ()
    else return ()
  in

  (* place dune with ignored_subdirs stanza inside node_modiles *)

  let%bind () =
    let packagesPath = SandboxSpec.nodeModulesPath sandbox.spec in
    let%bind items = Fs.listDir packagesPath in
    let items = String.concat " " items in
    let data = "(ignored_subdirs (" ^ items ^ "))\n" in
    Fs.writeFile ~data Path.(packagesPath / "dune")
  in

  return ()
