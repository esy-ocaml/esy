let toRunAsync ?(desc="I/O failed") promise =
  let open RunAsync.Syntax in
  try%lwt
    let%lwt v = promise () in
    return v
  with Unix.Unix_error (err, _, _) ->
    let msg = Unix.error_message err in
    error (Printf.sprintf "%s: %s" desc msg)

let readFile (path : Path.t) =
  let path = Path.to_string path in
  let desc = Printf.sprintf "Unable to read file %s" path in
  toRunAsync ~desc (fun () ->
    let f ic = Lwt_io.read ic in
    Lwt_io.with_file ~mode:Lwt_io.Input path f
  )

let openFile ~mode ~perm path =
  toRunAsync (fun () ->
    Lwt_unix.openfile (Path.to_string path) mode perm)

let readJsonFile (path : Path.t) =
  let open RunAsync.Syntax in
  let%bind data = readFile path in
  return (Yojson.Safe.from_string data)

let exists (path : Path.t) =
  let path = Path.to_string path in
  let%lwt exists = Lwt_unix.file_exists path in
  RunAsync.return exists

let chmod permission (path : Path.t) =
  let path = Path.to_string path in
  let%lwt () = Lwt_unix.chmod path permission in
  RunAsync.return ()

let createDirectory (path : Path.t) =
  let rec create path =
    try%lwt (
      let path = Path.to_string path in
      Lwt_unix.mkdir path 0o777
    ) with
    | Unix.Unix_error (Unix.EEXIST, _, _) ->
      Lwt.return ()
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
      let%lwt () = create (Path.parent path) in
      let%lwt () = create path in
      Lwt.return ()
  in
  let%lwt () = create path in
  RunAsync.return ()

let stat (path : Path.t) =
  let path = Path.to_string path in
  let%lwt stats = Lwt_unix.stat path in
  RunAsync.return stats

let unlink (path : Path.t) =
  let path = Path.to_string path in
  let%lwt () = Lwt_unix.unlink path in
  RunAsync.return ()

let withTemporaryFile content f =
  let path = Filename.temp_file "esy" "tmp" in

  let%lwt () =
    let writeContent oc =
      let%lwt () = Lwt_io.write oc content in
      let%lwt () = Lwt_io.flush oc in
      Lwt.return ()
    in
    Lwt_io.with_file ~mode:Lwt_io.Output path writeContent
  in

  Lwt.finalize
    (fun () -> f (Path.v path))
    (fun () -> Lwt_unix.unlink path)

let no _path = false

let fold ?(skipTraverse=no) ~f ~(init : 'a) (path : Path.t) =
  let rec visitPathItems acc path dir =
    match%lwt Lwt_unix.readdir dir with
    | exception End_of_file -> Lwt.return acc
    | "." | ".." -> visitPathItems acc path dir
    | name ->
      let%lwt acc = visitPath acc Path.(path / name) in
      visitPathItems acc path dir
  and visitPath (acc : 'a) path =
    if skipTraverse path
    then Lwt.return acc
    else (
      let spath = Path.to_string path in
      let%lwt stat = Lwt_unix.stat spath in
      match stat.Unix.st_kind with
      | Unix.S_DIR ->
        let%lwt dir = Lwt_unix.opendir spath in
        Lwt.finalize
          (fun () -> visitPathItems acc path dir)
          (fun () -> Lwt_unix.closedir dir)
      | _ -> f acc path stat
    )
  in
  let%lwt v = visitPath init path
  in RunAsync.return v
