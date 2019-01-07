type t = Override.t list

let empty = []

let isEmpty = function
  | [] -> true
  | _ -> false

let add override overrides =
  override::overrides

let addMany newOverrides overrides =
  newOverrides @ overrides

let merge newOverrides overrides =
  newOverrides @ overrides

let fold' ~f ~init overrides =
  RunAsync.List.foldLeft ~f ~init (List.rev overrides)

let foldWithBuildOverrides ~f ~init overrides =
  let open RunAsync.Syntax in
  let f v override =
    let%lwt () = Logs_lwt.debug (fun m -> m "build override: %a" Override.pp override) in
    match%bind Override.build override with
    | Some override -> return (f v override)
    | None -> return v
  in
  fold' ~f ~init overrides

let foldWithInstallOverrides ~f ~init overrides =
  let open RunAsync.Syntax in
  let f v override =
    let%lwt () = Logs_lwt.debug (fun m -> m "install override: %a" Override.pp override) in
    match%bind Override.install override with
    | Some override -> return (f v override)
    | None -> return v
  in
  fold' ~f ~init overrides
