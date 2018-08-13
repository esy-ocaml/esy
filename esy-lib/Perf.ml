let measure ~label f =
  let before = Unix.gettimeofday () in
  let%lwt res = f () in
  let after = Unix.gettimeofday () in
  let%lwt () =
    let spent = 1000.0 *. (after -. before) in
    Logs_lwt.debug (fun m -> m ~header:"time" "%s: %fms" label spent)
  in
  Lwt.return res
