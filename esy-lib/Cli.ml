let isCi =
  match Sys.getenv_opt "CI" with
  | Some _ -> true
  | None -> false

module ProgressReporter : sig
  val status : unit -> string option
  val setStatus : string -> unit Lwt.t
  val clearStatus : unit -> unit Lwt.t
end = struct

  type t = {
    mutable status : string option;
    statusLock : Lwt_mutex.t;
    enabled : bool;
  }

  let reporter =
    let isatty = Unix.isatty Unix.stderr in
    let enabled = (not isCi) && isatty in
    {
      status = None;
      statusLock = Lwt_mutex.create ();
      enabled;
    }

  let hide s =
    match s with
    | None -> Lwt.return ()
    | Some s ->
      let len = String.length s in
      if len > 0
      then
        let s = Printf.sprintf "\r%*s\r" len "" in
        Lwt_io.write Lwt_io.stderr s
      else
        Lwt.return ()

  let show s =
    match s with
    | Some s -> Lwt_io.write Lwt_io.stderr s
    | None -> Lwt.return ()

  let status () =
    reporter.status

  let setStatus status =
    if reporter.enabled
    then
      Lwt_mutex.with_lock reporter.statusLock begin fun () ->
        hide reporter.status;%lwt
        reporter.status <- Some status;
        show reporter.status;%lwt
        Lwt_io.flush Lwt_io.stderr;%lwt
        Lwt.return ()
      end
    else Lwt.return ()

  let clearStatus () =
    if reporter.enabled
    then
      Lwt_mutex.with_lock reporter.statusLock begin fun () ->
        hide reporter.status;%lwt
        Lwt_io.flush Lwt_io.stderr;%lwt
        reporter.status <- None;
        Lwt.return ()
      end
    else Lwt.return ()
end

let pathConv =
  let open Cmdliner in
  let parse = Path.ofString in
  let print = Path.pp in
  Arg.conv ~docv:"PATH" (parse, print)

let checkoutConv =
  let open Cmdliner in
  let parse v =
    match Astring.String.cut ~sep:":" v with
    | Some (remote, "") -> Ok (`Remote remote)
    | Some ("", local) -> Ok (`Local (Path.v local))
    | Some (remote, local) -> Ok (`RemoteLocal (remote, (Path.v local)))
    | None -> Ok (`Remote v)
  in
  let print (fmt : Format.formatter) v =
    match v with
    | `RemoteLocal (remote, local) -> Fmt.pf fmt "%s:%s" remote (Path.show local)
    | `Local local -> Fmt.pf fmt ":%s" (Path.show local)
    | `Remote remote -> Fmt.pf fmt "%s" remote
  in
  Arg.conv ~docv:"VAL" (parse, print)


let cmdTerm ~doc ~docv =
  let open Cmdliner in
  let commandTerm =
    Arg.(non_empty & (pos_all string []) & (info [] ~doc ~docv))
  in
  let d command =
    match command with
    | [] ->
      `Error (false, "command cannot be empty")
    | tool::args ->
      let cmd = Cmd.(v tool |> addArgs args) in
      `Ok cmd
  in
  Term.(ret (const d $ commandTerm))

let cmdOptionTerm ~doc ~docv =
  let open Cmdliner in
  let commandTerm =
    Arg.(value & (pos_all string []) & (info [] ~doc ~docv))
  in
  let d command =
    match command with
    | [] ->
      `Ok None
    | tool::args ->
      let cmd = Cmd.(v tool |> addArgs args) in
      `Ok (Some cmd)
  in
  Term.(ret (const d $ commandTerm))

let setupLogTerm =
  let pp_header ppf ((lvl : Logs.level), _header) =
    match lvl with
    | Logs.App ->
      Fmt.(styled `Blue (unit "info ")) ppf ()
    | Logs.Error ->
      Fmt.(styled `Red (unit "error ")) ppf ()
    | Logs.Warning ->
      Fmt.(styled `Yellow (unit "warn ")) ppf ()
    | Logs.Info ->
      Fmt.(styled `Blue (unit "info ")) ppf ()
    | Logs.Debug ->
      Fmt.(unit "debug ") ppf ()
  in
  let lwt_reporter () =
    let buf_fmt ~like =
      let b = Buffer.create 512 in
      Fmt.with_buffer ~like b,
      fun () -> let m = Buffer.contents b in Buffer.reset b; m
    in
    let mutex = Lwt_mutex.create () in
    let app, app_flush = buf_fmt ~like:Fmt.stdout in
    let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
    let reporter = Logs_fmt.reporter ~pp_header ~app ~dst () in
    let report src level ~over k msgf =
      let k () =
        let write () =
          let%lwt () =
            match level with
            | Logs.App ->
              let msg = app_flush () in
              Lwt_io.write Lwt_io.stdout msg;%lwt
              Lwt_io.flush Lwt_io.stdout;%lwt
              Lwt.return ()
            | _ ->
              let msg = dst_flush () in
              Lwt_io.write Lwt_io.stderr msg;%lwt
              Lwt_io.flush Lwt_io.stderr;%lwt
              Lwt.return ()
          in
          Lwt.return ()
        in
        let writeAndPreserveProgress () =
          Lwt_mutex.with_lock mutex begin fun () ->
            match ProgressReporter.status () with
            | None ->
              write ();%lwt
              Lwt.return ()
            | Some status ->
              ProgressReporter.clearStatus ();%lwt
              write ();%lwt
              ProgressReporter.setStatus status;%lwt
              Lwt.return ()
          end
        in
        let unblock () = over (); Lwt.return_unit in
        Lwt.finalize writeAndPreserveProgress unblock |> Lwt.ignore_result;
        k ()
      in
      reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
    in
    { Logs.report = report }
  in
  let setupLog style_renderer level =
    let style_renderer = match style_renderer with
      | None -> `None
      | Some renderer -> renderer
    in
    Fmt_tty.setup_std_outputs ~style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (lwt_reporter ());
  in
  let open Cmdliner in
  Term.(
    const setupLog
    $ Fmt_cli.style_renderer ()
    $ Logs_cli.level ~env:(Arg.env_var "ESY__LOG") ())

let eval ?(argv=Sys.argv) ~defaultCommand ~commands () =
  let result = Cmdliner.Term.eval_choice ~argv defaultCommand commands in
  Lwt_main.run (ProgressReporter.clearStatus ());
  result
