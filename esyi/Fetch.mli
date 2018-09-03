(**

  Fetch & install sandbox solution.

 *)

val fetch :
  sandbox:Sandbox.t
  -> Solution.t
  -> unit RunAsync.t
(** Fetch & install solution for the currently configured sandbox. *)

val isInstalled :
  sandbox:Sandbox.t
  -> Solution.t
  -> bool RunAsync.t
(** Check if the solution is installed. *)
