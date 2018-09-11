(** Sandbox *)

type t = {
  (** Configuration. *)
  cfg : Config.t;

  spec : SandboxSpec.t;

  (** Root package. *)
  root : Package.t;

  (**
   * A set of dependencies to be installed for the sandbox.
   *
   * Such dependencies are different than of root.dependencies as sandbox
   * aggregates both regular dependencies and devDependencies.
   *)
  dependencies : Package.Dependencies.t;

  (** A set of resolutions. *)
  resolutions : Package.Resolutions.t;

  (** OCaml version request defined for the sandbox. *)
  ocamlReq : Req.t option;
}

val make : cfg:Config.t -> SandboxSpec.t -> t RunAsync.t
