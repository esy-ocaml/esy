val make :
  ocamlopt:Path.t
  -> outputPath:Path.t
  -> concurrency:int
  -> Config.t
  -> BuildSandbox.t
  -> EsyInstall.Solution.Package.t
  -> unit RunAsync.t
(**
 * Produce an npm release for the [sandbox].
 *)
