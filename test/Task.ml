open Esy

let cfg = {
  Config.
  esyVersion = "0.x.x";
  sandboxPath = Path.v "/tmp/__sandbox__";
  prefixPath = Path.v "/tmp/__prefix__";
  storePath = Path.v "/tmp/__store__";
  localStorePath = Path.v "/tmp/__local_store__";
  fastreplacestringCommand = Cmd.v "fastreplacestring.exe";
  esyBuildPackageCommand = Cmd.v "esy-build-package";
  esyInstallJsCommand = Cmd.v "esy-install.js";
}

module TestCommandExpr = struct
  let dep = Package.{
    id = "%dep%";
    name = "dep";
    version = "1.0.0";
    dependencies = [];
    buildCommands = None;
    installCommands = None;
    buildType = BuildType.InSource;
    sourceType = SourceType.Immutable;
    exportedEnv = [
      {
        ExportedEnv.
        name = "OK";
        value = "#{self.install / 'ok'}";
        exclusive = false;
        scope = Local;
      };
      {
        ExportedEnv.
        name = "OK_BY_NAME";
        value = "#{dep.install / 'ok-by-name'}";
        exclusive = false;
        scope = Local;
      }
    ];
    sandboxEnv = [];
    sourcePath = Config.ConfigPath.ofPath cfg (Path.v "/path");
    resolution = Some "ok";
  }

  let pkg = Package.{
    id = "%pkg%";
    name = "pkg";
    version = "1.0.0";
    dependencies = [Dependency dep];
    buildCommands = Some [
      CommandList.Command.Unparsed "cp ./hello #{self.bin}";
      CommandList.Command.Unparsed "cp ./hello2 #{pkg.bin}";
    ];
    installCommands = Some [CommandList.Command.Parsed ["cp"; "./man"; "#{self.man}"]];
    buildType = BuildType.InSource;
    sourceType = SourceType.Immutable;
    exportedEnv = [];
    sandboxEnv = [];
    sourcePath = Config.ConfigPath.ofPath cfg (Path.v "/path");
    resolution = Some "ok";
  }

  let task = Task.ofPackage pkg

  let check f =
    match task with
    | Ok task ->
      f task
    | Error err ->
      print_endline (Run.formatError err);
      false

  let%test "#{...} inside esy.build" =
    check (fun task ->
      Task.CommandList.equal
        task.buildCommands
        [["cp"; "./hello"; "%store%/s/pkg-1.0.0-d7d07b72/bin"];
         ["cp"; "./hello2"; "%store%/s/pkg-1.0.0-d7d07b72/bin"]]
    )

  let%test "#{self...} inside esy.install" =
    check (fun task ->
      Task.CommandList.equal
        task.installCommands
        [["cp"; "./man"; "%store%/s/pkg-1.0.0-d7d07b72/man"]]
    )

  let%test "#{...} inside esy.exportedEnv" =
    check (fun task ->
      let bindings = Environment.Closed.bindings task.env in
      let f = function
        | {Environment. name = "OK"; value = Value value; _} ->
          Some (value = "%store%/i/dep-1.0.0-54f35bf6/ok")
        | {Environment. name = "OK_BY_NAME"; value = Value value; _} ->
          Some (value = "%store%/i/dep-1.0.0-54f35bf6/ok-by-name")
        | _ ->
          None
      in
      not (
        bindings
        |> List.map f
        |> List.exists (function | Some false -> true | _ -> false)
      )
    )

end
