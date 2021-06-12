open Cmdliner;

let esyStoreVersion = Store.version;

module Path = {
  include Path;
  let join = (x, y) => Filename.concat(x, y) |> Path.v;
};

let storeBuildTree = Store.buildTree;
let storeInstallTree = Store.installTree;
let storeStageTree = Store.stageTree;
let cwd = Sys.getcwd();
let releasePackagePath = cwd;
let releaseExportPath = Path.(v(releasePackagePath) / "_export");
let releaseBinPath = Path.(v(releasePackagePath) / "bin");

type rewritePrefix =
  | Rewrite(Path.t)
  | NoRewrite(Path.t);

let getStorePathForPrefix = (prefix, ocamlPkgName, ocamlVersion) => {
  switch (Store.getPadding(~ocamlPkgName, ~ocamlVersion, Path.v(prefix))) {
  | Error(err) => Error(err)
  | Ok(padding) => Ok(Path.join(prefix, esyStoreVersion ++ padding))
  };
};

let importBuild = (filePath, rewritePrefix) => {
  RunAsync.Syntax.(
    switch (rewritePrefix) {
    | Rewrite(storePath) =>
      EsyBuild.BuildSandbox.importBuild(storePath, filePath)
    | NoRewrite(storePath) =>
      let buildId =
        Str.global_replace(
          Str.regexp(".tar.gz$"),
          "",
          Fpath.basename(filePath),
        );
      let storeStagePath = Path.(storePath / storeStageTree);
      let buildStagePath = Path.(storeStagePath / buildId);
      let buildFinalPath = Path.(storePath / storeInstallTree / buildId);

      let%bind () = Fs.createDir(buildStagePath);
      let%bind () = Tarball.unpack(filePath, ~dst=storeStagePath);
      Fs.rename(~src=buildStagePath, buildFinalPath);
    }
  );
};

let main = rewritePrefix => {
  let storePath =
    switch (rewritePrefix) {
    | Rewrite(path)
    | NoRewrite(path) => path
    };

  let check = () => {
    let%lwt buildFound = Fs.exists(releaseExportPath);
    switch (buildFound) {
    | Ok(true) =>
      switch (rewritePrefix) {
      | NoRewrite(_) => Lwt.return(Ok())
      | Rewrite(storePath) =>
        let%lwt storeFound = Fs.exists(storePath);
        Lwt.return(
          switch (storeFound) {
          | Ok(true) => Error(`ReleaseAlreadyInstalled)
          | Ok(false) => Ok()
          | Error(err) => Error(`EsyLibError(err))
          },
        );
      }
    | Ok(false) => Lwt.return(Error(`NoBuildFound))
    | Error(err) => Lwt.return(Error(`EsyLibError(err)))
    };
  };
  let initStore = () => {
    Lwt_result.(
      Fs.createDir(storePath)
      >>= (
        _ =>
          RunAsync.List.waitAll([
            Fs.createDir(Path.(storePath / storeBuildTree)),
            Fs.createDir(Path.(storePath / storeInstallTree)),
            Fs.createDir(Path.(storePath / storeStageTree)),
          ])
      )
      |> Lwt_result.map_err(err => `EsyLibError(err))
    );
  };
  let doImport = () => {
    open RunAsync.Syntax;
    let importBuilds = () => {
      open RunAsync.Syntax;
      let%bind files = Fs.listDir(releaseExportPath);

      RunAsync.List.mapAndJoin(
        ~f=file => importBuild(file, rewritePrefix),
        files |> List.map(~f=Path.addSeg(releaseExportPath)),
      );
    };
    let rewriteBinWrappers = () =>
      switch (rewritePrefix) {
      | NoRewrite(_) => Lwt_result.return()
      | Rewrite(storePath) =>
        let%bind prevStorePath =
          Fs.readFile(Path.(releaseBinPath / "_storePath"));
        RewritePrefix.rewritePrefix(
          ~origPrefix=Path.v(prevStorePath),
          ~destPrefix=storePath,
          releaseBinPath,
        );
      };
    let%bind _ = importBuilds();
    rewriteBinWrappers();
  };
  Lwt_result.(
    check()
    >>= initStore
    >>= (_ => doImport() |> Lwt_result.map_err(err => `EsyLibError(err)))
    >>= (
      _ => {
        switch (System.Platform.host, System.Arch.host) {
        | (Darwin, Arm64) =>
          print_endline("Detected macOS arm64. Signing binaries...");
          Result.Syntax.Let_syntax.bind(
            EsyBuildPackage.Build.getMachOBins(
              (module EsyBuildPackage.Run),
              [],
              Path.v(releasePackagePath),
            ),
            ~f=entries =>
            EsyBuildPackage.BigSurArm.sign(entries)
          )
          |> Lwt.return;
        | _ => Lwt.return(Ok())
        };
      }
    )
  );
};

let lwt_main = (ocamlPkgName, ocamlVersion, shouldRewritePrefix) => {
  let unpaddedStorePath = Path.(v(releasePackagePath) / esyStoreVersion);

  let rewritePrefixResult =
    shouldRewritePrefix
      ? getStorePathForPrefix(releasePackagePath, ocamlPkgName, ocamlVersion)
        |> Result.map(~f=storePath => Rewrite(storePath))
      : Ok(NoRewrite(unpaddedStorePath));

  let result =
    rewritePrefixResult
    |> Result.Syntax.Let_syntax.bind(~f=rewritePrefix => {
         Lwt_main.run(main(rewritePrefix))
       });
  switch (result) {
  | Ok(_) => print_endline("Done!")
  | Error(`NoBuildFound) => Printf.eprintf("No build found!")
  | Error(`ReleaseAlreadyInstalled) =>
    Printf.eprintf("Release already installed!")
  | Error(`Msg(msg)) => Printf.eprintf("%s", msg)
  | Error(`EsyLibError(err)) =>
    Printf.eprintf("%s", EsyLib.Run.formatError(err))
  | Error(`CommandError(cmd, _)) => Bos.Cmd.pp(Format.err_formatter, cmd)
  };
};
let ocamlPkgName = {
  let doc = "OCaml package name";
  Arg.(
    value
    & opt(string, "ocaml")
    & info(["ocaml-pkg-name"], ~docv="RELEASE CONFIG", ~doc)
  );
};

let ocamlVersion = {
  let doc = "OCaml package version";
  Arg.(
    value
    & opt(string, "n.00.0000")
    & info(["ocaml-version"], ~docv="RELEASE CONFIG", ~doc)
  );
};

let rewritePrefix = {
  let doc = "Whether to rewrite prefixes in the binary or not";
  Arg.(
    value
    & opt(bool, true)
    & info(["rewrite-prefix"], ~docv="RELEASE CONFIG", ~doc)
  );
};

let main_t =
  Term.(const(lwt_main) $ ocamlPkgName $ ocamlVersion $ rewritePrefix);

let info = {
  let doc = "Export native builds and rewrite prefixes in them";
  let man = [`S(Manpage.s_bugs), `P("")];

  Term.info(
    "NatEsyInstallRelease",
    ~version="0.0.0",
    ~doc,
    ~exits=Term.default_exits,
    ~man,
  );
};

let () = Term.exit @@ Term.eval((main_t, info));
