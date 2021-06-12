open EsyPackageConfig;
open EsyInstall;
open EsyBuild;

let esyNativeInstallNpmReleasePath = {
  Path.(
    (v(Sys.argv[0]) |> parent |> parent)
    / "lib"
    / "esy"
    / "esyNativeInstallNpmRelease"
  );
};

type filterPackages =
  | ExcludeById(list(string))
  | IncludeByPkgSpec(list(PkgSpec.t));

type rewritePrefix =
  | Rewrite
  | NoRewrite;

type config = {
  name: string,
  version: string,
  license: option(Json.t),
  keywords: option(Json.t),
  description: option(string),
  bin: StringMap.t(string),
  filterPackages,
  rewritePrefix,
};

module OfPackageJson = {
  type bin =
    | ByList(list(string))
    | ByName(string)
    | ByNameMany(StringMap.t(string));

  let bin_of_yojson = json =>
    Result.Syntax.(
      switch (json) {
      | `String(name) => return(ByName(name))
      | `List(_) =>
        let%bind names = Json.Decode.(list(string))(json);
        return(ByList(names));
      | `Assoc(_) =>
        let%bind names = Json.Decode.(stringMap(string))(json);
        return(ByNameMany(names));

      | _ =>
        error({|"esy.release.bin": expected a string, array or an object|})
      }
    );

  [@deriving of_yojson]
  type release = {
    [@default None]
    includePackages: option(list(PkgSpec.t)),
    [@default None]
    releasedBinaries: option(list(string)),
    [@default None]
    bin: option(bin),
    [@default None]
    deleteFromBinaryRelease: option(list(string)),
    [@default None]
    rewritePrefix: option(bool),
  };

  [@deriving of_yojson({strict: false})]
  type t = {
    [@default "project"]
    name: string,
    [@default "0.0.0"]
    version: string,
    [@default None]
    license: option(Json.t),
    [@default None]
    description: option(string),
    [@default None]
    keywords: option(Json.t),
    [@default {release: None}]
    esy,
  }
  and esy = {
    [@default None]
    release: option(release),
  };
};

let configure = (spec: EsyInstall.SandboxSpec.t, ()) => {
  open RunAsync.Syntax;
  let docs = "https://esy.sh/docs/release.html";
  switch (spec.manifest) {
  | EsyInstall.SandboxSpec.ManifestAggregate(_)
  | [@implicit_arity] EsyInstall.SandboxSpec.Manifest(Opam, _) =>
    errorf(
      "could not create releases without package.json, see %s for details",
      docs,
    )
  | [@implicit_arity] EsyInstall.SandboxSpec.Manifest(Esy, filename) =>
    let%bind json = Fs.readJsonFile(Path.(spec.path / filename));
    let%bind pkgJson = RunAsync.ofStringError(OfPackageJson.of_yojson(json));
    switch (pkgJson.OfPackageJson.esy.release) {
    | None =>
      errorf(
        "no release config found in package.json, see %s for details",
        docs,
      )
    | Some(releaseCfg) =>
      let%bind filterPackages =
        switch (
          releaseCfg.includePackages,
          releaseCfg.deleteFromBinaryRelease,
        ) {
        | (None, None) => return(IncludeByPkgSpec([PkgSpec.Root]))
        | (Some(f), None) => return(IncludeByPkgSpec(f))
        | (None, Some(f)) => return(ExcludeById(f))
        | (Some(_), Some(_)) =>
          errorf(
            {|both "esy.release.deleteFromBinaryRelease" and "esy.release.includePackages" are specified, which is not allowed|},
          )
        };

      let%bind bin =
        switch (releaseCfg.bin, releaseCfg.releasedBinaries) {
        | (None, None) => errorf({|missing "esy.release.bin" configuration|})
        | (None, Some(names))
        | (Some(OfPackageJson.ByList(names)), None) =>
          let f = (bin, name) => StringMap.add(name, name, bin);
          return(List.fold_left(~f, ~init=StringMap.empty, names));
        | (Some(OfPackageJson.ByName(name)), None) =>
          return(StringMap.add(name, name, StringMap.empty))
        | (Some(OfPackageJson.ByNameMany(bin)), None) => return(bin)
        | (Some(_), Some(_)) =>
          errorf(
            {|both "esy.release.bin" and "esy.release.releasedBinaries" are specified, which is not allowed|},
          )
        };

      let rewritePrefix =
        switch (releaseCfg.rewritePrefix) {
        | None => NoRewrite
        | Some(false) => NoRewrite
        | Some(true) => Rewrite
        };

      return({
        name: pkgJson.name,
        version: pkgJson.version,
        license: pkgJson.license,
        keywords: pkgJson.keywords,
        description: pkgJson.description,
        bin,
        filterPackages,
        rewritePrefix,
      });
    };
  };
};

let makeBinWrapper =
    (~noEnv, ~destPrefix, ~bin, ~environment: Environment.Bindings.t) => {
  let path_sep =
    if (!Sys.unix) {
      '\\';
    } else {
      '/';
    };

  let path_sep_str = String.make(1, path_sep);

  let is_abs = p =>
    if (!Sys.unix) {
      switch (String.split_on_char(':', p)) {
      | [drive, ..._] when String.length(drive) == 1 => true
      | _ => false
      };
    } else {
      String.length(p) > 0 && p.[0] == '/';
    };

  let normalize = p => {
    let p = Str.global_substitute(Str.regexp("\\"), _ => "/", p);
    let parts = String.split_on_char('/', p);
    let need_leading_sep = Sys.unix && is_abs(p);
    let f = (parts, part) =>
      switch (part, parts) {
      | ("", parts) => parts
      | (".", parts) => parts
      | ("..", []) => parts
      | ("..", [part]) =>
        if (!Sys.unix) {
          [part];
        } else {
          [];
        }
      | ("..", [_, ...parts]) => parts
      | (part, parts) => [part, ...parts]
      };

    let p =
      String.concat(
        path_sep_str,
        List.rev(List.fold_left(~f, ~init=[], parts)),
      );
    if (need_leading_sep) {
      "/" ++ p;
    } else {
      p;
    };
  };

  let expandFallback = storePrefix => {
    let dummyPrefix = String.make(String.length(storePrefix), '_');
    let dirname = Filename.dirname(bin);
    let pattern = Str.regexp(dummyPrefix);
    let storePrefix = {
      let (/) = Filename.concat;
      normalize(dirname / ".." / "3");
    };

    let rewrite = value =>
      Str.global_substitute(pattern, _ => storePrefix, value);

    rewrite;
  };

  let storePrefix =
    Path.show(destPrefix)
    |> String.split_on_char('\\')
    |> String.concat("\\\\");

  /* TODO: When noEnv is true, environment bindings need not be embedded */
  let environmentString =
    environment
    |> Environment.renderToList
    |> List.filter(~f=((name, _)) =>
         switch (name) {
         | "SHELL"
         | "_"
         | "cur__original_root"
         | "cur__root" => false
         | _ => true
         }
       )
    |> List.map(~f=((name, value)) =>
         "{|"
         ++ name
         ++ "|}, {|"
         ++ EsyLib.Path.normalizePathSepOfFilename(value)
         ++ "|}"
       )
    |> List.append([
         "{|_|}, {|" ++ expandFallback(storePrefix, bin) ++ "|}",
       ])
    |> String.concat(";");

  Printf.sprintf(
    {|
    let windows = Sys.os_type = "Win32";;
    let cwd = Sys.getcwd ();;
    let path_sep = if windows then '\\' else '/';;
    let path_sep_str = String.make 1 path_sep;;

    let caseInsensitiveEqual i j = String.lowercase_ascii i = String.lowercase_ascii j;;
    let caseInsensitiveHash k = Hashtbl.hash (String.lowercase_ascii k);;

    module EnvHash =
      struct
        type t = string

        let equal = if (windows)
        then caseInsensitiveEqual
        else (=);;

        let hash = if windows
        then caseInsensitiveHash
        else Hashtbl.hash;;
      end

    module EnvHashtbl = Hashtbl.Make(EnvHash)

    let is_root p =
      if windows
      then
        match String.split_on_char ':' p with
        | [drive] when String.length drive = 1 -> true
        | [drive; p] when String.length drive = 1 && (String.equal p "/" || String.equal p "\\") -> true
        | _ -> false
      else String.equal p "/" || String.equal p "//"
    ;;

    let is_abs p =
      if windows
      then
        match String.split_on_char ':' p with
        | drive::_ when String.length drive = 1 -> true
        | _ -> false
      else String.length p > 0 && String.get p 0 = '/'
    ;;

    let normalize p =
      let p = Str.global_substitute (Str.regexp "\\") (fun _ -> "/") p in
      let parts = String.split_on_char '/' p in
      let need_leading_sep = not windows && is_abs p in
      let f parts part =
        match part, parts with
        | "", parts -> parts
        | ".", parts -> parts
        | "..", [] -> parts
        | "..", part::[] -> if windows then part::[] else []
        | "..", _::parts -> parts
        | part, parts -> part::parts
      in
      let p = String.concat path_sep_str (List.rev (List.fold_left f [] parts)) in
      if need_leading_sep
      then "/" ^ p
      else p
    ;;

    let is_symlink p =
      match Unix.lstat p with
      | {Unix.st_kind = Unix.S_LNK; _} -> true
      | _ -> false
      | exception Unix.Unix_error _ -> false
    ;;

    let rec resolve_path p =
      let p =
        if is_abs p
        then p
        else normalize (Filename.concat cwd p)
      in

      if is_root p then p
      else
        if is_symlink p then (
          let target = Unix.readlink p in
          if is_abs target
          then resolve_path target
          else resolve_path (normalize (Filename.concat (Filename.dirname p) target))
        ) else (
          Filename.concat (resolve_path (Filename.dirname p)) (Filename.basename p)
        )
    ;;

    let curEnvMap =
      let curEnv = Unix.environment () in
      let table = EnvHashtbl.create (Array.length curEnv) in
      let f item =
        try (
          let idx = String.index item '=' in
          let name = String.sub item 0 idx in
          let value = if (String.length item - 1) = idx then "" else String.sub item (idx + 1) (String.length item - idx - 1) in
          EnvHashtbl.replace table name value
        ) with Not_found -> ()
      in
      Array.iter f curEnv;
      table
    ;;

    let expandEnv env =
      let findVarRe = Str.regexp "\\$\\([a-zA-Z0-9_]+\\)" in
      let replace v =
        let name = Str.matched_group 1 v in
        try EnvHashtbl.find curEnvMap name
        with Not_found -> ""
      in
      let f (name, value) =
        let value = Str.global_substitute findVarRe replace value in
        EnvHashtbl.replace curEnvMap name value
      in
      Array.iter f env;
      let f name value items = (name ^ "=" ^ value)::items in
      Array.of_list (EnvHashtbl.fold f curEnvMap [])
    ;;

    let this_executable =
      resolve_path Sys.executable_name
    ;;

    (** this expands not rewritten store prefix _______ into a local release path *)
    let expandFallback storePrefix =
      let dummyPrefix = String.make (String.length storePrefix) '_' in
      let dirname = Filename.dirname this_executable in
      let pattern = Str.regexp dummyPrefix in
      let storePrefix =
        let (/) = Filename.concat in
        normalize (dirname / ".." / "3")
      in
      let rewrite value =
        Str.global_substitute pattern (fun _ -> storePrefix) value
      in
      rewrite
    ;;

    let expandFallbackEnv storePrefix env =
      Array.map (expandFallback storePrefix) env
    ;;

    let () =
      let env = [|%s|] in
      let program = "%s" in
      let storePrefix = "%s" in
      let no_wrapper = %s in
      let expandedEnv = expandFallbackEnv storePrefix (expandEnv env) in
      let shellEnv = match (Sys.getenv_opt "SHELL") with
      | Some v -> [| "SHELL=" ^ v |]
      | None -> [|"SHELL=/bin/sh"|]
      in
      let expandedEnv = Array.append expandedEnv shellEnv in
      if Array.length Sys.argv = 2 && Sys.argv.(1) = "----where" then
        print_endline (expandFallback storePrefix program)
      else if Array.length Sys.argv = 2 && Sys.argv.(1) = "----env" then
        Array.iter print_endline expandedEnv
      else (
        let program = expandFallback storePrefix program in
        Sys.argv.(0) <- program;
        if windows then (
          let pid = if no_wrapper then
            Unix.create_process program Sys.argv Unix.stdin Unix.stdout Unix.stderr
          else
            Unix.create_process_env program Sys.argv expandedEnv Unix.stdin Unix.stdout Unix.stderr
          in
          let (_, status) = Unix.waitpid [] pid in
          match status with
          | WEXITED code -> exit code
          | WSIGNALED code -> exit code
          | WSTOPPED code -> exit code
        )
        else
          Unix.execve program Sys.argv expandedEnv
      )
    ;;
  |},
    environmentString,
    bin,
    storePrefix,
    string_of_bool(noEnv),
  );
};

let envspec = {
  EnvSpec.buildIsInProgress: false,
  includeCurrentEnv: false,
  includeBuildEnv: false,
  includeNpmBin: false,
  includeEsyIntrospectionEnv: false,
  augmentDeps:
    Some(
      Solution.DepSpec.(
        package(self) + dependencies(self) + devDependencies(self)
      ),
    ),
};
let buildspec = {
  BuildSpec.all: Solution.DepSpec.(dependencies(self)),
  dev: Solution.DepSpec.(dependencies(self)),
};
let cleanupLinksFromGlobalStore = (cfg, tasks) => {
  open RunAsync.Syntax;
  let f = task =>
    switch (task.BuildSandbox.Task.pkg.source) {
    | PackageSource.Install(_) => return()
    | PackageSource.Link(_) =>
      let installPath = BuildSandbox.Task.installPath(cfg, task);
      Fs.rmPath(installPath);
    };

  RunAsync.List.mapAndWait(~f, tasks);
};

let make =
    (
      ~ocamlopt,
      ~createStatic,
      ~ocamlPkgName,
      ~ocamlVersion,
      ~noEnv,
      ~outputPath,
      ~concurrency,
      cfg: EsyBuildPackage.Config.t,
      spec: EsyInstall.SandboxSpec.t,
      sandbox: BuildSandbox.t,
      root,
    ) => {
  open RunAsync.Syntax;

  let%lwt () = Logs_lwt.app(m => m("Creating npm release"));
  let%bind releaseCfg = configure(spec, ()) /* * Construct a task tree with all tasks marked as immutable. This will make * sure all packages are built into a global store and this is required for * the release tarball as only globally stored artefacts can be relocated * between stores (b/c of a fixed path length). */;

  let%bind plan =
    RunAsync.ofRun(
      BuildSandbox.makePlan(~forceImmutable=true, buildspec, Build, sandbox),
    );
  let tasks = BuildSandbox.Plan.all(plan);

  let shouldDeleteFromEnv =
    switch (releaseCfg.filterPackages) {
    | IncludeByPkgSpec(specs) => (
        binding =>
          switch (Environment.Binding.origin(binding)) {
          | None => false
          | Some(pkgid) =>
            switch (PackageId.parse(pkgid)) {
            | Error(_) => false
            | Ok(pkgid) =>
              let f = spec => PkgSpec.matches(root.Package.id, spec, pkgid);
              let included = List.exists(~f, specs);
              !included;
            }
          }
      )
    | ExcludeById(_) => (_ => false)
    };

  let shouldDeleteFromBinaryRelease =
    switch (releaseCfg.filterPackages) {
    | IncludeByPkgSpec(specs) => (
        (pkgid, _buildid) => {
          let f = spec => PkgSpec.matches(root.Package.id, spec, pkgid);
          let included = List.exists(~f, specs);
          !included;
        }
      )
    | ExcludeById(patterns) =>
      let patterns = {
        let f = pattern => pattern |> Re.Glob.glob |> Re.compile;
        List.map(~f, patterns);
      };

      let filterOut = (_pkgid, buildid) => {
        let buildid = BuildId.show(buildid);
        List.exists(~f=pattern => Re.execp(pattern, buildid), patterns);
      };

      filterOut;
    } /* Make sure all packages are built */;

  let%bind () = {
    let%lwt () = Logs_lwt.app(m => m("Building packages"));
    BuildSandbox.build(
      ~buildLinked=true,
      ~skipStalenessCheck=true,
      ~concurrency,
      sandbox,
      plan,
      [root.Package.id],
    );
  };

  let%bind () = Fs.createDir(outputPath) /* Export builds */;

  let%bind () = {
    let%lwt () =
      switch (releaseCfg.filterPackages) {
      | IncludeByPkgSpec(specs) =>
        let f = (unused, spec) => {
          let f = task =>
            PkgSpec.matches(root.id, spec, task.BuildSandbox.Task.pkg.id);
          List.exists(~f, tasks) ? unused : [spec, ...unused];
        };

        switch (List.fold_left(~f, ~init=[], specs)) {
        | [] => Lwt.return()
        | unused =>
          Logs_lwt.warn(m =>
            m(
              {|found unused package specs in "esy.release.includePackages": %a|},
              Fmt.(list(~sep=unit(", "), PkgSpec.pp)),
              unused,
            )
          )
        };
      | _ => Lwt.return()
      };

    let%lwt () = Logs_lwt.app(m => m("Exporting built packages"));
    let f = (task: BuildSandbox.Task.t) => {
      let id = Scope.id(task.scope);
      if (shouldDeleteFromBinaryRelease(task.pkg.id, id)) {
        let%lwt () =
          Logs_lwt.app(m =>
            m("Skipping %a", PackageId.ppNoHash, task.pkg.id)
          );
        return();
      } else {
        let buildPath = BuildSandbox.Task.installPath(cfg, task);
        let outputPrefixPath = Path.(outputPath / "_export");
        BuildSandbox.exportBuild(cfg, ~outputPrefixPath, buildPath);
      };
    };

    RunAsync.List.mapAndWait(~concurrency=8, ~f, tasks);
  };

  let%bind () = {
    let%lwt () = Logs_lwt.app(m => m("Configuring release"));
    let binPath = Path.(outputPath / "bin");
    let%bind () = Fs.createDir(binPath) /* Emit wrappers for released binaries */;

    let%bind () = {
      let%bind bindings =
        RunAsync.ofRun(
          BuildSandbox.env(
            ~forceImmutable=true,
            envspec,
            buildspec,
            Build,
            sandbox,
            root.Package.id,
          ),
        );
      let bindings = Scope.SandboxEnvironment.Bindings.render(cfg, bindings);

      let bindings =
        List.filter(~f=binding => !shouldDeleteFromEnv(binding), bindings);

      let%bind env =
        RunAsync.ofStringError(Environment.Bindings.eval(bindings));

      let generateBinaryWrapper =
          (stagePath, destPrefix, (publicName, innerName)) => {
        let resolveBinInEnv = (~env, prg) => {
          let path = {
            let v =
              switch (StringMap.find_opt("PATH", env)) {
              | Some(v) => v
              | None => ""
              };

            String.split_on_char(System.Environment.sep().[0], v);
          };
          RunAsync.ofRun(Run.ofBosError(Cmd.resolveCmd(path, prg)));
        };

        let%bind namePath = resolveBinInEnv(~env, innerName) /* Create the .ml file that we will later compile and write it to disk */;
        let data =
          makeBinWrapper(
            ~noEnv,
            ~destPrefix,
            ~environment=bindings,
            ~bin=EsyLib.Path.normalizePathSepOfFilename(namePath),
          );

        let mlPath = Path.(stagePath / (innerName ++ ".ml"));
        let%bind () = Fs.writeFile(~data, mlPath) /* Compile the wrapper to a binary */;
        let ocamloptCmd =
          Cmd.(
            createStatic
              ? v(EsyLib.Path.normalizePathSepOfFilename(p(ocamlopt)))
                % "-ccopt"
                % "-static"
              : v(EsyLib.Path.normalizePathSepOfFilename(p(ocamlopt)))
          );
        let compile =
          Cmd.(
            ocamloptCmd
            % "-o"
            % EsyLib.Path.normalizePathSepOfFilename(
                p(Path.(binPath / publicName)),
              )
            % "unix.cmxa"
            % "str.cmxa"
            % EsyLib.Path.normalizePathSepOfFilename(p(mlPath))
          ) /* Needs to have ocaml in environment */;
        let%bind env =
          switch (System.Platform.host) {
          | Windows =>
            let currentPath = Sys.getenv("PATH");
            let userPath = EsyBash.getBinPath();
            let normalizedOcamlPath =
              ocamlopt |> Path.parent |> Path.showNormalized;
            let override = {
              let sep = System.Environment.sep();
              let path =
                String.concat(
                  sep,
                  [Path.show(userPath), normalizedOcamlPath, currentPath],
                );
              StringMap.(add("PATH", path, empty));
            };

            return(ChildProcess.CurrentEnvOverride(override));
          | _ => return(ChildProcess.CurrentEnv)
          };

        ChildProcess.run(~env, compile);
      };

      let (origPrefix, destPrefix) = {
        let destPrefix =
          switch (releaseCfg.rewritePrefix, System.Platform.host) {
          | (Rewrite, Windows) =>
            /* Keep the slashes segments in the path.  It's important for doing
             * replacement of double backslashes in artifacts.  */
            String.split_on_char('\\', cfg.storePath |> Path.show)
            |> List.map(~f=seg => String.make(String.length(seg), '_'))
            |> String.concat("\\")
          | _ =>
            String.make(
              String.length(Path.show(cfg.EsyBuildPackage.Config.storePath)),
              '_',
            )
          };
        (cfg.storePath, Path.v(destPrefix));
      };

      let%bind () =
        Fs.withTempDir(stagePath =>
          RunAsync.List.mapAndWait(
            ~f=generateBinaryWrapper(stagePath, destPrefix),
            StringMap.bindings(releaseCfg.bin),
          )
        );

      let%bind () = {
        /* Replace the storePath with a string of equal length containing only _ */
        let%bind () =
          Fs.writeFile(
            ~data=Path.show(destPrefix),
            Path.(binPath / "_storePath"),
          );
        let%bind () =
          RewritePrefix.rewritePrefix(~origPrefix, ~destPrefix, binPath);
        return();
      };

      return();
    } /* Emit package.json */;

    let%bind () = {
      let postinstall =
        switch (releaseCfg.rewritePrefix) {
        | NoRewrite =>
          Printf.sprintf(
            "./esyInstallRelease --ocaml-version='%s' --ocaml-pkg-name='%s' --rewrite-prefix=false",
            ocamlPkgName,
            ocamlVersion,
          )
        | Rewrite =>
          Printf.sprintf(
            "./esyInstallRelease --ocaml-version='%s' --ocaml-pkg-name='%s' --rewrite-prefix=true",
            ocamlPkgName,
            ocamlVersion,
          )
        };

      let pkgJson = {
        let items = [
          ("name", `String(releaseCfg.name)),
          ("version", `String(releaseCfg.version)),
          ("scripts", `Assoc([("postinstall", `String(postinstall))])),
          (
            "bin",
            `Assoc(
              {
                let f = ((publicName, _innerName)) => {
                  let binName =
                    switch (System.Platform.host) {
                    | Windows =>
                      if (Path.hasExt("exe", Path.v(publicName))) {
                        publicName;
                      } else {
                        publicName ++ ".exe";
                      }
                    | _ => publicName
                    };

                  (publicName, `String("bin/" ++ binName));
                };

                List.map(~f, StringMap.bindings(releaseCfg.bin));
              },
            ),
          ),
        ];

        let items =
          switch (releaseCfg.license) {
          | Some(license) => [("license", license), ...items]
          | None => items
          };

        let items =
          switch (releaseCfg.keywords) {
          | Some(keywords) => [("keywords", keywords), ...items]
          | None => items
          };

        let items =
          switch (releaseCfg.description) {
          | Some(description) => [
              ("description", `String(description)),
              ...items,
            ]
          | None => items
          };

        `Assoc(items);
      };

      let data = Yojson.Safe.pretty_to_string(pkgJson);
      Fs.writeFile(~data, Path.(outputPath / "package.json"));
    };

    // let%bind () =
    //   Fs.copyFile(
    //     ~src=esyNativeInstallNpmReleasePath,
    //     ~dst=Path.(outputPath / "esyInstallRelease"),
    //   );
    let%bind () = {
      let f = filename => {
        let src = Path.(spec.path / filename);
        if%bind (Fs.exists(src)) {
          Fs.copyFile(~src, ~dst=Path.(outputPath / filename));
        } else {
          return();
        };
      };

      RunAsync.List.mapAndWait(
        ~f,
        [
          "README.md",
          "README",
          "LICENSE.md",
          "LICENSE",
          "LICENCE.md",
          "LICENCE",
        ],
      );
    };

    return();
  } /*** Cleanup linked packages from global store */;

  let%bind () = cleanupLinksFromGlobalStore(cfg, tasks);

  let%lwt () = Logs_lwt.app(m => m("Done!"));
  return();
};

let run = (createStatic: bool, noEnv: bool, proj: Project.t) => {
  open RunAsync.Syntax;

  let%bind solved = Project.solved(proj);
  let%bind fetched = Project.fetched(proj);
  let%bind configured = Project.configured(proj);
  let ocamlPkgName = proj.projcfg.ocamlPkgName;
  let ocamlVersion = proj.projcfg.ocamlVersion;

  let%bind outputPath = {
    let outputDir = "_release";
    let outputPath = Path.(proj.buildCfg.projectPath / outputDir);
    let%bind () = Fs.rmPath(outputPath);
    return(outputPath);
  };

  let%bind ocamlopt = {
    let%bind () =
      Project.buildDependencies(
        ~buildLinked=true,
        proj,
        configured.Project.planForDev,
        configured.Project.root.pkg,
      );

    let%bind p = Project.ocaml(proj);
    return(Path.(p / "bin" / "ocamlopt"));
  };

  make(
    ~ocamlopt,
    ~createStatic,
    ~ocamlPkgName,
    ~ocamlVersion,
    ~noEnv,
    ~outputPath,
    ~concurrency=
      EsyRuntime.concurrency(proj.projcfg.ProjectConfig.buildConcurrency),
    proj.buildCfg,
    proj.projcfg.ProjectConfig.spec,
    fetched.Project.sandbox,
    Solution.root(solved.Project.solution),
  );
};
