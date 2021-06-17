open EsyPackageConfig;

module Dependencies = InstallManifest.Dependencies;

let computeOverrideDigest = (sandbox, override) =>
  RunAsync.Syntax.(
    switch (override) {
    | Override.OfJson({json}) => return(Digestv.ofJson(json))
    | OfDist({dist, json: _}) => return(Digestv.ofString(Dist.show(dist)))
    | OfOpamOverride(info) =>
      let* files =
        EsyInstall.Fetch.fetchOverrideFiles(
          sandbox.Sandbox.cfg.installCfg,
          sandbox.spec,
          override,
        );

      let* digests = RunAsync.List.mapAndJoin(~f=File.digest, files);
      let digest = Digestv.ofJson(info.json);
      let digests = [digest, ...digests];
      let digests = List.sort(~cmp=Digestv.compare, digests);
      return(
        List.fold_left(~init=Digestv.empty, ~f=Digestv.combine, digests),
      );
    }
  );

let computeOverridesDigest = (sandbox, overrides) => {
  open RunAsync.Syntax;
  let* digests =
    RunAsync.List.mapAndJoin(~f=computeOverrideDigest(sandbox), overrides);
  return(List.fold_left(~init=Digestv.empty, ~f=Digestv.combine, digests));
};

let lock = (sandbox, pkg: InstallManifest.t) =>
  RunAsync.Syntax.(
    switch (pkg.source) {
    | Install({source: _, opam: Some(opam)}) =>
      let* id = {
        let* opamDigest = OpamResolution.digest(opam);
        let* overridesDigest = computeOverridesDigest(sandbox, pkg.overrides);
        let digest = Digestv.(opamDigest + overridesDigest);
        return(PackageId.make(pkg.name, pkg.version, Some(digest)));
      };

      return((id, pkg));
    | Install({source: _, opam: None}) =>
      let* id = {
        let* digest = computeOverridesDigest(sandbox, pkg.overrides);
        return(PackageId.make(pkg.name, pkg.version, Some(digest)));
      };

      return((id, pkg));
    | Link(_) =>
      let id = PackageId.make(pkg.name, pkg.version, None);
      return((id, pkg));
    }
  );

module Strategy = {
  let trendy = "-count[staleness,solution]";
  /* let minimalAddition = "-removed,-changed,-notuptodate" */
};

type t = {
  universe: Universe.t,
  solvespec: SolveSpec.t,
  sandbox: Sandbox.t,
};

let evalDependencies = (solver, manifest) =>
  SolveSpec.eval(solver.solvespec, manifest);

module Reason: {
  type t
  and chain = {
    constr: Dependencies.t,
    trace,
  }
  and trace = list(InstallManifest.t);

  let pp: Fmt.t(t);

  let conflict: (chain, chain) => t;
  let missing: (~available: list(Resolution.t)=?, chain) => t;

  module Set: Set.S with type elt := t;
} = {
  [@deriving ord]
  type t =
    | Conflict(chain, chain)
    | Missing({
        chain,
        available: list(Resolution.t),
      })
  and chain = {
    constr: Dependencies.t,
    trace,
  }
  and trace = list(InstallManifest.t);

  let conflict = (left, right) =>
    if (compare_chain(left, right) <= 0) {
      Conflict(left, right);
    } else {
      Conflict(right, left);
    };

  let missing = (~available=[], chain) => Missing({chain, available});

  let ppTrace = (fmt, path) => {
    let ppPkgName = (fmt, pkg) => {
      let name =
        Option.orDefault(~default=pkg.InstallManifest.name, pkg.originalName);
      Fmt.string(fmt, name);
    };

    let sep = Fmt.unit(" -> ");
    Fmt.(hbox(list(~sep, ppPkgName)))(fmt, List.rev(path));
  };

  let ppChain = (fmt, {constr, trace}) =>
    switch (trace) {
    | [] => Fmt.pf(fmt, "%a", Dependencies.pp, constr)
    | trace =>
      Fmt.pf(fmt, "%a -> %a", ppTrace, trace, Dependencies.pp, constr)
    };

  let pp = fmt =>
    fun
    | Missing({chain, available: []}) =>
      Fmt.pf(fmt, "No package matching:@;@[<v 2>@;%a@;@]", ppChain, chain)
    | Missing({chain, available}) =>
      Fmt.pf(
        fmt,
        "No package matching:@;@[<v 2>@;%a@;@;Versions available:@;@[<v 2>@;%a@]@]",
        ppChain,
        chain,
        Fmt.list(Resolution.pp),
        available,
      )
    | Conflict(left, right) =>
      Fmt.pf(
        fmt,
        "@[<v 2>Conflicting constraints:@;%a@;%a@]",
        ppChain,
        left,
        ppChain,
        right,
      );

  module Set =
    Set.Make({
      type nonrec t = t;
      let compare = compare;
    });
};

module Explanation = {
  type t = list(Reason.t);

  let empty: t = ([]: t);

  let pp = (fmt, reasons) => {
    let ppReasons = (fmt, reasons) => {
      let sep = Fmt.unit("@;@;");
      Fmt.pf(fmt, "@[<v>%a@;@]", Fmt.list(~sep, Reason.pp), reasons);
    };

    Fmt.pf(fmt, "@[<v>No solution found:@;@;%a@]", ppReasons, reasons);
  };

  let collectReasons =
      (~gitUsername, ~gitPassword, cudfMapping, solver, reasons) => {
    open RunAsync.Syntax;

    /* Find a pair of requestor, path for the current package.
     * Note that there can be multiple paths in the dependency graph but we only
     * consider one of them.
     */
    let resolveDepChain = pkg => {
      let map = {
        let f = map =>
          fun
          | Dose_algo.Diagnostic.Dependency((pkg, _, _))
              when pkg.Cudf.package == "dose-dummy-request" => map
          | Dose_algo.Diagnostic.Dependency((pkg, _, deplist)) => {
              let pkg = Universe.CudfMapping.decodePkgExn(pkg, cudfMapping);
              let f = (map, dep) => {
                let dep = Universe.CudfMapping.decodePkgExn(dep, cudfMapping);
                InstallManifest.Map.add(dep, pkg, map);
              };

              List.fold_left(~f, ~init=map, deplist);
            }
          | _ => map;

        let map = InstallManifest.Map.empty;
        List.fold_left(~f, ~init=map, reasons);
      };

      let resolve = pkg =>
        if (pkg.InstallManifest.name
            == solver.sandbox.root.InstallManifest.name) {
          (pkg, []);
        } else {
          let rec aux = (path, pkg) =>
            switch (InstallManifest.Map.find_opt(pkg, map)) {
            | None => [pkg, ...path]
            | Some(npkg) => aux([pkg, ...path], npkg)
            };

          switch (List.rev(aux([], pkg))) {
          | []
          | [_] => failwith("inconsistent state: empty dep path")
          | [_, requestor, ...path] => (requestor, path)
          };
        };

      resolve(pkg);
    };

    let resolveReqViaDepChain = pkg => {
      let (requestor, path) = resolveDepChain(pkg);
      (requestor, path);
    };

    let maybeEvalDependencies = manifest =>
      switch (evalDependencies(solver, manifest)) {
      | Ok(deps) => deps
      | Error(_) => Dependencies.NpmFormula([])
      };

    let* reasons = {
      let f = reasons =>
        fun
        | Dose_algo.Diagnostic.Conflict((left, right, _)) => {
            let left = {
              let pkg = Universe.CudfMapping.decodePkgExn(left, cudfMapping);
              let (requestor, path) = resolveReqViaDepChain(pkg);
              let constr =
                Dependencies.filterDependenciesByName(
                  ~name=pkg.name,
                  maybeEvalDependencies(requestor),
                );

              {Reason.constr, trace: [requestor, ...path]};
            };

            let right = {
              let pkg = Universe.CudfMapping.decodePkgExn(right, cudfMapping);
              let (requestor, path) = resolveReqViaDepChain(pkg);
              let constr =
                Dependencies.filterDependenciesByName(
                  ~name=pkg.name,
                  maybeEvalDependencies(requestor),
                );

              {Reason.constr, trace: [requestor, ...path]};
            };

            let conflict = Reason.conflict(left, right);
            if (!Reason.Set.mem(conflict, reasons)) {
              return(Reason.Set.add(conflict, reasons));
            } else {
              return(reasons);
            };
          }
        | Dose_algo.Diagnostic.Missing((pkg, vpkglist)) => {
            let pkg = Universe.CudfMapping.decodePkgExn(pkg, cudfMapping);
            let (requestor, path) = resolveDepChain(pkg);
            let trace =
              if (pkg.InstallManifest.name
                  == solver.sandbox.root.InstallManifest.name) {
                [];
              } else {
                [pkg, requestor, ...path];
              };

            let f = (reasons, (name, _)) => {
              let name =
                Universe.CudfMapping.decodePkgName(
                  Universe.CudfName.make(name),
                );
              let%lwt available =
                switch%lwt (
                  Resolver.resolve(
                    ~gitUsername,
                    ~gitPassword,
                    ~name,
                    solver.sandbox.resolver,
                  )
                ) {
                | Ok(available) => Lwt.return(available)
                | Error(_) => Lwt.return([])
                };

              let constr =
                Dependencies.filterDependenciesByName(
                  ~name,
                  maybeEvalDependencies(pkg),
                );
              let missing = Reason.missing(~available, {constr, trace});
              if (!Reason.Set.mem(missing, reasons)) {
                return(Reason.Set.add(missing, reasons));
              } else {
                return(reasons);
              };
            };

            RunAsync.List.foldLeft(~f, ~init=reasons, vpkglist);
          }
        | _ => return(reasons);

      RunAsync.List.foldLeft(~f, ~init=Reason.Set.empty, reasons);
    };

    return(Reason.Set.elements(reasons));
  };

  let explain = (~gitUsername, ~gitPassword, cudfMapping, solver, cudf) =>
    RunAsync.Syntax.(
      switch (Dose_algo.Depsolver.check_request(~explain=true, cudf)) {
      | Dose_algo.Depsolver.Sat(_)
      | Dose_algo.Depsolver.Unsat(None)
      | Dose_algo.Depsolver.Unsat(
          Some({result: Dose_algo.Diagnostic.Success(_), _}),
        ) =>
        return(None)
      | Dose_algo.Depsolver.Unsat(
          Some({result: Dose_algo.Diagnostic.Failure(reasons), _}),
        ) =>
        let reasons = reasons();
        let* reasons =
          collectReasons(
            ~gitUsername,
            ~gitPassword,
            cudfMapping,
            solver,
            reasons,
          );
        return(Some(reasons));
      | Dose_algo.Depsolver.Error(err) => error(err)
      }
    );
};

let rec findResolutionForRequest = (resolver, req) =>
  fun
  | [] => None
  | [res, ...rest] => {
      let version =
        switch (res.Resolution.resolution) {
        | Version(version) => version
        | SourceOverride({source, _}) => Version.Source(source)
        };

      if (Resolver.versionMatchesReq(
            resolver,
            req,
            res.Resolution.name,
            version,
          )) {
        Some(res);
      } else {
        findResolutionForRequest(resolver, req, rest);
      };
    };

let lockPackage =
    (
      resolver,
      id: PackageId.t,
      pkg: InstallManifest.t,
      dependenciesMap: StringMap.t(PackageId.t),
      allDependenciesMap: StringMap.t(Version.Map.t(PackageId.t)),
    ) => {
  open RunAsync.Syntax;

  let {
    InstallManifest.name,
    version,
    originalVersion: _,
    originalName: _,
    source,
    overrides,
    dependencies,
    devDependencies,
    peerDependencies,
    optDependencies,
    resolutions: _,
    kind: _,
    installConfig,
    extraSources,
  } = pkg;

  let idsOfDependencies = dependencies =>
    dependencies
    |> Dependencies.toApproximateRequests
    |> List.map(~f=req => StringMap.find(req.Req.name, dependenciesMap))
    |> List.filterNone
    |> PackageId.Set.of_list;

  let optDependencies = {
    let f = name =>
      switch (StringMap.find(name, dependenciesMap)) {
      | Some(dep) => Some(dep)
      | None =>
        switch (StringMap.find(name, allDependenciesMap)) {
        | Some(versions) =>
          let (_version, id) = Version.Map.find_first(_ => true, versions);
          Some(id);
        | None => None
        }
      };

    optDependencies
    |> StringSet.elements
    |> List.map(~f)
    |> List.filterNone
    |> PackageId.Set.of_list;
  };

  let peerDependencies = {
    let f = req => {
      let versions =
        switch (StringMap.find(req.Req.name, allDependenciesMap)) {
        | Some(versions) => versions
        | None => Version.Map.empty
        };

      let versions = List.rev(Version.Map.bindings(versions));
      let f = ((version, _id)) =>
        Resolver.versionMatchesReq(resolver, req, req.Req.name, version);

      switch (List.find_opt(~f, versions)) {
      | Some((_version, id)) => Some(id)
      | None => None
      };
    };

    peerDependencies
    |> List.map(~f)
    |> List.filterNone
    |> PackageId.Set.of_list;
  };

  let dependencies = {
    let dependencies = idsOfDependencies(dependencies);
    dependencies
    |> PackageId.Set.union(optDependencies)
    |> PackageId.Set.union(peerDependencies);
  };

  let devDependencies = idsOfDependencies(devDependencies);
  let source =
    switch (source) {
    | PackageSource.Link(link) => PackageSource.Link(link)
    | Install({source, opam: None}) =>
      PackageSource.Install({source, opam: None})
    | Install({source, opam: Some(opam)}) =>
      PackageSource.Install({source, opam: Some(opam)})
    };

  return({
    EsyInstall.Package.id,
    name,
    version,
    source,
    overrides,
    dependencies,
    devDependencies,
    installConfig,
    extraSources,
  });
};

let make = (solvespec, sandbox: Sandbox.t) => {
  open RunAsync.Syntax;
  let universe = ref(Universe.empty(sandbox.resolver));
  return({solvespec, universe: universe^, sandbox});
};

let add = (~gitUsername, ~gitPassword, ~dependencies: Dependencies.t, solver) => {
  open RunAsync.Syntax;

  let universe = ref(solver.universe);
  let (report, finish) =
    Cli.createProgressReporter(~name="resolving esy packages", ());

  let rec addPackage = (manifest: InstallManifest.t) =>
    if (!Universe.mem(~pkg=manifest, universe^)) {
      switch (manifest.kind) {
      | InstallManifest.Esy =>
        universe := Universe.add(~pkg=manifest, universe^);
        let* dependencies =
          RunAsync.ofRun(evalDependencies(solver, manifest));
        let* () =
          RunAsync.contextf(
            addDependencies(dependencies),
            "resolving %a",
            InstallManifest.pp,
            manifest,
          );

        universe := Universe.add(~pkg=manifest, universe^);
        return();
      | InstallManifest.Npm => return()
      };
    } else {
      return();
    }
  and addDependencies = (dependencies: Dependencies.t) =>
    switch (dependencies) {
    | Dependencies.NpmFormula(reqs) =>
      let f = (req: Req.t) => addDependency(req);
      RunAsync.List.mapAndWait(~f, reqs);

    | Dependencies.OpamFormula(_) =>
      let f = (req: Req.t) => addDependency(req);
      let reqs = Dependencies.toApproximateRequests(dependencies);
      RunAsync.List.mapAndWait(~f, reqs);
    }
  and addDependency = (req: Req.t) => {
    let%lwt () = report("%s", req.name);
    let* resolutions =
      RunAsync.contextf(
        Resolver.resolve(
          ~gitUsername,
          ~gitPassword,
          ~fullMetadata=true,
          ~name=req.name,
          ~spec=req.spec,
          solver.sandbox.resolver,
        ),
        "resolving %a",
        Req.pp,
        req,
      );

    let* packages = {
      let fetchPackage = resolution => {
        let* pkg =
          RunAsync.contextf(
            Resolver.package(
              ~gitUsername,
              ~gitPassword,
              ~resolution,
              solver.sandbox.resolver,
            ),
            "resolving metadata %a",
            Resolution.pp,
            resolution,
          );

        switch (pkg) {
        | Ok(pkg) => return(Some(pkg))
        | Error(reason) =>
          let%lwt () =
            Logs_lwt.info(m =>
              m("skipping package %a: %s", Resolution.pp, resolution, reason)
            );
          return(None);
        };
      };

      resolutions |> List.map(~f=fetchPackage) |> RunAsync.List.joinAll;
    };

    let* () = {
      let f = (tasks, manifest) =>
        switch (manifest) {
        | Some(manifest) => [addPackage(manifest), ...tasks]
        | None => tasks
        };

      packages |> List.fold_left(~f, ~init=[]) |> RunAsync.List.waitAll;
    };

    return();
  };

  let* () = addDependencies(dependencies);

  let%lwt () = finish();

  /* TODO: return rewritten deps */
  return(({...solver, universe: universe^}, dependencies));
};

let printCudfDoc = doc => {
  let o = IO.output_string();
  Cudf_printer.pp_io_doc(o, doc);
  IO.close_out(o);
};

let parseCudfSolution = (~cudfUniverse, data) => {
  let i = IO.input_string(data);
  let p = Cudf_parser.from_IO_in_channel(i);
  let solution = Cudf_parser.load_solution(p, cudfUniverse);
  IO.close_in(i);
  solution;
};

let solveDependencies =
    (
      ~gitUsername,
      ~gitPassword,
      ~root,
      ~installed,
      ~strategy,
      ~dumpCudfInput,
      ~dumpCudfOutput,
      dependencies,
      solver,
    ) => {
  open RunAsync.Syntax;

  let runSolver = (filenameIn, filenameOut) => {
    let cmd =
      Cmd.(
        solver.sandbox.cfg.Config.esySolveCmd
        % ("--strategy=" ++ strategy)
        % ("--timeout=" ++ string_of_float(solver.sandbox.cfg.solveTimeout))
        % p(filenameIn)
        % p(filenameOut)
      );

    try%lwt({
      let env = ChildProcess.CustomEnv(EsyBash.currentEnvWithMingwInPath);
      ChildProcess.run(~env, cmd);
    }) {
    | Unix.Unix_error(err, _, _) =>
      let msg = Unix.error_message(err);
      RunAsync.error(msg);
    | _ => RunAsync.error("error running cudf solver")
    };
  };

  let dummyRoot = {
    InstallManifest.name: root.InstallManifest.name,
    version: Version.parseExn("0.0.0"),
    originalVersion: None,
    originalName: root.originalName,
    source:
      PackageSource.Link({
        path: DistPath.v("."),
        manifest: None,
        kind: LinkRegular,
      }),
    overrides: Overrides.empty,
    dependencies,
    devDependencies: Dependencies.NpmFormula([]),
    peerDependencies: NpmFormula.empty,
    optDependencies: StringSet.empty,
    resolutions: Resolutions.empty,
    kind: Esy,
    installConfig: InstallConfig.empty,
    extraSources: [],
  };

  let universe = Universe.add(~pkg=dummyRoot, solver.universe);
  let (cudfUniverse, cudfMapping) =
    Universe.toCudf(~installed, solver.solvespec, universe);
  let cudfRoot = Universe.CudfMapping.encodePkgExn(dummyRoot, cudfMapping);

  let request = {
    ...Cudf.default_request,
    install: [(cudfRoot.Cudf.package, Some((`Eq, cudfRoot.Cudf.version)))],
  };

  let preamble = {
    ...Cudf.default_preamble,
    property: [
      ("staleness", `Int(None)),
      ("original-version", `String(None)),
      ...Cudf.default_preamble.property,
    ],
  };

  /* The solution has CRLF on Windows, which breaks the parser */
  let normalizeSolutionData = s =>
    Str.global_replace(Str.regexp_string("\r\n"), "\n", String.trim(s))
    ++ "\n";
  let emptySolution = "\n";

  let solution = {
    let cudf = (
      Some(preamble),
      Cudf.(
        get_packages(cudfUniverse) |> List.sort(~cmp=(p1, p2) => p1 <% p2)
      ),
      request,
    );
    let cudfData = printCudfDoc(cudf);
    let* () =
      switch (dumpCudfInput) {
      | None => return()
      | Some(filename) => EsyLib.DumpToFile.dump(filename, cudfData)
      };

    Fs.withTempDir(path => {
      let* filenameIn = {
        let filename = Path.(path / "in.cudf");
        let* () = Fs.writeFile(~data=cudfData, filename);
        return(filename);
      };

      let filenameOut = Path.(path / "out.cudf");
      let (report, finish) =
        Cli.createProgressReporter(~name="solving esy constraints", ());
      let%lwt () = report("running solver");
      let* () = runSolver(filenameIn, filenameOut);
      let%lwt () = finish();
      let* result = {
        let* dataOut = Fs.readFile(filenameOut);
        let dataOut = normalizeSolutionData(dataOut);
        let* () =
          switch (dumpCudfOutput) {
          | None => return()
          | Some(filename) => EsyLib.DumpToFile.dump(filename, dataOut)
          };

        if (dataOut == emptySolution) {
          return(None);
        } else {
          let solution = parseCudfSolution(~cudfUniverse, dataOut);
          return(Some(solution));
        };
      };

      return(result);
    });
  };

  switch%bind (solution) {
  | Some((_preamble, cudfUniv)) =>
    let packages =
      cudfUniv
      |> Cudf.get_packages(~filter=p => p.Cudf.installed)
      |> List.map(~f=p => Universe.CudfMapping.decodePkgExn(p, cudfMapping))
      |> List.filter(~f=p =>
           p.InstallManifest.name != dummyRoot.InstallManifest.name
         )
      |> InstallManifest.Set.of_list;

    return(Ok(packages));

  | None =>
    let cudf = (preamble, cudfUniverse, request);
    switch%bind (
      Explanation.explain(
        ~gitUsername,
        ~gitPassword,
        cudfMapping,
        solver,
        cudf,
      )
    ) {
    | Some(reasons) => return(Error(reasons))
    | None => return(Error(Explanation.empty))
    };
  };
};

let solveDependenciesNaively =
    (
      ~gitUsername,
      ~gitPassword,
      ~installed: InstallManifest.Set.t,
      ~root: InstallManifest.t,
      dependencies: Dependencies.t,
      solver: t,
    ) => {
  open RunAsync.Syntax;

  let (report, finish) =
    Cli.createProgressReporter(~name="resolving npm packages", ());

  let installed = {
    let tbl = Hashtbl.create(100);
    InstallManifest.Set.iter(
      pkg => Hashtbl.add(tbl, pkg.name, pkg),
      installed,
    );
    tbl;
  };

  let addToInstalled = pkg =>
    Hashtbl.replace(installed, pkg.InstallManifest.name, pkg);

  let resolveOfInstalled = req => {
    let rec findFirstMatching =
      fun
      | [] => None
      | [pkg, ...pkgs] =>
        if (Req.name(req) == pkg.InstallManifest.name) {
          Some(pkg);
        } else {
          findFirstMatching(pkgs);
        };

    findFirstMatching(Hashtbl.find_all(installed, req.name));
  };

  let resolveOfOutside = req => {
    let%lwt () = report("%a", Req.pp, req);
    let* resolutions =
      Resolver.resolve(
        ~gitUsername,
        ~gitPassword,
        ~name=req.name,
        ~spec=req.spec,
        solver.sandbox.resolver,
      );
    switch (
      findResolutionForRequest(solver.sandbox.resolver, req, resolutions)
    ) {
    | Some(resolution) =>
      switch%bind (
        Resolver.package(
          ~gitUsername,
          ~gitPassword,
          ~resolution,
          solver.sandbox.resolver,
        )
      ) {
      | Ok(pkg) => return(Some(pkg))
      | Error(reason) =>
        errorf("invalid package %a: %s", Resolution.pp, resolution, reason)
      }
    | None => return(None)
    };
  };

  let resolve = (trace, req: Req.t) => {
    let* pkg =
      switch (resolveOfInstalled(req)) {
      | None =>
        switch%bind (resolveOfOutside(req)) {
        | None =>
          let explanation = [
            Reason.missing({constr: Dependencies.NpmFormula([req]), trace}),
          ];
          errorf("%a", Explanation.pp, explanation);
        | Some(pkg) => return(pkg)
        }
      | Some(pkg) => return(pkg)
      };

    return(pkg);
  };

  let (sealDependencies, addDependencies) = {
    let solved = Hashtbl.create(100);
    let key = pkg =>
      pkg.InstallManifest.name
      ++ "."
      ++ Version.show(pkg.InstallManifest.version);
    let sealDependencies = () => {
      let f = (_key, (pkg, dependencies), map) =>
        InstallManifest.Map.add(pkg, dependencies, map);

      Hashtbl.fold(f, solved, InstallManifest.Map.empty);
    };
    /* Hashtbl.find_opt solved (key pkg) */

    let register = (pkg, dependencies) =>
      Hashtbl.add(solved, key(pkg), (pkg, dependencies));

    (sealDependencies, register);
  };

  let solveDependencies = (trace, dependencies) => {
    let reqs =
      switch (dependencies) {
      | Dependencies.NpmFormula(reqs) => reqs
      | Dependencies.OpamFormula(_) =>
        /* only use already installed dependencies here
         * TODO: refactor solution * construction so we don't need to do that */
        let reqs = Dependencies.toApproximateRequests(dependencies);
        let reqs = {
          let f = req => Hashtbl.mem(installed, req.Req.name);
          List.filter(~f, reqs);
        };

        reqs;
      };

    let* pkgs = {
      let f = req => {
        let* manifest =
          RunAsync.contextf(
            resolve(trace, req),
            "resolving request %a",
            Req.pp,
            req,
          );

        addToInstalled(manifest);
        return(manifest);
      };

      reqs |> List.map(~f) |> RunAsync.List.joinAll;
    };

    let (_, solved) = {
      let f = ((seen, solved), manifest) =>
        if (InstallManifest.Set.mem(manifest, seen)) {
          (seen, solved);
        } else {
          let seen = InstallManifest.Set.add(manifest, seen);
          let solved = [manifest, ...solved];
          (seen, solved);
        };

      List.fold_left(~f, ~init=(InstallManifest.Set.empty, []), pkgs);
    };

    return(solved);
  };

  let rec loop = (trace, seen) =>
    fun
    | [pkg, ...rest] =>
      InstallManifest.Set.mem(pkg, seen)
        ? loop(trace, seen, rest)
        : {
          let seen = InstallManifest.Set.add(pkg, seen);
          let* dependencies = RunAsync.ofRun(evalDependencies(solver, pkg));
          let* dependencies =
            RunAsync.contextf(
              solveDependencies([pkg, ...trace], dependencies),
              "solving dependencies of %a",
              InstallManifest.pp,
              pkg,
            );

          addDependencies(pkg, dependencies);
          loop(trace, seen, rest @ dependencies);
        }
    | [] => return();

  let* () = {
    let* dependencies = solveDependencies([root], dependencies);
    let* () = loop([root], InstallManifest.Set.empty, dependencies);
    addDependencies(root, dependencies);
    return();
  };

  let%lwt () = finish();
  return(sealDependencies());
};

let solveOCamlReq = (~gitUsername, ~gitPassword, req: Req.t, resolver) => {
  open RunAsync.Syntax;

  let make = resolution => {
    let%lwt () = Logs_lwt.info(m => m("using %a", Resolution.pp, resolution));
    let* pkg =
      Resolver.package(~gitUsername, ~gitPassword, ~resolution, resolver);
    let* pkg = RunAsync.ofStringError(pkg);
    return((pkg.InstallManifest.originalVersion, Some(pkg.version)));
  };

  switch (req.spec) {
  | VersionSpec.Npm(_)
  | VersionSpec.NpmDistTag(_) =>
    let* resolutions =
      Resolver.resolve(
        ~gitUsername,
        ~gitPassword,
        ~name=req.name,
        ~spec=req.spec,
        resolver,
      );
    switch (findResolutionForRequest(resolver, req, resolutions)) {
    | Some(resolution) => make(resolution)
    | None =>
      let%lwt () =
        Logs_lwt.warn(m => m("no version found for %a", Req.pp, req));
      return((None, None));
    };
  | VersionSpec.Opam(_) =>
    error("ocaml version should be either an npm version or source")
  | VersionSpec.Source(_) =>
    switch%bind (
      Resolver.resolve(
        ~gitUsername,
        ~gitPassword,
        ~name=req.name,
        ~spec=req.spec,
        resolver,
      )
    ) {
    | [resolution] => make(resolution)
    | _ => errorf("multiple resolutions for %a, expected one", Req.pp, req)
    }
  };
};

let solve =
    (
      ~gitUsername,
      ~gitPassword,
      ~dumpCudfInput=None,
      ~dumpCudfOutput=None,
      solvespec,
      sandbox: Sandbox.t,
    ) => {
  open RunAsync.Syntax;

  let getResultOrExplain =
    fun
    | Ok(dependencies) => return(dependencies)
    | Error(explanation) => errorf("%a", Explanation.pp, explanation);

  let* solver = make(solvespec, sandbox);

  let* (dependencies, ocamlVersion) = {
    let* rootDependencies =
      RunAsync.ofRun(evalDependencies(solver, sandbox.root));

    let ocamlReq =
      switch (rootDependencies) {
      | InstallManifest.Dependencies.OpamFormula(_) => None
      | InstallManifest.Dependencies.NpmFormula(reqs) =>
        NpmFormula.find(~name="ocaml", reqs)
      };

    switch (ocamlReq) {
    | None => return((rootDependencies, None))
    | Some(ocamlReq) =>
      let* (ocamlVersionOrig, ocamlVersion) =
        RunAsync.contextf(
          solveOCamlReq(
            ~gitUsername,
            ~gitPassword,
            ocamlReq,
            sandbox.resolver,
          ),
          "resolving %a",
          Req.pp,
          ocamlReq,
        );

      let* dependencies =
        switch (ocamlVersion, rootDependencies) {
        | (
            Some(ocamlVersion),
            InstallManifest.Dependencies.NpmFormula(reqs),
          ) =>
          let ocamlSpec = VersionSpec.ofVersion(ocamlVersion);
          let ocamlReq = Req.make(~name="ocaml", ~spec=ocamlSpec);
          let reqs = NpmFormula.override(reqs, [ocamlReq]);
          return(InstallManifest.Dependencies.NpmFormula(reqs));
        | (
            Some(ocamlVersion),
            InstallManifest.Dependencies.OpamFormula(deps),
          ) =>
          let req =
            switch (ocamlVersion) {
            | Version.Npm(v) =>
              InstallManifest.Dep.Npm(SemverVersion.Constraint.EQ(v))
            | Version.Source(src) =>
              InstallManifest.Dep.Source(SourceSpec.ofSource(src))
            | Version.Opam(v) =>
              InstallManifest.Dep.Opam(OpamPackageVersion.Constraint.EQ(v))
            };

          let ocamlDep = {InstallManifest.Dep.name: "ocaml", req};
          return(
            InstallManifest.Dependencies.OpamFormula(deps @ [[ocamlDep]]),
          );
        | (None, deps) => return(deps)
        };

      return((dependencies, ocamlVersionOrig));
    };
  };

  let () =
    switch (ocamlVersion) {
    | Some(version) => Resolver.setOCamlVersion(version, sandbox.resolver)
    | None => ()
    };

  let* (solver, dependencies) = {
    let* (solver, dependencies) =
      add(~gitUsername, ~gitPassword, ~dependencies, solver);
    return((solver, dependencies));
  };

  /* Solve esy dependencies first. */
  let* installed = {
    let* res =
      solveDependencies(
        ~gitUsername,
        ~gitPassword,
        ~root=sandbox.root,
        ~installed=InstallManifest.Set.empty,
        ~strategy=Strategy.trendy,
        ~dumpCudfInput,
        ~dumpCudfOutput,
        dependencies,
        solver,
      );

    getResultOrExplain(res);
  };

  /* Solve npm dependencies now. */
  let* dependenciesMap =
    solveDependenciesNaively(
      ~gitUsername,
      ~gitPassword,
      ~installed,
      ~root=sandbox.root,
      dependencies,
      solver,
    );

  let* (packageById, idByPackage, dependenciesById) = {
    let* (packageById, idByPackage) = {
      let rec aux = ((packageById, idByPackage) as acc) =>
        fun
        | [pkg, ...rest] => {
            let* (id, pkg) = lock(sandbox, pkg);
            switch (PackageId.Map.find_opt(id, packageById)) {
            | Some(_) => aux(acc, rest)
            | None =>
              let deps =
                switch (InstallManifest.Map.find_opt(pkg, dependenciesMap)) {
                | Some(deps) => deps
                | None =>
                  Exn.failf(
                    "no dependencies solved found for %a",
                    InstallManifest.pp,
                    pkg,
                  )
                };

              let acc = {
                let packageById = PackageId.Map.add(id, pkg, packageById);
                let idByPackage =
                  InstallManifest.Map.add(pkg, id, idByPackage);
                (packageById, idByPackage);
              };

              aux(acc, rest @ deps);
            };
          }
        | [] => return((packageById, idByPackage));

      let packageById = PackageId.Map.empty;
      let idByPackage = InstallManifest.Map.empty;

      aux((packageById, idByPackage), [sandbox.root]);
    };

    let dependencies = {
      let f = (pkg, id, map) => {
        let dependencies =
          switch (InstallManifest.Map.find_opt(pkg, dependenciesMap)) {
          | Some(deps) => deps
          | None =>
            Exn.failf(
              "no dependencies solved found for %a",
              InstallManifest.pp,
              pkg,
            )
          };

        let dependencies = {
          let f = (deps, pkg) => {
            let id = InstallManifest.Map.find(pkg, idByPackage);
            StringMap.add(pkg.InstallManifest.name, id, deps);
          };

          List.fold_left(~f, ~init=StringMap.empty, dependencies);
        };

        PackageId.Map.add(id, dependencies, map);
      };

      InstallManifest.Map.fold(f, idByPackage, PackageId.Map.empty);
    };

    return((packageById, idByPackage, dependencies));
  };

  let* solution = {
    let allDependenciesByName = {
      let f = (_id, deps, map) => {
        let f = (_key, a, b) =>
          switch (a, b) {
          | (None, None) => None
          | (Some(vs), None) => Some(vs)
          | (None, Some(id)) =>
            let version = PackageId.version(id);
            Some(Version.Map.add(version, id, Version.Map.empty));
          | (Some(vs), Some(id)) =>
            let version = PackageId.version(id);
            Some(Version.Map.add(version, id, vs));
          };

        StringMap.merge(f, map, deps);
      };

      PackageId.Map.fold(f, dependenciesById, StringMap.empty);
    };

    let* solution = {
      let id = InstallManifest.Map.find(sandbox.root, idByPackage);
      let dependenciesByName = PackageId.Map.find(id, dependenciesById);

      let* root =
        lockPackage(
          sandbox.resolver,
          id,
          sandbox.root,
          dependenciesByName,
          allDependenciesByName,
        );

      return(
        {
          let solution = EsyInstall.Solution.empty(root.EsyInstall.Package.id);
          EsyInstall.Solution.add(solution, root);
        },
      );
    };

    let* solution = {
      let f = (solution, (id, dependencies)) => {
        let pkg = PackageId.Map.find(id, packageById);
        let* pkg =
          lockPackage(
            sandbox.resolver,
            id,
            pkg,
            dependencies,
            allDependenciesByName,
          );

        return(EsyInstall.Solution.add(solution, pkg));
      };

      dependenciesById
      |> PackageId.Map.bindings
      |> RunAsync.List.foldLeft(~f, ~init=solution);
    };

    return(solution);
  };

  return(solution);
};
