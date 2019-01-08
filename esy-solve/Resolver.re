open EsyPackageConfig;

module PackageCache =
  Memoize.Make({
    type key = (string, Resolution.resolution);
    type value = RunAsync.t(result(InstallManifest.t, string));
  });

module SourceCache =
  Memoize.Make({
    type key = SourceSpec.t;
    type value = RunAsync.t(Source.t);
  });

module ResolutionCache =
  Memoize.Make({
    type key = string;
    type value = RunAsync.t(list(Resolution.t));
  });

let requireOpamName = name =>
  Run.Syntax.(
    switch (Astring.String.cut(~sep="@opam/", name)) {
    | Some(("", name)) => return(OpamPackage.Name.of_string(name))
    | _ => errorf("invalid opam package name: %s", name)
    }
  );

let ensureOpamName = name =>
  Run.Syntax.(
    switch (Astring.String.cut(~sep="@opam/", name)) {
    | Some(("", name)) => return(OpamPackage.Name.of_string(name))
    | Some(_)
    | None => return(OpamPackage.Name.of_string(name))
    }
  );

let toOpamOcamlVersion = version =>
  switch (version) {
  | Some(Version.Npm({major, minor, patch, _})) =>
    let minor =
      if (minor < 10) {
        "0" ++ string_of_int(minor);
      } else {
        string_of_int(minor);
      };

    let patch =
      if (patch < 1000) {
        patch;
      } else {
        patch / 1000;
      };

    let v = Printf.sprintf("%i.%s.%i", major, minor, patch);
    let v =
      switch (OpamPackageVersion.Version.parse(v)) {
      | Ok(v) => v
      | Error(msg) => failwith(msg)
      };

    Some(v);
  | Some(Version.Opam(v)) => Some(v)
  | Some(Version.Source(_)) => None
  | None => None
  };

type t = {
  cfg: Config.t,
  sandbox: EsyInstall.SandboxSpec.t,
  pkgCache: PackageCache.t,
  srcCache: SourceCache.t,
  opamRegistry: OpamRegistry.t,
  npmRegistry: NpmRegistry.t,
  mutable ocamlVersion: option(Version.t),
  mutable resolutions: Resolutions.t,
  resolutionCache: ResolutionCache.t,
  resolutionUsage: Hashtbl.t(Resolution.t, bool),
  npmDistTags: Hashtbl.t(string, StringMap.t(SemverVersion.Version.t)),
  sourceSpecToSource: Hashtbl.t(SourceSpec.t, Source.t),
  sourceToSource: Hashtbl.t(Source.t, Source.t),
};

let emptyLink = (~name, ~path, ~manifest, ~kind, ()) => {
  InstallManifest.name,
  version: Version.Source(Source.Link({path, manifest, kind})),
  originalVersion: None,
  originalName: None,
  source: PackageSource.Link({path, manifest: None, kind}),
  overrides: Overrides.empty,
  dependencies: InstallManifest.Dependencies.NpmFormula([]),
  devDependencies: InstallManifest.Dependencies.NpmFormula([]),
  peerDependencies: NpmFormula.empty,
  optDependencies: StringSet.empty,
  resolutions: Resolutions.empty,
  kind: Esy,
};

let emptyInstall = (~name, ~source, ()) => {
  InstallManifest.name,
  version: Version.Source(Dist(source)),
  originalVersion: None,
  originalName: None,
  source: PackageSource.Install({source: (source, []), opam: None}),
  overrides: Overrides.empty,
  dependencies: InstallManifest.Dependencies.NpmFormula([]),
  devDependencies: InstallManifest.Dependencies.NpmFormula([]),
  peerDependencies: NpmFormula.empty,
  optDependencies: StringSet.empty,
  resolutions: Resolutions.empty,
  kind: Esy,
};

let make = (~cfg, ~sandbox, ()) =>
  RunAsync.return({
    cfg,
    sandbox,
    pkgCache: PackageCache.make(),
    srcCache: SourceCache.make(),
    opamRegistry: OpamRegistry.make(~cfg, ()),
    npmRegistry: NpmRegistry.make(~url=cfg.Config.npmRegistry, ()),
    ocamlVersion: None,
    resolutions: Resolutions.empty,
    resolutionCache: ResolutionCache.make(),
    resolutionUsage: Hashtbl.create(10),
    npmDistTags: Hashtbl.create(500),
    sourceSpecToSource: Hashtbl.create(500),
    sourceToSource: Hashtbl.create(500),
  });

let setOCamlVersion = (ocamlVersion, resolver) =>
  resolver.ocamlVersion = Some(ocamlVersion);

let setResolutions = (resolutions, resolver) =>
  resolver.resolutions = resolutions;

let getUnusedResolutions = resolver => {
  let nameIfUnused = (usage, resolution: Resolution.t) =>
    switch (Hashtbl.find_opt(usage, resolution)) {
    | Some(true) => None
    | _ => Some(resolution.name)
    };

  List.filter_map(
    ~f=nameIfUnused(resolver.resolutionUsage),
    Resolutions.entries(resolver.resolutions),
  );
};

/* This function increments the resolution usage count of that resolution */
let markResolutionAsUsed = (resolver, resolution) =>
  Hashtbl.replace(resolver.resolutionUsage, resolution, true);

let sourceMatchesSpec = (resolver, spec, source) =>
  switch (Hashtbl.find_opt(resolver.sourceSpecToSource, spec)) {
  | Some(resolvedSource) =>
    if (Source.compare(resolvedSource, source) == 0) {
      true;
    } else {
      switch (Hashtbl.find_opt(resolver.sourceToSource, resolvedSource)) {
      | Some(resolvedSource) => Source.compare(resolvedSource, source) == 0
      | None => false
      };
    }
  | None => false
  };

let versionMatchesReq = (resolver: t, req: Req.t, name, version: Version.t) => {
  let checkVersion = () =>
    switch (req.spec, version) {
    | (VersionSpec.Npm(spec), Version.Npm(version)) =>
      SemverVersion.Formula.DNF.matches(~version, spec)

    | (VersionSpec.NpmDistTag(tag), Version.Npm(version)) =>
      switch (Hashtbl.find_opt(resolver.npmDistTags, req.name)) {
      | Some(tags) =>
        switch (StringMap.find_opt(tag, tags)) {
        | None => false
        | Some(taggedVersion) =>
          SemverVersion.Version.compare(version, taggedVersion) == 0
        }
      | None => false
      }

    | (VersionSpec.Opam(spec), Version.Opam(version)) =>
      OpamPackageVersion.Formula.DNF.matches(~version, spec)

    | (VersionSpec.Source(spec), Version.Source(source)) =>
      sourceMatchesSpec(resolver, spec, source)

    | (VersionSpec.Npm(_), _) => false
    | (VersionSpec.NpmDistTag(_), _) => false
    | (VersionSpec.Opam(_), _) => false
    | (VersionSpec.Source(_), _) => false
    };

  let checkResolutions = () =>
    switch (Resolutions.find(resolver.resolutions, req.name)) {
    | Some(_) => true
    | None => false
    };

  req.name == name && (checkResolutions() || checkVersion());
};

let versionMatchesDep =
    (resolver: t, dep: InstallManifest.Dep.t, name, version: Version.t) => {
  let checkVersion = () =>
    switch (version, dep.InstallManifest.Dep.req) {
    | (Version.Npm(version), Npm(spec)) =>
      SemverVersion.Constraint.matches(~version, spec)

    | (Version.Opam(version), Opam(spec)) =>
      OpamPackageVersion.Constraint.matches(~version, spec)

    | (Version.Source(source), Source(spec)) =>
      sourceMatchesSpec(resolver, spec, source)

    | (Version.Npm(_), _) => false
    | (Version.Opam(_), _) => false
    | (Version.Source(_), _) => false
    };

  let checkResolutions = () =>
    switch (Resolutions.find(resolver.resolutions, dep.name)) {
    | Some(_) => true
    | None => false
    };

  dep.name == name && (checkResolutions() || checkVersion());
};

let packageOfSource = (~name, ~overrides, source: Source.t, resolver) => {
  open RunAsync.Syntax;

  let readManifest =
      (
        ~name,
        ~source,
        {
          EsyInstall.DistResolver.kind,
          filename: _,
          data,
          suggestedPackageName,
        },
      ) =>
    RunAsync.Syntax.(
      switch (kind) {
      | ManifestSpec.Esy =>
        let%bind (manifest, _warnings) =
          RunAsync.ofRun(
            {
              open Run.Syntax;
              let%bind json = Json.parse(data);
              OfPackageJson.installManifest(
                ~parseResolutions=true,
                ~parseDevDependencies=true,
                ~name,
                ~version=Version.Source(source),
                ~source,
                json,
              );
            },
          );
        return(Ok(manifest));
      | ManifestSpec.Opam =>
        let%bind opamname =
          RunAsync.ofRun(ensureOpamName(suggestedPackageName));
        let%bind manifest =
          RunAsync.ofRun(
            {
              let version = OpamPackage.Version.of_string("dev");
              OpamManifest.ofString(~name=opamname, ~version, data);
            },
          );
        OpamManifest.toInstallManifest(
          ~name,
          ~version=Version.Source(source),
          ~source,
          manifest,
        );
      }
    );

  let pkg = {
    let%bind {
      EsyInstall.DistResolver.overrides,
      dist: resolvedDist,
      manifest,
      _,
    } =
      EsyInstall.DistResolver.resolve(
        ~cfg=resolver.cfg.installCfg,
        ~sandbox=resolver.sandbox,
        ~overrides,
        Source.toDist(source),
      );

    let%bind resolvedSource =
      switch (source, resolvedDist) {
      | (Source.Dist(_), _) => return(Source.Dist(resolvedDist))
      | (Source.Link({kind, _}), Dist.LocalPath({path, manifest})) =>
        return(Source.Link({path, manifest, kind}))
      | (Source.Link(_), dist) =>
        errorf("unable to link to %a", Dist.pp, dist)
      };

    let%bind pkg =
      switch (manifest) {
      | Some(manifest) =>
        readManifest(~name, ~source=resolvedSource, manifest)
      | None =>
        if (!Overrides.isEmpty(overrides)) {
          switch (source) {
          | Source.Link({path, manifest, kind}) =>
            let pkg = emptyLink(~name, ~path, ~manifest, ~kind, ());
            return(Ok(pkg));
          | _ =>
            let pkg = emptyInstall(~name, ~source=resolvedDist, ());
            return(Ok(pkg));
          };
        } else {
          errorf("no manifest found at %a", Source.pp, source);
        }
      };

    let pkg =
      switch (pkg) {
      | Ok(pkg) => Ok({...pkg, InstallManifest.overrides})
      | err => err
      };

    Hashtbl.replace(resolver.sourceToSource, source, resolvedSource);

    return(pkg);
  };

  RunAsync.contextf(
    pkg,
    "reading package metadata from %a",
    Source.ppPretty,
    source,
  );
};

let applyOverride = (pkg, override: Override.install) => {
  let {Override.dependencies, devDependencies, resolutions} = override;

  let applyNpmFormulaOverride = (dependencies, override) =>
    switch (dependencies) {
    | InstallManifest.Dependencies.NpmFormula(formula) =>
      let formula = {
        let dependencies = {
          let f = (map, req) => StringMap.add(req.Req.name, req, map);
          List.fold_left(~f, ~init=StringMap.empty, formula);
        };

        let dependencies = StringMap.Override.apply(dependencies, override);

        StringMap.values(dependencies);
      };

      InstallManifest.Dependencies.NpmFormula(formula);
    | InstallManifest.Dependencies.OpamFormula(formula) =>
      /* remove all terms which we override */
      let formula = {
        let filter = dep =>
          switch (StringMap.find_opt(dep.InstallManifest.Dep.name, override)) {
          | None => true
          | Some(_) => false
          };

        formula
        |> List.map(~f=List.filter(~f=filter))
        |> List.filter(
             ~f=
               fun
               | [] => false
               | _ => true,
           );
      };

      /* now add all edits */
      let formula = {
        let edits = {
          let f = (_name, override, edits) =>
            switch (override) {
            | StringMap.Override.Drop => edits
            | StringMap.Override.Edit(req) =>
              switch (req.Req.spec) {
              | VersionSpec.Npm(formula) =>
                let f = (c: SemverVersion.Constraint.t) => {
                  InstallManifest.Dep.name: req.name,
                  req: Npm(c),
                };

                let formula = SemverVersion.Formula.ofDnfToCnf(formula);
                List.map(~f=List.map(~f), formula) @ edits;
              | VersionSpec.Opam(formula) =>
                let f = (c: OpamPackageVersion.Constraint.t) => {
                  InstallManifest.Dep.name: req.name,
                  req: Opam(c),
                };

                List.map(~f=List.map(~f), formula) @ edits;
              | VersionSpec.NpmDistTag(_) =>
                failwith("cannot override opam with npm dist tag")
              | VersionSpec.Source(spec) => [
                  [{InstallManifest.Dep.name: req.name, req: Source(spec)}],
                  ...edits,
                ]
              }
            };

          StringMap.fold(f, override, []);
        };

        formula @ edits;
      };

      InstallManifest.Dependencies.OpamFormula(formula);
    };

  let pkg =
    switch (dependencies) {
    | Some(override) => {
        ...pkg,
        InstallManifest.dependencies:
          applyNpmFormulaOverride(pkg.InstallManifest.dependencies, override),
      }
    | None => pkg
    };

  let pkg =
    switch (devDependencies) {
    | Some(override) => {
        ...pkg,
        InstallManifest.devDependencies:
          applyNpmFormulaOverride(
            pkg.InstallManifest.devDependencies,
            override,
          ),
      }
    | None => pkg
    };

  let pkg =
    switch (resolutions) {
    | Some(resolutions) =>
      let resolutions = {
        let f = Resolutions.add;
        StringMap.fold(f, resolutions, Resolutions.empty);
      };

      {...pkg, InstallManifest.resolutions};
    | None => pkg
    };

  pkg;
};

let package = (~resolution: Resolution.t, resolver) => {
  open RunAsync.Syntax;
  let key = (resolution.name, resolution.resolution);

  let ofVersion = (version: Version.t) =>
    switch (version) {
    | Version.Npm(version) =>
      let%bind pkg =
        NpmRegistry.package(
          ~name=resolution.name,
          ~version,
          resolver.npmRegistry,
          (),
        );

      return(Ok(pkg));
    | Version.Opam(version) =>
      switch%bind (
        {
          let%bind name = RunAsync.ofRun(requireOpamName(resolution.name));
          OpamRegistry.version(~name, ~version, resolver.opamRegistry);
        }
      ) {
      | Some(manifest) =>
        OpamManifest.toInstallManifest(
          ~name=resolution.name,
          ~version=Version.Opam(version),
          manifest,
        )
      | None => errorf("no such opam package: %a", Resolution.pp, resolution)
      }

    | Version.Source(source) =>
      packageOfSource(
        ~overrides=Overrides.empty,
        ~name=resolution.name,
        source,
        resolver,
      )
    };

  PackageCache.compute(
    resolver.pkgCache,
    key,
    _ => {
      let%bind pkg =
        switch (resolution.resolution) {
        | Version(version) => ofVersion(version)
        | SourceOverride({source, override}) =>
          let override = Override.ofJson(override);
          let overrides = Overrides.(add(override, empty));
          packageOfSource(
            ~name=resolution.name,
            ~overrides,
            source,
            resolver,
          );
        };

      switch (pkg) {
      | Ok(pkg) =>
        let%bind pkg =
          Overrides.foldWithInstallOverrides(
            ~f=applyOverride,
            ~init=pkg,
            pkg.overrides,
          );

        return(Ok(pkg));
      | err => return(err)
      };
    },
  );
};

let resolveSource = (~name, ~sourceSpec: SourceSpec.t, resolver: t) => {
  open RunAsync.Syntax;

  let errorResolvingSource = msg =>
    errorf(
      "unable to resolve %s@%a: %s",
      name,
      SourceSpec.pp,
      sourceSpec,
      msg,
    );

  SourceCache.compute(
    resolver.srcCache,
    sourceSpec,
    _ => {
      let%lwt () =
        Logs_lwt.debug(m =>
          m("resolving %s@%a", name, SourceSpec.pp, sourceSpec)
        );
      let%bind source =
        switch (sourceSpec) {
        | SourceSpec.Github({user, repo, ref, manifest}) =>
          let remote =
            Printf.sprintf("https://github.com/%s/%s.git", user, repo);
          let%bind commit = Git.lsRemote(~ref?, ~remote, ());
          switch (commit, ref) {
          | (Some(commit), _) =>
            return(Source.Dist(Github({user, repo, commit, manifest})))
          | (None, Some(ref)) =>
            if (Git.isCommitLike(ref)) {
              return(
                Source.Dist(Github({user, repo, commit: ref, manifest})),
              );
            } else {
              errorResolvingSource("cannot resolve commit");
            }
          | (None, None) => errorResolvingSource("cannot resolve commit")
          };

        | SourceSpec.Git({remote, ref, manifest}) =>
          let%bind commit = Git.lsRemote(~ref?, ~remote, ());
          switch (commit, ref) {
          | (Some(commit), _) =>
            return(Source.Dist(Git({remote, commit, manifest})))
          | (None, Some(ref)) =>
            if (Git.isCommitLike(ref)) {
              return(Source.Dist(Git({remote, commit: ref, manifest})));
            } else {
              errorResolvingSource("cannot resolve commit");
            }
          | (None, None) => errorResolvingSource("cannot resolve commit")
          };

        | SourceSpec.NoSource => return(Source.Dist(NoSource))

        | SourceSpec.Archive({url, checksum: None}) =>
          failwith(
            "archive sources without checksums are not implemented: " ++ url,
          )
        | SourceSpec.Archive({url, checksum: Some(checksum)}) =>
          return(Source.Dist(Archive({url, checksum})))

        | SourceSpec.LocalPath({path, manifest}) =>
          let abspath = DistPath.toPath(resolver.sandbox.path, path);
          if%bind (Fs.exists(abspath)) {
            return(Source.Dist(LocalPath({path, manifest})));
          } else {
            errorf("path '%a' does not exist", Path.ppPretty, abspath);
          };
        };

      Hashtbl.replace(resolver.sourceSpecToSource, sourceSpec, source);
      return(source);
    },
  );
};

let resolve' = (~fullMetadata, ~name, ~spec, resolver) =>
  RunAsync.Syntax.(
    switch (spec) {
    | VersionSpec.Npm(_)
    | VersionSpec.NpmDistTag(_) =>
      let%bind resolutions =
        switch%bind (
          NpmRegistry.versions(~fullMetadata, ~name, resolver.npmRegistry, ())
        ) {
        | Some({NpmRegistry.versions, distTags}) =>
          Hashtbl.replace(resolver.npmDistTags, name, distTags);

          let resolutions = {
            let f = version => {
              let version = Version.Npm(version);
              {Resolution.name, resolution: Version(version)};
            };

            List.map(~f, versions);
          };

          return(resolutions);

        | None => return([])
        };

      let resolutions = {
        let tryCheckConformsToSpec = resolution =>
          switch (resolution.Resolution.resolution) {
          | Version(version) =>
            versionMatchesReq(
              resolver,
              Req.make(~name, ~spec),
              resolution.name,
              version,
            )
          | SourceOverride(_) => true
          }; /* do not filter them out yet */

        resolutions
        |> List.sort(~cmp=(a, b) => Resolution.compare(b, a))
        |> List.filter(~f=tryCheckConformsToSpec);
      };

      return(resolutions);

    | VersionSpec.Opam(_) =>
      let%bind resolutions =
        ResolutionCache.compute(
          resolver.resolutionCache,
          name,
          () => {
            let%lwt () =
              Logs_lwt.debug(m =>
                m("resolving %s %a", name, VersionSpec.pp, spec)
              );
            let%bind versions = {
              let%bind name = RunAsync.ofRun(requireOpamName(name));
              OpamRegistry.versions(
                ~ocamlVersion=?toOpamOcamlVersion(resolver.ocamlVersion),
                ~name,
                resolver.opamRegistry,
              );
            };

            let f = (resolution: OpamResolution.t) => {
              let version = OpamResolution.version(resolution);
              {Resolution.name, resolution: Version(version)};
            };

            return(List.map(~f, versions));
          },
        );

      let resolutions = {
        let tryCheckConformsToSpec = resolution =>
          switch (resolution.Resolution.resolution) {
          | Version(version) =>
            versionMatchesReq(
              resolver,
              Req.make(~name, ~spec),
              resolution.name,
              version,
            )
          | SourceOverride(_) => true
          }; /* do not filter them out yet */

        resolutions
        |> List.sort(~cmp=(a, b) => Resolution.compare(b, a))
        |> List.filter(~f=tryCheckConformsToSpec);
      };

      return(resolutions);

    | VersionSpec.Source(sourceSpec) =>
      let%bind source = resolveSource(~name, ~sourceSpec, resolver);
      let version = Version.Source(source);
      let resolution = {
        Resolution.name,
        resolution: Resolution.Version(version),
      };
      return([resolution]);
    }
  );

let resolve =
    (
      ~fullMetadata=false,
      ~name: string,
      ~spec: option(VersionSpec.t)=?,
      resolver: t,
    ) =>
  RunAsync.Syntax.(
    switch (Resolutions.find(resolver.resolutions, name)) {
    | Some(resolution) =>
      /* increment usage counter for that resolution so that we know it was used */
      markResolutionAsUsed(resolver, resolution);
      return([resolution]);
    | None =>
      let spec =
        switch (spec) {
        | None =>
          if (InstallManifest.isOpamPackageName(name)) {
            VersionSpec.Opam([[OpamPackageVersion.Constraint.ANY]]);
          } else {
            VersionSpec.Npm([[SemverVersion.Constraint.ANY]]);
          }
        | Some(spec) => spec
        };

      resolve'(~fullMetadata, ~name, ~spec, resolver);
    }
  );
