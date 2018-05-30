open Opam;
open Npm;
open Shared;
module Path = EsyLib.Path;
module Cmd = EsyLib.Cmd;

let satisfies = (realVersion, req) =>
  switch (req, realVersion) {
  | (Types.Github(user, repo, ref), Lockfile.Github(user_, repo_, ref_))
      when user == user_ && repo == repo_ && ref == ref_ =>
    true
  | (Npm(semver), Lockfile.Npm(s)) when NpmVersion.matches(semver, s) =>
    true
  | (Opam(semver), Lockfile.Opam(s)) when OpamVersion.matches(semver, s) =>
    true
  | (LocalPath(p1), Lockfile.LocalPath(p2)) when Path.equal(p1, p2) => true
  | _ => false
  };

let toRealVersion = versionPlus =>
  switch (versionPlus) {
  | `Github(user, repo, ref) => Lockfile.Github(user, repo, ref)
  | `Npm(x, _, _) => Lockfile.Npm(x)
  | `Opam(x, _, _) => Lockfile.Opam(x)
  | `LocalPath(p) => Lockfile.LocalPath(p)
  };

/** TODO(jared): This is a HACK and will hopefully be removed once we stop the
 * pseudo-npm opam version stuff */
let tryConvertingOpamFromNpm = version =>
  Shared.Types.(
    version
    |> Shared.GenericVersion.map(opam =>
         switch (opam) {
         /* yay jbuilder */
         | Alpha(
             "",
             Some(
               Num(
                 major,
                 Some(
                   Alpha(
                     ".",
                     Some(
                       Num(
                         minor,
                         Some(
                           Alpha(
                             ".",
                             Some(Num(0, Some(Alpha("-beta", rest)))),
                           ),
                         ),
                       ),
                     ),
                   ),
                 ),
               ),
             ),
           ) =>
           Alpha(
             "",
             Some(
               Num(
                 major,
                 Some(
                   Alpha(
                     ".",
                     Some(Num(minor, Some(Alpha("+beta", rest)))),
                   ),
                 ),
               ),
             ),
           )
         | Alpha(
             "",
             Some(
               Num(
                 major,
                 Some(
                   Alpha(
                     ".",
                     Some(
                       Num(minor, Some(Alpha(".", Some(Num(0, post))))),
                     ),
                   ),
                 ),
               ),
             ),
           ) =>
           Alpha(
             "",
             Some(Num(major, Some(Alpha(".", Some(Num(minor, post)))))),
           )
         | _ => opam
         }
       )
  );

let expectSuccess = (msg, v) =>
  if (v) {
    ();
  } else {
    failwith(msg);
  };

let ensureGitRepo = (source, dest) =>
  if (! Shared.Files.exists(Path.toString(dest))) {
    Shared.Files.mkdirp(Filename.dirname(Path.toString(dest)));
    let cmd = Cmd.(v("git") % "clone" % "--depth" % "1" % source % p(dest));
    Shared.ExecCommand.execSync(~cmd, ())
    |> snd
    |> expectSuccess("Unable to clone " ++ source);
  } else {
    let cmd = Cmd.(v("git") % "pull" % "--depth" % "1");
    Shared.ExecCommand.execSync(~workingDir=dest, ~cmd, ())
    |> snd
    |> expectSuccess("Unable tp update: " ++ Path.toString(dest));
  };

let lockDownRef = (url, ref) => {
  let cmd = Cmd.(v("git") % "ls-remote" % url % ref);
  let (output, success) = Shared.ExecCommand.execSync(~cmd, ());
  if (success) {
    switch (output) {
    | [] => ref
    | [line, ..._] =>
      let ref = String.split_on_char('\t', line) |> List.hd;
      ref;
    };
  } else {
    print_endline("Failed to execute git ls-remote " ++ Cmd.toString(cmd));
    ref;
  };
};

let rec lockDownSource = pendingSource =>
  switch (pendingSource) {
  | Types.PendingSource.NoSource => (Types.Source.NoSource, None)
  | WithOpamFile(source, opamFile) =>
    switch (lockDownSource(source)) {
    | (s, None) => (s, Some(opamFile))
    | _ => failwith("can't nest withOpamFiles inside each other")
    }
  | Archive(url, None) => (
      /* print_endline("Pretending to get a checksum for " ++ url); */
      Types.Source.Archive(url, "fake checksum"),
      None,
    )
  | Archive(url, Some(checksum)) => (
      Types.Source.Archive(url, checksum),
      None,
    )
  | GitSource(url, ref) =>
    let ref = Shared.Infix.(ref |? "master");
    /** TODO getting HEAD */
    (Types.Source.GitSource(url, lockDownRef(url, ref)), None);
  | GithubSource(user, name, ref) =>
    let ref = Shared.Infix.(ref |? "master");
    (
      Types.Source.GithubSource(
        user,
        name,
        lockDownRef(
          "git://github.com/" ++ user ++ "/" ++ name ++ ".git",
          ref,
        ),
      ),
      None,
    );
  | File(s) => (Types.Source.File(s), None)
  };

/* let lockDownWithOpam = (pending, opam) => switch opam {
   | Some(s) => lockDownSource(Types.PendingSource.WithOpamFile(pending, s))
   | _ => lockDownSource(pending)
   }; */
let checkRepositories = config => {
  Logs.debug(m => m("git: updating esy-ocaml/esy-opam-override"));
  ensureGitRepo(
    "https://github.com/esy-ocaml/esy-opam-override",
    config.Config.esyOpamOverridePath,
  );
  Logs.debug(m => m("git: updating ocaml/opam-repository"));
  ensureGitRepo(
    "https://github.com/ocaml/opam-repository",
    config.Config.opamRepositoryPath,
  );
};

let getCachedManifest = (opamOverrides, cache, (name, versionPlus)) => {
  let realVersion = toRealVersion(versionPlus);
  switch (Hashtbl.find(cache, (name, realVersion))) {
  | exception Not_found =>
    let manifest =
      switch (versionPlus) {
      | `Github(user, repo, ref) => Github.getManifest(name, user, repo, ref)
      /* Registry.getGithubManifest(url) */
      | `Npm(_version, json, _) => `PackageJson(json)
      | `LocalPath(_p) =>
        failwith("do not know how to get manifest from LocalPath")
      | `Opam(_version, path, _) =>
        `OpamFile(OpamFile.getManifest(opamOverrides, path))
      };
    let depsByKind = Manifest.getDeps(manifest);
    let res = (manifest, depsByKind);
    Hashtbl.replace(cache, (name, realVersion), res);
    res;
  | x => x
  };
};

let runSolver = (~strategy="-notuptodate", rootName, deps, universe) => {
  let root = {
    ...Cudf.default_package,
    package: rootName,
    version: 1,
    depends: deps,
  };
  Cudf.add_package(universe, root);
  let request = {
    ...Cudf.default_request,
    install: [(root.Cudf.package, Some((`Eq, root.Cudf.version)))],
  };
  let preamble = Cudf.default_preamble;
  let solution =
    Mccs.resolve_cudf(
      ~verbose=false,
      ~timeout=5.,
      strategy,
      (preamble, universe, request),
    );
  switch (solution) {
  | None => None
  | Some((_preamble, universe)) =>
    let packages = Cudf.get_packages(~filter=p => p.Cudf.installed, universe);
    Some(packages);
  };
};

let getOpamFile = (manifest, name, version) =>
  switch (manifest) {
  | `PackageJson(_) => None
  | `OpamFile(manifest) =>
    Some(OpamFile.toPackageJson(manifest, name, version))
  };
