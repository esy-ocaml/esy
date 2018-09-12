module MS = SandboxSpec.ManifestSpec

include Types.SourceSpec

let toStringOrig = function
  | Github {user; repo; ref = None; manifest = None;} ->
    Printf.sprintf "github:%s/%s" user repo
  | Github {user; repo; ref = None; manifest = Some manifest;} ->
    Printf.sprintf "github:%s/%s:%s" user repo (MS.toString manifest)
  | Github {user; repo; ref = Some ref; manifest = None} ->
    Printf.sprintf "github:%s/%s#%s" user repo ref
  | Github {user; repo; ref = Some ref; manifest = Some manifest} ->
    Printf.sprintf "github:%s/%s:%s#%s" user repo (MS.toString manifest) ref

  | Git {remote; ref = None; manifest = None;} ->
    Printf.sprintf "git:%s" remote
  | Git {remote; ref = None; manifest = Some manifest;} ->
    Printf.sprintf "git:%s:%s" remote (MS.toString manifest)
  | Git {remote; ref = Some ref; manifest = None} ->
    Printf.sprintf "git:%s#%s" remote ref
  | Git {remote; ref = Some ref; manifest = Some manifest} ->
    Printf.sprintf "git:%s:%s#%s" remote (MS.toString manifest) ref

  | Archive {url; checksum = Some checksum} ->
    Printf.sprintf "archive:%s#%s" url (Checksum.show checksum)
  | Archive {url; checksum = None} ->
    Printf.sprintf "archive:%s" url

  | LocalPath {path; manifest = None;} ->
    Printf.sprintf "path:%s" (Path.toString path)
  | LocalPath {path; manifest = Some manifest;} ->
    Printf.sprintf "path:%s/%s" (Path.toString path) (MS.toString manifest)

  | LocalPathLink {path; manifest = None;} ->
    Printf.sprintf "link:%s" (Path.toString path)
  | LocalPathLink {path; manifest = Some manifest;} ->
    Printf.sprintf "link:%s/%s" (Path.toString path) (MS.toString manifest)

  | NoSource -> "no-source:"

let toString = function
  | Orig sourceSpec -> toStringOrig sourceSpec
  | Override {sourceSpec; _} -> "override:" ^ toStringOrig sourceSpec

let to_yojson src = `String (toString src)

let pp fmt spec =
  Fmt.pf fmt "%s" (toString spec)

let matches ~(source : Source.t) (spec : t) =
  let eqManifestName = [%derive.eq: MS.t option] in
  let matchesOrig source spec =
    match spec, source with
    | LocalPath {path = p1; manifest = m1},
      Source.LocalPath {path = p2; manifest = m2} ->
      Path.equal p1 p2 && eqManifestName m1 m2
    | LocalPath {path = p1; manifest = m1},
      Source.LocalPathLink {path = p2; manifest = m2} ->
      Path.equal p1 p2 && eqManifestName m1 m2
    | LocalPath _, _ -> false

    | LocalPathLink {path = p1; manifest = m1},
      Source.LocalPathLink {path = p2; manifest = m2} ->
      Path.equal p1 p2 && eqManifestName m1 m2
    | LocalPathLink _, _ -> false

    | Github ({ref = Some specRef; manifest = m1; _} as spec),
      Source.Github src ->
      String.(
        equal src.user spec.user
        && equal src.repo spec.repo
        && equal src.commit specRef
      ) && eqManifestName src.manifest m1
    | Github ({ref = None; _} as spec),
      Source.Github src ->
      String.(
        equal spec.user src.user
        && equal spec.repo src.repo
      ) && eqManifestName spec.manifest src.manifest
    | Github _, _ -> false

    | Git ({ref = Some specRef; _} as spec),
      Source.Git src ->
      String.(
        equal spec.remote src.remote
        && equal specRef src.commit
      ) && eqManifestName spec.manifest src.manifest
    | Git ({ref = None; _} as spec),
      Source.Git src ->
      String.(equal spec.remote src.remote)
      && eqManifestName spec.manifest src.manifest
    | Git _, _ -> false

    | Archive {url = url1; _},
      Source.Archive {url = url2; _}  ->
      String.equal url1 url2
    | Archive _, _ -> false

    | NoSource, _ -> false
  in
  match source, spec with
  | Source.Orig source, Orig sourceSpec ->
    matchesOrig source sourceSpec
  | Source.Override {source; override = sourceOverride},
    Override {sourceSpec; override = sourceSpecOverride} ->
    Types.SourceOverride.equal sourceOverride sourceSpecOverride
    && matchesOrig source sourceSpec
  | Source.Override _, Orig _ -> false
  | Source.Orig _, Override _ -> false

let ofSource (source : Source.t) =
  let aux (source : Source.source) =
    match source with
    | Archive {url; checksum} -> Archive {url; checksum = Some checksum}
    | Git {remote; commit; manifest;} ->
      Git {remote; ref =  Some commit; manifest;}
    | Github {user; repo; commit; manifest;} ->
      Github {user; repo; ref = Some commit; manifest;}
    | LocalPath {path; manifest;} ->
      LocalPath {path; manifest;}
    | LocalPathLink {path; manifest;} ->
      LocalPathLink {path; manifest;}
    | NoSource ->
      NoSource
  in
  match source with
  | Orig source -> Orig (aux source)
  | Override {source; override} -> Override {sourceSpec = aux source; override}

module Parse = struct
  include Parse

  let manifestFilenameBeforeSharp =
    till (fun c -> c <> '#') MS.parser

  let collectString xs =
    let l = List.length xs in
    let s = Bytes.create l in
    List.iteri ~f:(fun i c -> Bytes.set s i c) xs;
    Bytes.unsafe_to_string s

  let githubWithoutProto =
    let user = take_while1 (fun c -> c <> '/') in
    let repo =
      many_till any_char (string ".git") >>| collectString
      <|> take_while1 (fun c -> c <> '#' && c <> ':' && c <> '/')
    in
    let manifest = maybe (char ':' *> manifestFilenameBeforeSharp) in
    let ref = maybe (char '#' *> take_while1 (fun _ -> true)) in
    let make user repo manifest ref =
      Github { user; repo; ref; manifest; }
    in
    make <$> (user <* char '/') <*> repo <*> manifest <*> ref

  let github =
    let prefix = string "github:" <|> string "gh:" in
    prefix *> githubWithoutProto

  let git =
    let prefix = string "git+" in
    let proto =
      let gitWithProto =
        prefix *> (
          string "https:"
          <|> string "http:"
          <|> string "ssh:"
          <|> string "ftp:"
          <|> string "rsync:"
        )
      in
      gitWithProto <|> string "git:"
    in
    let remote = take_while1 (fun c -> c <> '#' && c <> ':') in
    let manifest = maybe (char ':' *> manifestFilenameBeforeSharp) in
    let ref = maybe (char '#' *> take_while1 (fun _ -> true)) in
    let make proto remote manifest ref =
      Git { remote = proto ^ remote; ref; manifest; }
    in
    (make <$> proto <*> remote <*> manifest <*> ref)

  let archive =
    let proto = string "https:" <|> string "http:" in
    let url = take_while1 (fun c -> c <> '#') in
    let checksum = maybe (char '#' *> Checksum.parser) in
    let make proto url checksum =
      Archive { url = proto ^ url; checksum; }
    in
    (make <$> proto <*> url <*> checksum)

  let pathWithoutProto make =
    let path = take_while1 (fun _ -> true) in
    let make path =
      let path = Path.(normalizeAndRemoveEmptySeg (v path)) in
      let path, manifest =
        match MS.ofString (Path.basename path) with
        | Ok manifest ->
          let path = Path.(remEmptySeg (parent path)) in
          path, Some manifest
        | Error _ ->
          path, None
      in
      make path manifest
    in
    (make <$> path)

  let pathLike proto make =
    string proto *> pathWithoutProto make

  let file =
    let make path manifest = LocalPath { path; manifest; } in
    pathLike "file:" make

  let path =
    let make path manifest = LocalPath { path; manifest; } in
    pathLike "path:" make

  let link =
    let make path manifest = LocalPathLink { path; manifest; } in
    pathLike "link:" make

  let origSourceSpec =
    let source = Parse.(
      github
      <|> git
      <|> archive
      <|> file
      <|> path
      <|> link
      <|> githubWithoutProto
    ) in
    let makePath path manifest = LocalPath { path; manifest; } in
    match%bind peek_char_fail with
    | '.'
    | '/' -> pathWithoutProto makePath
    | _ -> source

  let sourceSpec =
    let%bind sourceSpec = origSourceSpec in
    return (Orig sourceSpec)

end

let parser: t Parse.t = Parse.sourceSpec

let parse =
  Parse.(parse parser)

let%test_module "parsing" = (module struct

  let expectParses =
    Parse.Test.expectParses ~pp ~equal parse

  let%test "github:user/repo" =
    expectParses
      "github:user/repo"
      (Orig (Github {user = "user"; repo = "repo"; ref = None; manifest = None}))

  let%test "github:user/repo.git" =
    expectParses
      "github:user/repo.git"
      (Orig (Github {user = "user"; repo = "repo"; ref = None; manifest = None}))

  let%test "github:user/repo#ref" =
    expectParses
      "github:user/repo#ref"
      (Orig (Github {user = "user"; repo = "repo"; ref = Some "ref"; manifest = None}))

  let%test "github:user/repo:lwt.opam#ref" =
    expectParses
      "github:user/repo:lwt.opam#ref"
      (Orig (Github {
        user = "user";
        repo = "repo";
        ref = Some "ref";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "github:user/repo:lwt.opam" =
    expectParses
      "github:user/repo:lwt.opam"
      (Orig (Github {
        user = "user";
        repo = "repo";
        ref = None;
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "gh:user/repo" =
    expectParses
      "gh:user/repo"
      (Orig (Github {
        user = "user";
        repo = "repo";
        ref = None;
        manifest = None;
      }))

  let%test "gh:user/repo#ref" =
    expectParses
      "gh:user/repo#ref"
      (Orig (Github {
        user = "user";
        repo = "repo";
        ref = Some "ref";
        manifest = None;
      }))

  let%test "gh:user/repo:lwt.opam#ref" =
    expectParses
      "gh:user/repo:lwt.opam#ref"
      (Orig (Github {
        user = "user";
        repo = "repo";
        ref = Some "ref";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "gh:user/repo:lwt.opam" =
    expectParses
      "gh:user/repo:lwt.opam"
      (Orig (Github {
        user = "user";
        repo = "repo";
        ref = None;
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "git+https://example.com/repo.git" =
    expectParses
      "git+https://example.com/repo.git"
      (Orig (Git {
        remote = "https://example.com/repo.git";
        ref = None;
        manifest = None;
      }))

  let%test "git+https://example.com/repo.git#ref" =
    expectParses
      "git+https://example.com/repo.git#ref"
      (Orig (Git {
        remote = "https://example.com/repo.git";
        ref = Some "ref";
        manifest = None;
      }))

  let%test "git+https://example.com/repo.git:lwt.opam#ref" =
    expectParses
      "git+https://example.com/repo.git:lwt.opam#ref"
      (Orig (Git {
        remote = "https://example.com/repo.git";
        ref = Some "ref";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "git+https://example.com/repo.git:lwt.opam" =
    expectParses
      "git+https://example.com/repo.git:lwt.opam"
      (Orig (Git {
        remote = "https://example.com/repo.git";
        ref = None;
        manifest = Some (MS.ofStringExn "lwt.opam")
      }))

  let%test "git+http://example.com/repo.git:lwt.opam#ref" =
    expectParses
      "git+http://example.com/repo.git:lwt.opam#ref"
      (Orig (Git {
        remote = "http://example.com/repo.git";
        ref = Some "ref";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "git+ftp://example.com/repo.git:lwt.opam#ref" =
    expectParses
      "git+ftp://example.com/repo.git:lwt.opam#ref"
      (Orig (Git {
        remote = "ftp://example.com/repo.git";
        ref = Some "ref";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "git+ssh://example.com/repo.git:lwt.opam#ref" =
    expectParses
      "git+ssh://example.com/repo.git:lwt.opam#ref"
      (Orig (Git {
        remote = "ssh://example.com/repo.git";
        ref = Some "ref";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "git+rsync://example.com/repo.git:lwt.opam#ref" =
    expectParses
      "git+rsync://example.com/repo.git:lwt.opam#ref"
      (Orig (Git {
        remote = "rsync://example.com/repo.git";
        ref = Some "ref";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "git://example.com/repo.git" =
    expectParses
      "git://example.com/repo.git"
      (Orig (Git {
        remote = "git://example.com/repo.git";
        ref = None;
        manifest = None;
      }))

  let%test "git://example.com/repo.git#ref" =
    expectParses
      "git://example.com/repo.git#ref"
      (Orig (Git {
        remote = "git://example.com/repo.git";
        ref = Some "ref";
        manifest = None;
      }))

  let%test "git://example.com/repo.git:lwt.opam#ref" =
    expectParses
      "git://example.com/repo.git:lwt.opam#ref"
      (Orig (Git {
        remote = "git://example.com/repo.git";
        ref = Some "ref";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "git://github.com/yarnpkg/example-yarn-package.git" =
    expectParses
      "git://github.com/yarnpkg/example-yarn-package.git"
      (Orig (Git {
        remote = "git://github.com/yarnpkg/example-yarn-package.git";
        ref = None;
        manifest = None;
      }))

  let%test "user/repo" =
    expectParses
      "user/repo"
      (Orig (Github {
        user = "user";
        repo = "repo";
        ref = None;
        manifest = None;
      }))

  let%test "user/repo#ref" =
    expectParses
      "user/repo#ref"
      (Orig (Github {
        user = "user";
        repo = "repo";
        ref = Some "ref";
        manifest = None;
      }))

  let%test "user/repo:lwt.opam#ref" =
    expectParses
      "user/repo:lwt.opam#ref"
      (Orig (Github {
        user = "user";
        repo = "repo";
        ref = Some "ref";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "user/repo:lwt.opam" =
    expectParses
      "user/repo:lwt.opam"
      (Orig (Github {
        user = "user";
        repo = "repo";
        ref = None;
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "https://example.com/pkg.tgz" =
    expectParses
      "https://example.com/pkg.tgz"
      (Orig (Archive {url = "https://example.com/pkg.tgz"; checksum = None}))

  let%test "https://example.com/pkg.tgz#abc123" =
    expectParses
      "https://example.com/pkg.tgz#abc123"
      (Orig (Archive {
        url = "https://example.com/pkg.tgz";
        checksum = Some (Sha1, "abc123");
      }))

  let%test "http://example.com/pkg.tgz" =
    expectParses
      "http://example.com/pkg.tgz"
      (Orig (Archive {url = "http://example.com/pkg.tgz"; checksum = None}))

  let%test "http://example.com/pkg.tgz#abc123" =
    expectParses
      "http://example.com/pkg.tgz#abc123"
      (Orig (Archive {
        url = "http://example.com/pkg.tgz";
        checksum = Some (Sha1, "abc123");
      }))

  let%test "link:/some/path" =
    expectParses
      "link:/some/path"
      (Orig (LocalPathLink {path = Path.v "/some/path"; manifest = None;}))

  let%test "link:/some/path/opam" =
    expectParses
      "link:/some/path/opam"
      (Orig (LocalPathLink {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "opam");
      }))

  let%test "link:/some/path/lwt.opam" =
    expectParses
      "link:/some/path/lwt.opam"
      (Orig (LocalPathLink {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "link:/some/path/package.json" =
    expectParses
      "link:/some/path/package.json"
      (Orig (LocalPathLink {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "package.json");
      }))

  let%test "file:/some/path" =
    expectParses
      "file:/some/path"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = None;
      }))

  let%test "file:/some/path/opam" =
    expectParses
      "file:/some/path/opam"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "opam");
      }))

  let%test "file:/some/path/lwt.opam" =
    expectParses
      "file:/some/path/lwt.opam"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "file:/some/path/package.json" =
    expectParses
      "file:/some/path/package.json"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "package.json");
      }))

  let%test "path:/some/path" =
    expectParses
      "path:/some/path"
      (Orig (LocalPath {path = Path.v "/some/path"; manifest = None;}))

  let%test "path:/some/path/opam" =
    expectParses
      "path:/some/path/opam"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "opam");
      }))

  let%test "path:/some/path/lwt.opam" =
    expectParses
      "path:/some/path/lwt.opam"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "path:/some/path/package.json" =
    expectParses
      "path:/some/path/package.json"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "package.json");
      }))

  let%test "/some/path" =
    expectParses
      "/some/path"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = None;
      }))

  let%test "/some/path/opam" =
    expectParses
      "/some/path/opam"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "opam");
      }))

  let%test "/some/path/lwt.opam" =
    expectParses
      "/some/path/lwt.opam"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "/some/path/package.json" =
    expectParses
      "/some/path/package.json"
      (Orig (LocalPath {
        path = Path.v "/some/path";
        manifest = Some (MS.ofStringExn "package.json");
      }))

  let%test "./some/path" =
    expectParses
      "./some/path"
      (Orig (LocalPath {
        path = Path.v "some/path";
        manifest = None;
      }))

  let%test "./some/path/opam" =
    expectParses
      "./some/path/opam"
      (Orig (LocalPath {
        path = Path.v "some/path";
        manifest = Some (MS.ofStringExn "opam");
      }))

  let%test "./some/path/lwt.opam" =
    expectParses
      "./some/path/lwt.opam"
      (Orig (LocalPath {
        path = Path.v "some/path";
        manifest = Some (MS.ofStringExn "lwt.opam");
      }))

  let%test "./some/path/package.json" =
    expectParses
      "./some/path/package.json"
      (Orig (LocalPath {
        path = Path.v "some/path";
        manifest = Some (MS.ofStringExn "package.json");
      }))

end)

module Map = Map.Make(struct
  type nonrec t = t
  let compare = compare
end)

module Set = Set.Make(struct
  type nonrec t = t
  let compare = compare
end)
