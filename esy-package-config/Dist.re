open Sexplib0.Sexp_conv;

[@deriving (ord, sexp_of)]
type local = {
  path: DistPath.t,
  manifest: option(ManifestSpec.t),
};

[@deriving (ord, sexp_of)]
type t =
  | Archive{
      url: string,
      checksum: Checksum.t,
    }
  | Git{
      remote: string,
      commit: string,
      manifest: option(ManifestSpec.t),
    }
  | Github{
      user: string,
      repo: string,
      commit: string,
      manifest: option(ManifestSpec.t),
    }
  | LocalPath(local)
  | NoSource;

let manifest = (dist: t) =>
  switch (dist) {
  | Git({manifest: Some(manifest), _}) => Some(manifest)
  | Git(_) => None
  | Github({manifest: Some(manifest), _}) => Some(manifest)
  | Github(_) => None
  | LocalPath(info) => info.manifest
  | Archive(_) => None
  | NoSource => None
  };

let show' = (~showPath) =>
  fun
  | Github({user, repo, commit, manifest: None}) =>
    Printf.sprintf("github:%s/%s#%s", user, repo, commit)
  | Github({user, repo, commit, manifest: Some(manifest)}) =>
    Printf.sprintf(
      "github:%s/%s:%s#%s",
      user,
      repo,
      ManifestSpec.show(manifest),
      commit,
    )
  | Git({remote, commit, manifest: None}) =>
    Printf.sprintf("git:%s#%s", remote, commit)
  | Git({remote, commit, manifest: Some(manifest)}) =>
    Printf.sprintf(
      "git:%s:%s#%s",
      remote,
      ManifestSpec.show(manifest),
      commit,
    )
  | Archive({url, checksum}) =>
    Printf.sprintf("archive:%s#%s", url, Checksum.show(checksum))
  | LocalPath({path, manifest: None}) =>
    Printf.sprintf("path:%s", showPath(path))
  | LocalPath({path, manifest: Some(manifest)}) =>
    Printf.sprintf(
      "path:%s/%s",
      showPath(path),
      ManifestSpec.show(manifest),
    )
  | NoSource => "no-source:";

let show = show'(~showPath=DistPath.show);
let showPretty = show'(~showPath=DistPath.showPretty);

let pp = (fmt, src) => Fmt.pf(fmt, "%s", show(src));

let ppPretty = (fmt, src) => Fmt.pf(fmt, "%s", showPretty(src));

module Parse = {
  include Parse;

  let manifestFilenameBeforeSharp = till(c => c != '#', ManifestSpec.parser);

  let commitsha = {
    let err = fail("missing or incorrect <commit>");
    let%bind () = ignore(char('#')) <|> err;
    let%bind () = commit;
    let%bind sha = hex <|> err;
    if (String.length(sha) < 6) {
      err;
    } else {
      return(sha);
    };
  };

  let gitOrGithubManifest =
    switch%bind (peek_char) {
    | Some(':') =>
      let%bind () = advance(1);
      let%map manifest =
        manifestFilenameBeforeSharp
        <|> fail("missing or incorrect <manifest>");

      Some(manifest);
    | _ => return(None)
    };

  let github =
    {
      let%bind user =
        take_while1(c => c != '/')
        <* char('/')
        <|> fail("missing or incorrect <author>/<repo>");

      let%bind repo =
        take_while1(c => c != '#' && c != ':')
        <|> fail("missing or incorrect <author>/<repo>");

      let%bind manifest = gitOrGithubManifest;
      let%bind commit = commitsha;
      return(Github({user, repo, commit, manifest}));
    }
    <?> "<author>/<repo>(:<manifest>)?#<commit>";

  let git =
    {
      let%bind proto =
        take_while1(c => c != ':')
        <* char(':')
        <|> fail("missing on incorrect <remote>");

      let%bind remote =
        take_while1(c => c != '#' && c != ':')
        <|> fail("missing on incorrect <remote>");

      let%bind manifest = gitOrGithubManifest;
      let%bind commit = commitsha;
      return(Git({remote: proto ++ ":" ++ remote, commit, manifest}));
    }
    <?> "<remote>(:<manifest>)?#<commit>";

  let archive =
    {
      let%bind proto =
        take_while1(c => c != ':')
        <* string("://")
        <|> fail("missing on incorrect <remote>");

      let%bind () = commit;
      let%bind proto =
        switch (proto) {
        | "http"
        | "https" => return(proto)
        | _ => fail("incorrect protocol: expected http: or https:")
        };

      let%bind host = take_while1(c => c != '#');
      let%bind checksum =
        char('#')
        *> Checksum.parser
        <|> fail("missing or incorrect <checksum>");

      return(Archive({url: proto ++ "://" ++ host, checksum}));
    }
    <?> "https?://<host>/<path>#<checksum>";

  let local = (~requirePathSep) => {
    let make = path => {
      let path = Path.(normalizeAndRemoveEmptySeg(v(path)));
      let (path, manifest) =
        switch (ManifestSpec.ofString(Path.basename(path))) {
        | Ok(manifest) =>
          let path = Path.(remEmptySeg(parent(path)));
          (path, Some(manifest));
        | Error(_) => (path, None)
        };

      {path: DistPath.ofPath(path), manifest};
    };

    let path =
      scan(false, (seenPathSep, c) => Some(seenPathSep || c == '/'));

    let%bind (path, seenPathSep) = path;
    if (!requirePathSep || seenPathSep) {
      return(make(path));
    } else {
      fail("not a path");
    };
  };

  let path = (~requirePathSep) => {
    let%map local = local(~requirePathSep);
    LocalPath(local);
  };

  let proto =
    string("git:")
    >>= const(`Git)
    <|> (string("github:") >>= const(`GitHub))
    <|> (string("gh:") >>= const(`GitHub))
    <|> (string("archive:") >>= const(`Archive))
    <|> (string("path:") >>= const(`Path))
    <|> (string("no-source:") >>= const(`NoSource));

  let parser = {
    let%bind proto = proto;
    let%bind () = commit;
    switch (proto) {
    | `Git => git
    | `GitHub => github
    | `Archive => archive
    | `Path => path(~requirePathSep=false)
    | `NoSource => return(NoSource)
    };
  };

  let parserRelaxed =
    parser <|> archive <|> github <|> path(~requirePathSep=true);

  let%test_module "Parse tests" =
    (module
     {
       let test = Test.parse(~sexp_of=sexp_of_t, parse(parser));
       let testRelaxed =
         Test.parse(~sexp_of=sexp_of_t, parse(parserRelaxed));

       /* Testing parser: errors */

       let%expect_test "github:user/repo#ref" = {
         test("github:user/repo#ref");
         %expect
         {|
      ERROR: parsing "github:user/repo#ref": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "github:user/repo#" = {
         test("github:user/repo#");
         %expect
         {|
      ERROR: parsing "github:user/repo#": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "github:user/repo:#abc123" = {
         test("github:user/repo:#abc123");
         %expect
         {|
      ERROR: parsing "github:user/repo:#abc123": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <manifest>
      |};
       };

       let%expect_test "github:user/repo" = {
         test("github:user/repo");
         %expect
         {|
      ERROR: parsing "github:user/repo": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "github:user" = {
         test("github:user");
         %expect
         {|
      ERROR: parsing "github:user": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |};
       };

       let%expect_test "github:/repo" = {
         test("github:/repo");
         %expect
         {|
      ERROR: parsing "github:/repo": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |};
       };

       let%expect_test "github:user/" = {
         test("github:user/");
         %expect
         {|
      ERROR: parsing "github:user/": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |};
       };

       let%expect_test "github:/" = {
         test("github:/");
         %expect
         {|
      ERROR: parsing "github:/": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |};
       };

       let%expect_test "github:" = {
         test("github:");
         %expect
         {|
      ERROR: parsing "github:": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |};
       };

       let%expect_test "git:https://example.com#ref" = {
         test("git:https://example.com#ref");
         %expect
         {|
      ERROR: parsing "git:https://example.com#ref": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "git:https://example.com#" = {
         test("git:https://example.com#");
         %expect
         {|
      ERROR: parsing "git:https://example.com#": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "git:https://example.com" = {
         test("git:https://example.com");
         %expect
         {|
      ERROR: parsing "git:https://example.com": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "git:" = {
         test("git:");
         %expect
         {|
      ERROR: parsing "git:": <remote>(:<manifest>)?#<commit>: missing on incorrect <remote>
      |};
       };

       let%expect_test "archive:https://example.com#gibberish" = {
         test("archive:https://example.com#gibberish");
         %expect
         {|
      ERROR: parsing "archive:https://example.com#gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "archive:https://example.com#md5:gibberish" = {
         test("archive:https://example.com#md5:gibberish");
         %expect
         {|
      ERROR: parsing "archive:https://example.com#md5:gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "archive:https://example.com#" = {
         test("archive:https://example.com#");
         %expect
         {|
      ERROR: parsing "archive:https://example.com#": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "archive:https://example.com" = {
         test("archive:https://example.com");
         %expect
         {|
      ERROR: parsing "archive:https://example.com": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "archive:ftp://example.com" = {
         test("archive:ftp://example.com");
         %expect
         {|
      ERROR: parsing "archive:ftp://example.com": https?://<host>/<path>#<checksum>: incorrect protocol: expected http: or https:
      |};
       };

       /* Testing parserRelaxed: errors */

       let%expect_test "github:user/repo#ref" = {
         testRelaxed("github:user/repo#ref");
         %expect
         {|
      ERROR: parsing "github:user/repo#ref": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "github:user/repo#" = {
         testRelaxed("github:user/repo#");
         %expect
         {|
      ERROR: parsing "github:user/repo#": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "github:user/repo:#abc123" = {
         testRelaxed("github:user/repo:#abc123");
         %expect
         {|
      ERROR: parsing "github:user/repo:#abc123": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <manifest>
      |};
       };

       let%expect_test "github:user/repo" = {
         testRelaxed("github:user/repo");
         %expect
         {|
      ERROR: parsing "github:user/repo": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "github:user" = {
         testRelaxed("github:user");
         %expect
         {|
      ERROR: parsing "github:user": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |};
       };

       let%expect_test "github:/repo" = {
         testRelaxed("github:/repo");
         %expect
         {|
      ERROR: parsing "github:/repo": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |};
       };

       let%expect_test "github:user/" = {
         testRelaxed("github:user/");
         %expect
         {|
      ERROR: parsing "github:user/": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |};
       };

       let%expect_test "github:/" = {
         testRelaxed("github:/");
         %expect
         {|
      ERROR: parsing "github:/": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |};
       };

       let%expect_test "github:" = {
         testRelaxed("github:");
         %expect
         {|
      ERROR: parsing "github:": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <author>/<repo>
      |};
       };

       let%expect_test "git:https://example.com#ref" = {
         testRelaxed("git:https://example.com#ref");
         %expect
         {|
      ERROR: parsing "git:https://example.com#ref": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "git:https://example.com#" = {
         testRelaxed("git:https://example.com#");
         %expect
         {|
      ERROR: parsing "git:https://example.com#": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "git:https://example.com" = {
         testRelaxed("git:https://example.com");
         %expect
         {|
      ERROR: parsing "git:https://example.com": <remote>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "git:" = {
         testRelaxed("git:");
         %expect
         {|
      ERROR: parsing "git:": <remote>(:<manifest>)?#<commit>: missing on incorrect <remote>
      |};
       };

       let%expect_test "archive:https://example.com#gibberish" = {
         testRelaxed("archive:https://example.com#gibberish");
         %expect
         {|
      ERROR: parsing "archive:https://example.com#gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "archive:https://example.com#md5:gibberish" = {
         testRelaxed("archive:https://example.com#md5:gibberish");
         %expect
         {|
      ERROR: parsing "archive:https://example.com#md5:gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "archive:https://example.com#" = {
         testRelaxed("archive:https://example.com#");
         %expect
         {|
      ERROR: parsing "archive:https://example.com#": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "archive:https://example.com" = {
         testRelaxed("archive:https://example.com");
         %expect
         {|
      ERROR: parsing "archive:https://example.com": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "archive:ftp://example.com" = {
         testRelaxed("archive:ftp://example.com");
         %expect
         {|
      ERROR: parsing "archive:ftp://example.com": https?://<host>/<path>#<checksum>: incorrect protocol: expected http: or https:
      |};
       };

       let%expect_test "https://example.com#gibberish" = {
         testRelaxed("https://example.com#gibberish");
         %expect
         {|
      ERROR: parsing "https://example.com#gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "https://example.com#md5:gibberish" = {
         testRelaxed("https://example.com#md5:gibberish");
         %expect
         {|
      ERROR: parsing "https://example.com#md5:gibberish": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "https://example.com#" = {
         testRelaxed("https://example.com#");
         %expect
         {|
      ERROR: parsing "https://example.com#": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "https://example.com" = {
         testRelaxed("https://example.com");
         %expect
         {|
      ERROR: parsing "https://example.com": https?://<host>/<path>#<checksum>: missing or incorrect <checksum>
      |};
       };

       let%expect_test "ftp://example.com" = {
         testRelaxed("ftp://example.com");
         %expect
         {|
      ERROR: parsing "ftp://example.com": https?://<host>/<path>#<checksum>: incorrect protocol: expected http: or https:
      |};
       };

       let%expect_test "user/repo#ref" = {
         testRelaxed("user/repo#ref");
         %expect
         {|
      ERROR: parsing "user/repo#ref": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };

       let%expect_test "user/repo#" = {
         testRelaxed("user/repo#");
         %expect
         {|
      ERROR: parsing "user/repo#": <author>/<repo>(:<manifest>)?#<commit>: missing or incorrect <commit>
      |};
       };
     });
};

let parser = Parse.parser;
let parserRelaxed = Parse.parserRelaxed;
let parse = Parse.(parse(parser));
let parseRelaxed = Parse.(parse(parserRelaxed));

let to_yojson = v => `String(show(v));

let of_yojson = json =>
  switch (json) {
  | `String(string) => parse(string)
  | _ => Error("expected string")
  };

let relaxed_of_yojson = json =>
  switch (json) {
  | `String(string) =>
    let parse = Parse.(parse(parserRelaxed));
    parse(string);
  | _ => Error("expected string")
  };

let local_of_yojson = json =>
  switch (json) {
  | `String(string) =>
    let parse = Parse.(parse(local(~requirePathSep=false)));
    parse(string);
  | _ => Error("expected string")
  };

let local_to_yojson = local =>
  switch (local.manifest) {
  | None => `String(DistPath.show(local.path))
  | Some(m) => `String(DistPath.(show(local.path / ManifestSpec.show(m))))
  };

module Map =
  Map.Make({
    type nonrec t = t;
    let compare = compare;
  });

module Set =
  Set.Make({
    type nonrec t = t;
    let compare = compare;
  });
