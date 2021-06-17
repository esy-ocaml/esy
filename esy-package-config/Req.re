[@deriving ord]
type t = {
  name: string,
  spec: VersionSpec.t,
};

let show = ({name, spec}) => name ++ "@" ++ VersionSpec.show(spec);
let name = ({name, _}) => name;

let to_yojson = req => `String(show(req));

let pp = (fmt, req) => Fmt.fmt("%s", fmt, show(req));

module Parse = {
  include Parse;

  let name =
    take_while1(
      fun
      | '@'
      | '/' => false
      | _ => true,
    );
  let opamPackageName = {
    let make = (scope, name) => `opam(scope ++ name);
    make <$> string("@opam/") <*> name;
  };

  let npmPackageNameWithScope = {
    let make = (scope, name) => `npm("@" ++ scope ++ "/" ++ name);
    make <$> char('@') *> name <*> char('/') *> name;
  };

  let npmPackageName = {
    let make = name => `npm(name);
    make <$> name;
  };

  let packageName =
    opamPackageName <|> npmPackageNameWithScope <|> npmPackageName;

  let parser = {
    let* name = packageName;
    switch%bind (peek_char) {
    | None =>
      let (name, spec) =
        switch (name) {
        | `npm(name) => (
            name,
            VersionSpec.Npm([[SemverVersion.Constraint.ANY]]),
          )
        | `opam(name) => (
            name,
            VersionSpec.Opam([[OpamPackageVersion.Constraint.ANY]]),
          )
        };

      return({name, spec});
    | Some('@') =>
      let* () = advance(1);
      let* nextChar = peek_char;
      switch (nextChar, name) {
      | (None, _) =>
        let (name, spec) =
          switch (name) {
          | `npm(name) => (
              name,
              VersionSpec.Npm([[SemverVersion.Constraint.ANY]]),
            )
          | `opam(name) => (
              name,
              VersionSpec.Opam([[OpamPackageVersion.Constraint.ANY]]),
            )
          };

        return({name, spec});
      | (Some(_), `opam(name)) =>
        let* spec = VersionSpec.parserOpam;
        return({name, spec});
      | (Some(_), `npm(name)) =>
        let* spec = VersionSpec.parserNpm;
        return({name, spec});
      };
    | _ => fail("cannot parse request")
    };
  };
};

let parse = Parse.(parse(parser));

let%test_module "parsing" =
  (module
   {
     let cases = [
       (
         "name",
         {
           name: "name",
           spec: VersionSpec.Npm([[SemverVersion.Constraint.ANY]]),
         },
       ),
       (
         "name@",
         {
           name: "name",
           spec: VersionSpec.Npm([[SemverVersion.Constraint.ANY]]),
         },
       ),
       (
         "name@*",
         {
           name: "name",
           spec: VersionSpec.Npm([[SemverVersion.Constraint.ANY]]),
         },
       ),
       (
         "@scope/name",
         {
           name: "@scope/name",
           spec: VersionSpec.Npm([[SemverVersion.Constraint.ANY]]),
         },
       ),
       (
         "@scope/name@",
         {
           name: "@scope/name",
           spec: VersionSpec.Npm([[SemverVersion.Constraint.ANY]]),
         },
       ),
       (
         "@scope/name@*",
         {
           name: "@scope/name",
           spec: VersionSpec.Npm([[SemverVersion.Constraint.ANY]]),
         },
       ),
       (
         "@opam/name",
         {
           name: "@opam/name",
           spec: VersionSpec.Opam([[OpamPackageVersion.Constraint.ANY]]),
         },
       ),
       (
         "@opam/name@",
         {
           name: "@opam/name",
           spec: VersionSpec.Opam([[OpamPackageVersion.Constraint.ANY]]),
         },
       ),
       (
         "@opam/name@*",
         {
           name: "@opam/name",
           spec: VersionSpec.Opam([[OpamPackageVersion.Constraint.ANY]]),
         },
       ),
       (
         "name@git+https://some/repo",
         {
           name: "name",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "name.dot@git+https://some/repo",
         {
           name: "name.dot",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "name-dash@git+https://some/repo",
         {
           name: "name-dash",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "name_underscore@git+https://some/repo",
         {
           name: "name_underscore",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "@opam/name@git+https://some/repo",
         {
           name: "@opam/name",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "@scope/name@git+https://some/repo",
         {
           name: "@scope/name",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "@scope-dash/name@git+https://some/repo",
         {
           name: "@scope-dash/name",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "@scope.dot/name@git+https://some/repo",
         {
           name: "@scope.dot/name",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "@scope_underscore/name@git+https://some/repo",
         {
           name: "@scope_underscore/name",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "pkg@git://github.com/yarnpkg/example-yarn-package.git",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "git://github.com/yarnpkg/example-yarn-package.git",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "pkg@git+https://some/repo",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "pkg@git+https://some/repo#ref",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://some/repo",
                 ref: Some("ref"),
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "pkg@https://some/url#abc123",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Archive({
                 url: "https://some/url",
                 checksum: Some((Checksum.Sha1, "abc123")),
               }),
             ),
         },
       ),
       (
         "pkg@http://some/url#abc123",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Archive({
                 url: "http://some/url",
                 checksum: Some((Checksum.Sha1, "abc123")),
               }),
             ),
         },
       ),
       (
         "pkg@http://some/url#sha1:abc123",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Archive({
                 url: "http://some/url",
                 checksum: Some((Checksum.Sha1, "abc123")),
               }),
             ),
         },
       ),
       (
         "pkg@http://some/url#md5:abc123",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Archive({
                 url: "http://some/url",
                 checksum: Some((Checksum.Md5, "abc123")),
               }),
             ),
         },
       ),
       (
         "pkg@file:./some/file",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.LocalPath({
                 path: DistPath.v("some/file"),
                 manifest: None,
               }),
             ),
         },
       ),
       /* user/repo */
       (
         "pkg@user/repo",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Github({
                 user: "user",
                 repo: "repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "pkg@user/repo#ref",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Github({
                 user: "user",
                 repo: "repo",
                 ref: Some("ref"),
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "pkg@user/repo:lwt.opam#ref",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Github({
                 user: "user",
                 repo: "repo",
                 ref: Some("ref"),
                 manifest: Some((ManifestSpec.Opam, "lwt.opam")),
               }),
             ),
         },
       ),
       /* github:user/repo */
       (
         "pkg@github:user/repo",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Github({
                 user: "user",
                 repo: "repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "pkg@github:user/repo#ref",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Github({
                 user: "user",
                 repo: "repo",
                 ref: Some("ref"),
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "pkg@github:user/repo:lwt.opam#ref",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Github({
                 user: "user",
                 repo: "repo",
                 ref: Some("ref"),
                 manifest: Some((ManifestSpec.Opam, "lwt.opam")),
               }),
             ),
         },
       ),
       /* gh:user/repo */
       (
         "pkg@gh:user/repo",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Github({
                 user: "user",
                 repo: "repo",
                 ref: None,
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "pkg@gh:user/repo#ref",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Github({
                 user: "user",
                 repo: "repo",
                 ref: Some("ref"),
                 manifest: None,
               }),
             ),
         },
       ),
       (
         "pkg@gh:user/repo:lwt.opam#ref",
         {
           name: "pkg",
           spec:
             VersionSpec.Source(
               SourceSpec.Github({
                 user: "user",
                 repo: "repo",
                 ref: Some("ref"),
                 manifest: Some((ManifestSpec.Opam, "lwt.opam")),
               }),
             ),
         },
       ),
       (
         "eslint@git+https://github.com/eslint/eslint.git#9d6223040316456557e0a2383afd96be90d28c5a",
         {
           name: "eslint",
           spec:
             VersionSpec.Source(
               SourceSpec.Git({
                 remote: "https://github.com/eslint/eslint.git",
                 ref: Some("9d6223040316456557e0a2383afd96be90d28c5a"),
                 manifest: None,
               }),
             ),
         },
       ),
       /* npm */
       (
         "pkg@4.1.0",
         {
           name: "pkg",
           spec: VersionSpec.Npm(SemverVersion.Formula.parseExn("4.1.0")),
         },
       ),
       (
         "pkg@~4.1.0",
         {
           name: "pkg",
           spec: VersionSpec.Npm(SemverVersion.Formula.parseExn("~4.1.0")),
         },
       ),
       (
         "pkg@^4.1.0",
         {
           name: "pkg",
           spec: VersionSpec.Npm(SemverVersion.Formula.parseExn("^4.1.0")),
         },
       ),
       (
         "pkg@npm:>4.1.0",
         {
           name: "pkg",
           spec: VersionSpec.Npm(SemverVersion.Formula.parseExn(">4.1.0")),
         },
       ),
       (
         "pkg@npm:name@>4.1.0",
         {
           name: "pkg",
           spec: VersionSpec.Npm(SemverVersion.Formula.parseExn(">4.1.0")),
         },
       ),
       /* npm tags */
       (
         "pkg@latest",
         {name: "pkg", spec: VersionSpec.NpmDistTag("latest")},
       ),
       ("pkg@next", {name: "pkg", spec: VersionSpec.NpmDistTag("next")}),
       ("pkg@alpha", {name: "pkg", spec: VersionSpec.NpmDistTag("alpha")}),
       ("pkg@beta", {name: "pkg", spec: VersionSpec.NpmDistTag("beta")}),
       (
         "fastreplacestring@esy-ocaml/FastReplaceString#95f408b",
         {
           name: "fastreplacestring",
           spec:
             VersionSpec.Source(
               SourceSpec.Github({
                 user: "esy-ocaml",
                 repo: "FastReplaceString",
                 ref: Some("95f408b"),
                 manifest: None,
               }),
             ),
         },
       ),
     ];

     let expectParsesTo = (input, e) =>
       switch (parse(input)) {
       | Ok(req) =>
         if (compare(req, e) == 0) {
           true;
         } else {
           Format.printf(
             "@[<v>parsing: %s@\n     got: %a@\nexpected: %a@\n@]@\n",
             input,
             pp,
             req,
             pp,
             e,
           );
           false;
         }
       | Error(err) =>
         Format.printf("@[<v>parsing: %s@\n  error: %s@]@\n", input, err);
         false;
       };

     let%test "parsing" = {
       let f = (passes, (req, e)) => {
         let thisPasses = expectParsesTo(req, e);
         passes && thisPasses;
       };

       List.fold_left(~f, ~init=true, cases);
     };
   });

let make = (~name, ~spec) => {name, spec};

module Set =
  Set.Make({
    type nonrec t = t;
    let compare = compare;
  });

module Map =
  Map.Make({
    type nonrec t = t;
    let compare = compare;
  });
