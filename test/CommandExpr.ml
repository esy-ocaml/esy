open Esy.CommandExpr

let expectParseOk s expectedTokens =
  match parse s with
  | Ok tokens ->
    if not (equal tokens expectedTokens) then (
      Printf.printf "Expected: %s\n" (show expectedTokens);
      Printf.printf "     Got: %s\n" (show tokens);
      false
    ) else
      true
  | Error err ->
    let msg = Printf.sprintf "parse error: %s" (EsyLib.Run.formatError err) in
    print_endline msg;
    false

let%test "parse just string" =
  expectParseOk "something" [String "something"]

let%test "parse just string w/ leading space" =
  expectParseOk " something" [String " something"]

let%test "parse just string w/ trailing space" =
  expectParseOk "something " [String "something "]

let%test "string with squote" =
  expectParseOk "somet'ok'hing" [String "somet'ok'hing"] &&
  expectParseOk "somet'ok' hing" [String "somet'ok' hing"] &&
  expectParseOk "somet 'ok'hing" [String "somet 'ok'hing"]

let%test "string with dquote" =
  expectParseOk "somet\"ok\"hing" [String "somet\"ok\"hing"] &&
  expectParseOk "somet\"ok\" hing" [String "somet\"ok\" hing"] &&
  expectParseOk "somet \"ok\"hing" [String "somet \"ok\"hing"]

let%test "parse simple var" =
  expectParseOk "#{hi}" [Expr [Var ["hi"]]] &&
  expectParseOk "#{hi }" [Expr [Var ["hi"]]] &&
  expectParseOk "#{ hi}" [Expr [Var ["hi"]]]

let%test "parse var+" =
  expectParseOk "#{hi}#{world}" [
    Expr [Var ["hi"]];
    Expr [Var ["world"]]
    ]

let%test "parse string + var" =
  expectParseOk "hello #{world}" [String "hello "; Expr [Var ["world"]]] &&
  expectParseOk " #{world}" [String " "; Expr [Var ["world"]]] &&
  expectParseOk "#{world} " [Expr [Var ["world"]]; String " "] &&
  expectParseOk "hello#{world}" [String "hello"; Expr [Var ["world"]]] &&
  expectParseOk "#{hello} world" [Expr [Var ["hello"]]; String " world"] &&
  expectParseOk "#{hello}world" [Expr [Var ["hello"]]; String "world"]

let%test "parse complex var" =
  expectParseOk "#{hi world}" [Expr [Var ["hi"]; Var ["world"]]] &&
  expectParseOk "#{h-i world}" [Expr [Var ["h-i"]; Var ["world"]]] &&
  expectParseOk "#{hi :}" [Expr [Var ["hi"]; Colon]] &&
  expectParseOk "#{hi : world}" [Expr [Var ["hi"]; Colon; Var ["world"]]] &&
  expectParseOk "#{hi /}" [Expr [Var ["hi"]; PathSep]] &&
  expectParseOk "#{hi / world}" [Expr [Var ["hi"]; PathSep; Var ["world"]]]

let%test "parse var with scope vars" =
  expectParseOk "#{hi / $world}" [Expr [Var ["hi"]; PathSep; EnvVar "world"]]

let%test "parse var with literals" =
  expectParseOk "#{'world'}" [Expr [Literal "world"]] &&
  expectParseOk "#{/ 'world'}" [Expr [PathSep; Literal "world"]] &&
  expectParseOk "#{: 'world'}" [Expr [Colon; Literal "world"]] &&
  expectParseOk "#{/'world'}" [Expr [PathSep; Literal "world"]] &&
  expectParseOk "#{:'world'}" [Expr [Colon; Literal "world"]] &&
  expectParseOk "#{'world' /}" [Expr [Literal "world"; PathSep]] &&
  expectParseOk "#{'world' :}" [Expr [Literal "world"; Colon]] &&
  expectParseOk "#{'world'/}" [Expr [Literal "world"; PathSep]] &&
  expectParseOk "#{'world':}" [Expr [Literal "world"; Colon]] &&
  expectParseOk "#{hi'world'}" [Expr [Var ["hi"]; Literal "world"]] &&
  expectParseOk "#{'world'hi}" [Expr [Literal "world"; Var ["hi"]]] &&
  expectParseOk "#{hi / 'world'}" [Expr [Var ["hi"]; PathSep; Literal "world"]] &&
  expectParseOk "#{'hi''world'}" [Expr [Literal "hi";  Literal "world"]] &&
  expectParseOk "#{'h\\'i'}" [Expr [Literal "h'i"]]

let%test "parse namespace" =
  expectParseOk "#{ns.hi}" [Expr [Var ["ns"; "hi"]]] &&
  expectParseOk "#{n-s.hi}" [Expr [Var ["n-s"; "hi"]]] &&
  expectParseOk "#{ns.hi.hey}" [Expr [Var ["ns"; "hi"; "hey"]]] &&
  expectParseOk "#{@scope/pkg.hi}" [Expr [Var ["@scope/pkg"; "hi"]]] &&
  expectParseOk "#{@s-cope/pkg.hi}" [Expr [Var ["@s-cope/pkg"; "hi"]]] &&
  expectParseOk "#{@scope/pkg.hi.hey}" [Expr [Var ["@scope/pkg"; "hi"; "hey"]]] &&
  expectParseOk "#{@scope/pkg.hi 'hey'}" [Expr [
    Var ["@scope/pkg"; "hi"];
    Literal ("hey");
  ]]

let expectRenderOk scope s expected =
  match render ~scope s with
  | Ok v ->
    if v <> expected then (
      Printf.printf "Expected: %s\n" expected;
      Printf.printf "     Got: %s\n" v;
      false
    ) else
      true
  | Error err ->
    let msg = Printf.sprintf "error: %s" (EsyLib.Run.formatError err) in
    print_endline msg;
    false

let expectRenderError scope s expectedError =
  match render ~scope s with
  | Ok _ -> false
  | Error error ->
    let error = EsyLib.Run.formatError error in
    if expectedError <> error then (
      Printf.printf "Expected: %s\n" expectedError;
      Printf.printf "     Got: %s\n" error;
      false
    ) else true

let%test "render" =
  let scope = function
  | "name"::[] -> Some "pkg"
  | "self"::"lib"::[] -> Some "store/lib"
  | _ -> None
  in

  expectRenderOk scope "Hello, #{name}!" "Hello, pkg!" &&
  expectRenderOk scope "#{self.lib / $NAME}" "store/lib/$NAME" &&
  expectRenderError scope "#{unknown}" "Error: Undefined variable 'unknown'" &&
  expectRenderError scope "#{ns.unknown}" "Error: Undefined variable 'ns.unknown'"

