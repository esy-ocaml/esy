{

  exception Error of string

  let unexpected lexbuf =
    raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf))

  open Parser
  open Import.Types.Version
  open Import.Types.Formula

  (* We make actions return this type instead of tokens directly. This allows to
   * swicth between different lexer "modes" dynamically.
   *
   * Note that [K lexer] which doesn't produce a token backtracks, so the
   * location captured by the action which returned will be backtracked to a
   * previous position.
   *)
  type res =
    | R of token
      (* return token *)
    | K of (Lexing.lexbuf -> res)
      (* continue with another lexer *)
    | RK of token * (Lexing.lexbuf -> res)
      (* return token, then continue with another lexer *)

  let rec run curr lexbuf =
    let lexer = !curr in
    match lexer lexbuf with
    | R tok ->
      tok
    | K next ->
      curr := next;
      (* backtrack so the next lexer can reconsider and emit some token *)
      lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
      lexbuf.lex_curr_p <- {
        lexbuf.lex_start_p with
        pos_cnum = lexbuf.lex_abs_pos + lexbuf.lex_curr_pos
      };
      run curr lexbuf
    | RK (tok, next) ->
      curr := next;
      tok

  let make start () = run (ref start)

}

let n = ['0' - '9']
let a = ['a' - 'z'] | ['A'-'Z']
let num = n+
let word = (a | n | '-')+
let ws = ' '

let star = 'x' | 'X' | '*'
let prefix = 'v' | '='

rule main = parse
  | prefix? ws* (num as major) '.' (num as minor) '.' (num as patch) '-'? {
      let version = {
        major = int_of_string major;
        minor = int_of_string minor;
        patch = int_of_string patch;
        prerelease = [];
        build = [];
      } in
      RK (VERSION version, words)
    }
  | prefix? ws* (num as major) '.' (num as minor) ('.' star)? {
      R (PATTERN (Minor (int_of_string major, int_of_string minor)))
    }
  | prefix? ws* (num as major) ('.' star ('.' star)?)? {
      R (PATTERN (Major (int_of_string major)))
    }
  | prefix? ws* star ('.' star ('.' star)?)? {
      R (PATTERN Any)
    }
  | '^' ws* { R (SPEC Caret) }
  | '~' '>'? ws* { R (SPEC Tilda) }
  | '>' ws* { R (OP GT) }
  | '<' ws* { R (OP LT) }
  | '>' '=' ws* { R (OP GTE) }
  | '<' '=' ws* { R (OP LTE) }
  | '=' ws* { R (OP EQ) }
  | ws+ '-' ws+ { R HYPHEN }
  | ws* '|' '|' ws* { R OR }
  | ws+ { R WS }
  | eof { R EOF }
  | _ { unexpected lexbuf }

and words = parse
  | '.' { R DOT }
  | '+' { R PLUS }
  | num as v { R (NUM v) }
  | word as v { R (WORD v) }
  | eof { R EOF }
  | _ { K main }
