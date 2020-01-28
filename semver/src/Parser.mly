%token <Import.Types.Version.t> VERSION
%token <Import.Types.Formula.pattern> PATTERN
%token <Import.Types.Formula.op> OP
%token <Import.Types.Formula.spec> SPEC
%token <string> NUM
%token <string> WORD
%token DOT
%token PLUS
%token OR
%token WS
%token HYPHEN
%token EOF

%start parse_version parse_formula
%type <Import.Types.Version.t> parse_version
%type <Import.Types.Formula.t> parse_formula

%{
  open Import.Types.Version
  open Import.Types.Formula
%}

%%

parse_version:
  WS?; v = version; WS?; EOF { v }

parse_formula:
  WS?; v = disj; EOF { v }

disj:
    v = range { [v] }
  | { [Simple [Patt (Pattern Any)]] }
  | v = range; OR; vs = disj { v::vs }

range:
    v = clauses { Simple v }
  | a = pattern; HYPHEN; b = pattern { Hyphen (a, b) }

clauses:
    v = clause; WS? { [v] }
  | v = clause; WS; vs = clauses { v::vs }

clause:
    v = pattern { Patt v }
  | op = OP; v = pattern { Expr (op, v) }
  | spec = SPEC; v = pattern { Spec (spec, v) }

pattern:
    v = version { Version v }
  | p = PATTERN { Pattern p }

version:
  v = VERSION; p = loption(prerelease); b = loption(build) {
    {v with prerelease = p; build = b}
  }

build:
  v = preceded(PLUS, separated_nonempty_list(DOT, build_id)) { v }

prerelease:
  v = separated_nonempty_list(DOT, prerelease_id) { v }

prerelease_id:
    v = NUM { N (int_of_string v) }
  | v = WORD { A v }

build_id:
    v = NUM { v }
  | v = WORD { v }

%%
