%token <string> STRING
%token <string> ID
%token OPAM_OPEN
%token OPAM_CLOSE
%token QUESTION_MARK
%token COLON
%token DOT
%token SLASH
%token DOLLAR
%token AT
%token AND
%token PLUS
%token PAREN_LEFT
%token PAREN_RIGHT
%token EOF

%left QUESTION_MARK
%left AND

%{

  module E = CommandExprTypes.Expr

%}

%start start
%type <CommandExprTypes.Expr.t> start

%%

start:
  e = expr; EOF { e }

expr:
  | e = exprList { e }
  | PAREN_LEFT; e = exprList; PAREN_RIGHT { e }

(** Expressions which are allowed inside of then branch w/o parens *)
restrictedExpr:
    e = atomString { e }
  | e = atomId { e }
  | e = atomEnv { e }
  | PAREN_LEFT; e = expr; PAREN_RIGHT { e }

exprList:
  e = nonempty_list(atom) {
    match e with
    | [e] -> e
    | es -> E.Concat es
  }

atom:
    PAREN_LEFT; e = atom; PAREN_RIGHT { e }
  | OPAM_OPEN; e = opam_id; OPAM_CLOSE { E.OpamVar e }
  | e = atomString { e }
  | e = atomId { e }
  | e = atomEnv { e }
  | e = atomCond { e }
  | e = atomAnd { e }
  | SLASH { E.PathSep }
  | COLON { E.Colon }

atomAnd:
  a = atom; AND; b = atom { E.And (a, b) }

%inline atomCond:
  cond = atom; QUESTION_MARK; t = restrictedExpr; COLON; e = restrictedExpr { E.Condition (cond, t, e) }

%inline atomString:
  e = STRING { E.String e }

%inline atomEnv:
  DOLLAR; n = ID { E.EnvVar n }

%inline atomId:
  | e = id { E.Var e }

id:
    id = ID { (None, id) }
  | namespace = id_namespace; DOT; id = ID { (Some namespace, id) }


id_namespace:
    n = ID { n }
  | AT; s = ID; SLASH; n = ID { ("@" ^ s ^ "/" ^ n) }

opam_id:
    id = ID { ([], id) }
  | scope = opam_id_scope; COLON; id = ID { (scope, id) }

opam_id_scope:
  e = separated_nonempty_list(PLUS, ID) { e }

%%

