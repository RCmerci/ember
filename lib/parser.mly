%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token VECTOR_PAREN
%token LEFT_PAREN
%token RIGHT_PAREN
%token QUOTE
%token QUASIQUOTE
%token UNQUOTE
%token UNQUOTE_SPLICING
%token EOF


(* part "1" *)
%start <Syntax.t option> prog
%%


(* part "2" *)
prog:
  | EOF       { None }
  | v = value { Some v }
  ;


(* part "3" *)
value:
  | LEFT_PAREN; obj = object_fields; RIGHT_PAREN
    { Syntax.LIST obj}
  | VECTOR_PAREN; obj = object_fields; RIGHT_PAREN
    { Syntax.ARRAY obj }
  | s = STRING
    { Syntax.STRING s }
  | i = INT
    { Syntax.INT i }
  | x = FLOAT
    { Syntax.FLOAT x }
  | TRUE
    { Syntax.BOOL true }
  | FALSE
    { Syntax.BOOL false }
  | v = ID
    { Syntax.ID v }
  | NULL
    { Syntax.LIST [] }
  | QUOTE; v = value
    { Syntax.LIST [Syntax.ID "quote"; v] }
  | QUASIQUOTE; v = value
    { Syntax.LIST [Syntax.ID "quasiquote"; v]}
  | UNQUOTE; v = value
    { Syntax.LIST [Syntax.ID "unquote"; v] }
  | UNQUOTE_SPLICING; v = value
    { Syntax.LIST [Syntax.ID "unquote-splicing"; v] }
  ;


(* part "4" *)
object_fields: obj = rev_object_fields { List.rev obj };

rev_object_fields:
  | (* empty *) { [] }
  | obj = rev_object_fields; v = value
    { v :: obj }
  ;
