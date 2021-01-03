/* Taken from https://github.com/OhadRau/RelevantPromises/blob/master/src/parser.mly */

%{
  open Language
%}

%token <bool>   BOOL
%token <int>    NAT
%token <string> STRING
%token <string> IDENT

%token FUN
%token TYPE_UNIT TYPE_BOOL TYPE_NAT TYPE_STRING
%token LEFT_PAREN RIGHT_PAREN
%token LEFT_BRACKET RIGHT_BRACKET
%token LCURLY RCURLY
%token COMMA
%token COLON
%token SEMI

%token EOF

%start toplevel
%type <Language.program> toplevel
%type <Language.fn> Fun
%%

toplevel:
  | EOF                         { { fns=[]; expr=None } }
  | e = Expr; EOF               { { fns=[]; expr=Some e } }
  | f = Fun; prog = toplevel    { { prog with fns=f::prog.fns } }
;

Fun:
  | FUN; name = IDENT; LEFT_PAREN; p = Params; RIGHT_PAREN COLON; ty = Type; LCURLY; e = Expr; RCURLY
    { Fn (name, p, ty, e) }
;

Type:
  | AtomicType  { $1 }
;

AtomicType:
  | TYPE_NAT    { TyNat }
  | TYPE_STRING { TyString }
;

Params:
  | { [] }
  | id = IDENT; COLON; ty = Type { [(id, ty)] }  
  | id = IDENT; COLON; ty = Type; COMMA; rest = Params { (id, ty)::rest }
;

Expr:
  | IDENT  { Var $1 }
  | NAT    { Nat $1 }
  | STRING { String $1 }
;
