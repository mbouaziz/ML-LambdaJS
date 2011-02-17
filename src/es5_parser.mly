%{
open Prelude
open Es5_syntax
open JavaScript_syntax
open Desugar_helpers

(* All free variables "x" in the environment are renamed to "[[x]]" *)
let rename_env exp =
  let ren v exp = rename v ("[[" ^ v ^ "]]") exp in
  IdSet.fold ren (fv exp) exp

(* Macros for expanding arguments objects and function objects (a
little bit of desugaring)*)
    
let args_thunk = args_thunk ~from_parser:true
let func_expr_lambda = func_expr_lambda ~from_parser:true
let func_object = func_object ~from_parser:true

 %}

%token <int> INT
%token <float> NUM
%token <string> STRING
%token <bool> BOOL
%token <Prelude.id> ID
%token UNDEFINED NULL FUNC LET DELETE LBRACE RBRACE LPAREN RPAREN LBRACK
  RBRACK EQUALS COMMA COLON COLONEQ PRIM IF ELSE SEMI
  LABEL BREAK TRY CATCH FINALLY THROW LLBRACK RRBRACK EQEQEQUALS TYPEOF
  AMPAMP PIPEPIPE RETURN BANGEQEQUALS FUNCTION FIX WRITABLE GETTER SETTER
  CONFIG VALUE ENUM LT GT


%token EOF
%left COLONEQ
%left PIPEPIPE
%left AMPAMP
%left EQEQEQUALS BANGEQEQUALS
%left LBRACK
%left LPAREN


/* http://stackoverflow.com/questions/1737460/
   how-to-find-shift-reduce-conflict-in-this-yacc-file */

%type <Es5_syntax.prim_exp> prog
%type <('op1, 'op2, 'op3) Es5_syntax.exp -> ('op1, 'op2, 'op3) Es5_syntax.exp> env

%start prog
%start env


%%

const :
 | NUM { JavaScript_syntax.CNum $1 }
 | INT {  JavaScript_syntax.CInt $1 }
 | STRING {  JavaScript_syntax.CString $1 }
 | UNDEFINED { JavaScript_syntax.CUndefined }
 | NULL { JavaScript_syntax.CNull }
 | BOOL { JavaScript_syntax.CBool $1 }

attr :
 | STRING COLON exp { ($1, $3) }
 | ID COLON exp { ($1, $3) }

attrs :
 | { [] }
 | attr { [$1] }
 | attr COMMA attrs { $1 :: $3 }

attr_name :
 | WRITABLE { Writable }
 | CONFIG { Config }
 | VALUE { Value }
 | SETTER { Setter }
 | GETTER { Getter }
 | ENUM { Enum }

prop_attr :
 | attr_name COLON exp { ($1, $3) }

prop_attrs :
 | { [] }
 | prop_attr { [$1] }
 | prop_attr COMMA prop_attrs { $1 :: $3 }

prop :
 | STRING COLON LBRACE prop_attrs RBRACE { ($1, $4) }
 | ID COLON LBRACE prop_attrs RBRACE { ($1, $4) }

props :
 | { [] }
 | prop { [$1] }
 | prop COMMA props { $1 :: $3 }

exps :
 | { [] }
 | seq_exp { [$1] }
 | seq_exp COMMA exps { $1 :: $3 }

ids :
 | { [] }
 | ID { [$1] }
 | ID COMMA ids { $1 :: $3 }

func :
 | FUNC LPAREN ids RPAREN LBRACE RETURN seq_exp RBRACE
     { { p = $startpos, $endpos ; e = ELambda ($3, $7) } }

atom :
 | const { { p = $startpos, $endpos ; e = EConst $1 } }
 | ID { { p = $startpos, $endpos ; e = EId $1 } }
 | LBRACE LBRACK attrs RBRACK props RBRACE 
     { { p = $startpos, $endpos ; e = EObject ($3, $5) } }
 | LBRACE seq_exp RBRACE
     { $2 }
 | LPAREN seq_exp RPAREN { $2 }
 | func { $1 }
 | FUNCTION LPAREN ids RPAREN LBRACE RETURN seq_exp RBRACE
     {
       let ids = $3 in
       let body = $7 in
       let p = ($startpos, $endpos) in
       func_object p ids (func_expr_lambda p ids body)
     }
 | TYPEOF atom
     { { p = $startpos, $endpos ; e = EOp1 (`Prim1 "typeof", $2) } }
     
exp :
 | atom { $1 }
 | exp LPAREN exps RPAREN 
     { { p = $startpos, $endpos ; e = EApp ($1, $3) } }
 | PRIM LPAREN STRING COMMA seq_exp COMMA seq_exp COMMA seq_exp RPAREN
     { { p = $startpos, $endpos ; e = EOp3 (`Prim3 $3, $5, $7, $9) } }
 | PRIM LPAREN STRING COMMA seq_exp COMMA seq_exp RPAREN
     { { p = $startpos, $endpos ; e = EOp2 (`Prim2 $3, $5, $7) } }
 | PRIM LPAREN STRING COMMA seq_exp RPAREN
     { { p = $startpos, $endpos ; e = EOp1 (`Prim1 $3, $5) } }
 | ID COLONEQ exp
     { { p = $startpos, $endpos ; e = ESet ($1, $3) } }
 | exp EQEQEQUALS exp
     { { p = $startpos, $endpos ; e = EOp2 (`Prim2 "stx=", $1, $3) } }
 | exp BANGEQEQUALS exp
     { let p = ($startpos, $endpos) in
       { p ; e = EIf ({ p ; e = EOp2 (`Prim2 "stx=", $1, $3) },
		      { p ; e = EConst (CBool false) },
		      { p ; e = EConst (CBool true) }) } }
 | exp LBRACK seq_exp EQUALS seq_exp RBRACK
     { let p = ($startpos, $endpos) in
       { p ; e = ELet ("$newVal", $5,
		       { p ; e = EUpdateFieldSurface ($1, $3,
						      { p ; e = EId "$newVal" },
						      args_thunk p [{ p ; e = EId "$newVal" }]) }) } }
 | exp LBRACK seq_exp RBRACK
     { let p = ($startpos, $endpos) in
       { p ; e = EGetFieldSurface ($1,  $3, args_thunk p []) } }
 | exp LBRACK DELETE seq_exp RBRACK
     { { p = $startpos, $endpos ; e = EDeleteField ($1, $4) } }
 | exp LBRACK seq_exp LT attr_name GT RBRACK
     { { p = $startpos, $endpos ; e = EAttr ($5, $1, $3) } }
 | exp LBRACK seq_exp LT attr_name GT EQUALS seq_exp RBRACK
     { { p = $startpos, $endpos ; e = ESetAttr ($5, $1, $3, $8) } }
 | exp AMPAMP exp
     { { p = $startpos, $endpos ; e = EIf ($1, 
					   $3,
					   { p = $startpos, $endpos ; e = EConst (CBool false) }) } }
 | exp PIPEPIPE exp
     { let p = ($startpos, $endpos) in
       { p ; e = ELet ("%or", $1,
		       { p ; e = EIf ({ p ; e = EId "%or" }, { p ; e = EId "%or" }, $3) }) } }
 | FIX ID exp 
     { { p = $startpos, $endpos ; e = EFix ($2, $3) } }


cexp :
 | exp { $1 }
 | IF LPAREN seq_exp RPAREN seq_exp ELSE seq_exp
     { { p = $startpos, $endpos ; e = EIf ($3, $5, $7) } }
 | IF LPAREN seq_exp RPAREN seq_exp
     { { p = $startpos, $endpos ; e = EIf ($3, $5, 
					   { p = $startpos, $endpos ; e = EConst CUndefined }) } }
 | LABEL ID COLON seq_exp
     { { p = $startpos, $endpos ; e = ELabel ($2, $4) } }
 | BREAK ID cexp
     { { p = $startpos, $endpos ; e = EBreak ($2, $3) } }
 | THROW cexp
     { { p = $startpos, $endpos ; e = EThrow $2 } }
 | TRY LBRACE seq_exp RBRACE CATCH LBRACE seq_exp RBRACE
     { { p = $startpos, $endpos ; e = ETryCatch ($3, $7) } }
 | TRY LBRACE seq_exp RBRACE FINALLY LBRACE seq_exp RBRACE
     { { p = $startpos, $endpos ; e = ETryFinally ($3, $7) } }

seq_exp :
 | cexp { $1 }
 | LET LPAREN ID EQUALS seq_exp RPAREN seq_exp
     { { p = $startpos, $endpos ; e = ELet ($3, $5, $7) } }
 | cexp SEMI seq_exp
     { { p = $startpos, $endpos ; e = ESeq ($1, $3) } }

env :
 | EOF
     { fun x -> x }
 | LET LLBRACK ID RRBRACK EQUALS seq_exp env
     { fun x ->
         { p = $startpos, $endpos ; e = ELet ("[[" ^ $3 ^ "]]", rename_env $6, $7 x) } }
 | LBRACE seq_exp RBRACE env
     { fun x -> { p = $startpos, $endpos ; e = ESeq (rename_env $2, $4 x) } }

prog :
 | seq_exp EOF { $1 }
%%
