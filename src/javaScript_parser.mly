%{
(** A JavaScript parser that tries to do semicolon insertion. *)
open Prelude
open JavaScript_syntax

exception Expected_lvalue

exception Parse_failure of string

let rec expr_to_lvalue (e : expr) : lvalue =  match e with
  | VarExpr (p,x) -> VarLValue (p,x)
  | DotExpr (p,e,x) -> DotLValue (p,e,x)
  | BracketExpr (p,e1,e2) -> BracketLValue (p,e1,e2)
  | ParenExpr (_, e) -> expr_to_lvalue e
  | _ -> raise Expected_lvalue

let src_elt_function = function
  | ExprStmt (NamedFuncExpr (p, id, ids, s)) -> FuncStmt (p, id, ids, s)
  | ExprStmt (HintExpr (p0, h, NamedFuncExpr (p1, id, ids, s))) -> HintStmt (p0, h, FuncStmt (p1, id, ids, s))
  | x -> x

let forIn_init e =
  let no_somesome o = match o with
  | Some (_, Some _) -> assert false (* cannot happen, by construction *)
  | _ -> None
  in
  let if_somesome o f = match o with Some (x, Some y) -> let x, y = f x y in Some (x, Some y) | _ -> None in
  let mk_p (_, e1) (_, e2) = e1, e2 in
  let rec of_expr = function
    | ListExpr (p, e1, e2) -> if_somesome (of_expr e1) (fun (p1, id) e1 -> (p1, id), ListExpr (mk_p p1 p, e1, e2))
    | e -> of_assign_expr e
  and of_assign_expr = function
    | AssignExpr (p, op, e1, e2) -> if_somesome None (* pb here (of_lhs_expr e1) *) (fun (p1, id) e1 -> (p1, id), AssignExpr (mk_p p1 p, op, e1, e2))
    | e -> of_cond_expr e
  and of_cond_expr = function
    | IfExpr (p, e1, e2, e3) -> if_somesome (of_op_expr e1) (fun (p1, id) e1 -> (p1, id), IfExpr (mk_p p1 p, e1, e2, e3))
    | e -> of_op_expr e
  and of_op_expr = function
    | InfixExpr (p, op, e1, e2) ->
	begin match of_op_expr e1, op with
	| None, _ -> None
	| Some ((p1, id), None), OpIn -> Some ((p1, id), Some e2)
	| Some (_, None), _ -> None
	| Some ((p1, id), Some e1), _ -> Some ((p1, id), Some (InfixExpr (mk_p p1 p, op, e1, e2)))
	end
    | e -> of_unary_expr e
  and of_unary_expr = function
    | UnaryAssignExpr (_, PrefixInc, _)
    | UnaryAssignExpr (_, PrefixDec, _)
    | PrefixExpr _ -> None
    | e -> of_postfix_expr e
  and of_postfix_expr = function
    | UnaryAssignExpr (_, PostfixInc, e)
    | UnaryAssignExpr (_, PostfixDec, e) -> None
    | e -> of_lhs_expr e
  and of_lhs_expr = function
    | CallExpr (_, e1, _)
    | BracketExpr (_, e1, _)
    | DotExpr (_, e1, _) -> no_somesome (of_lhs_expr e1)
    | NewExpr _ -> None
    | e -> of_member_expr e
  and of_member_expr = function
    | NewExpr _
    | FuncExpr _
    | HintExpr _
    | NamedFuncExpr _ -> None
    | DotExpr (_, e, _)
    | BracketExpr (_, e, _) -> no_somesome (of_member_expr e)
    | e -> of_primary_expr e
  and of_primary_expr = function
    | ObjectExpr _
    | ConstExpr _
    | ArrayExpr _
    | ParenExpr _
    | HintExpr _
    | ThisExpr _ -> None
    | VarExpr (p, id) -> Some ((p, id), None)
    | _ -> assert false
  in of_expr e

let rec expr_noin = function
  | ThisExpr _
  | VarExpr _
  | ConstExpr _
  | ArrayExpr _
  | ObjectExpr _
  | ParenExpr _
  | DotExpr _
  | BracketExpr _
  | NewExpr _
  | FuncExpr _
  | NamedFuncExpr _
  | HintExpr _
  | CallExpr _
  | PrefixExpr _
  | UnaryAssignExpr _ -> true
  | InfixExpr (_, OpIn, _, _) -> false
  | InfixExpr (_, _, e1, e2)
  | ListExpr (_, e1, e2) -> (expr_noin e1) && (expr_noin e2)
  | IfExpr (_, e1, e2, e3) -> (expr_noin e1) && (expr_noin e2) && (expr_noin e3)
  | AssignExpr (_, _, _, e) -> expr_noin e
  | GetterExpr _
  | SetterExpr _ -> assert false

let varDecl_noin = function
  | VarDeclNoInit _ -> true
  | VarDecl (_, _, e) -> expr_noin e
let varDecls_noin = List.for_all varDecl_noin

%}

%token <string> TokContinueId
%token <string> TokBreakId
%token <string> TokId
%token <string> TokString
%token <string * bool * bool> TokRegexp
%token <int> TokInt
%token <float> TokFloat
%token <JavaScript_syntax.assignOp> TokAssignOp

%token <string> TokHINT

%token TokIf TokElse TokTrue TokFalse TokNew TokInstanceof TokThis TokNull TokFunction TokTypeof TokVoid
 TokDelete TokSwitch TokDefault TokCase TokWhile TokDo TokBreak TokVar TokIn TokFor TokTry TokCatch TokFinally TokThrow
 TokReturn TokWith TokContinue TokSet TokGet

%token TokLBrace TokRBrace TokLParen TokRParen TokAssign
 TokSemi TokComma TokQues TokColon TokLOr TokLAnd TokBOr TokBXor TokBAnd TokStrictEq TokAbstractEq
 TokStrictNEq TokAbstractNEq TokLShift TokRShift TokSpRShift TokLEq TokLT TokGEq TokGT TokPlusPlus TokMinusMinus
 TokPlus TokMinus TokTimes TokDiv TokMod TokExclamation TokTilde TokPeriod TokLBrack TokRBrack

%token TokEOF TokLineTerminator

(* http://stackoverflow.com/questions/1737460/
   how-to-find-shift-reduce-conflict-in-this-yacc-file *)
%nonassoc PrecLowerThanElse
%nonassoc TokElse

%left TokLOr
%left TokLAnd
%left TokBOr
%left TokBXor
%left TokBAnd
%left TokStrictEq TokStrictNEq TokAbstractEq TokAbstractNEq
%left TokLT TokLEq TokGT TokGEq TokIn TokInstanceof
%left TokLShift TokRShift TokSpRShift
%nonassoc PrecLowerThanInprefix
%left TokPlus TokMinus
%left TokTimes TokDiv TokMod

%nonassoc PrecLowerThanPostfix
%left TokPlusPlus TokMinusMinus

%nonassoc PrecLowerThanSemi
%nonassoc TokSemi

%start program
%start expression

%type <JavaScript_syntax.prog> program
%type <JavaScript_syntax.expr> expression

%%

%inline lT(X): x=X TokLineTerminator* {x}
%inline tAbstractEq: x=lT(TokAbstractEq) {x}
%inline tAbstractNEq: x=lT(TokAbstractNEq) {x}
%inline tAssign: x=lT(TokAssign) {x}
%inline tAssignOp: x=lT(TokAssignOp) {x}
%inline tBAnd: x=lT(TokBAnd) {x}
%inline tBOr: x=lT(TokBOr) {x}
%inline tBreak: x=lT(TokBreak) {x}
%inline tBreakId: x=lT(TokBreakId) {x}
%inline tBXor: x=lT(TokBXor) {x}
%inline tCase: x=lT(TokCase) {x}
%inline tCatch: x=lT(TokCatch) {x}
%inline tColon: x=lT(TokColon) {x}
%inline tComma: x=lT(TokComma) {x}
%inline tContinue: x=lT(TokContinue) {x}
%inline tContinueId: x=lT(TokContinueId) {x}
%inline tDefault: x=lT(TokDefault) {x}
%inline tDelete: x=lT(TokDelete) {x}
%inline tDiv: x=lT(TokDiv) {x}
%inline tDo: x=lT(TokDo) {x}
%inline tElse: x=lT(TokElse) {x}
%inline tExclamation: x=lT(TokExclamation) {x}
%inline tFalse: x=lT(TokFalse) {x}
%inline tFinally: x=lT(TokFinally) {x}
%inline tFloat: x=lT(TokFloat) {x}
%inline tFor: x=lT(TokFor) {x}
%inline tFunction: x=lT(TokFunction) {x}
%inline tGEq: x=lT(TokGEq) {x}
%inline tGet: x=lT(TokGet) {x}
%inline tGT: x=lT(TokGT) {x}
%inline tHINT: x=lT(TokHINT) {x}
%inline tId: x=lT(TokId) {x}
%inline tIf: x=lT(TokIf) {x}
%inline tIn: x=lT(TokIn) {x}
%inline tInstanceof: x=lT(TokInstanceof) {x}
%inline tInt: x=lT(TokInt) {x}
%inline tLAnd: x=lT(TokLAnd) {x}
%inline tLBrace: x=lT(TokLBrace) {x}
%inline tLBrack: x=lT(TokLBrack) {x}
%inline tLEq: x=lT(TokLEq) {x}
%inline tLOr: x=lT(TokLOr) {x}
%inline tLParen: x=lT(TokLParen) {x}
%inline tLShift: x=lT(TokLShift) {x}
%inline tLT: x=lT(TokLT) {x}
%inline tMinus: x=lT(TokMinus) {x}
%inline tMinusMinus: x=lT(TokMinusMinus) {x}
%inline tMod: x=lT(TokMod) {x}
%inline tNew: x=lT(TokNew) {x}
%inline tNull: x=lT(TokNull) {x}
%inline tPeriod: x=lT(TokPeriod) {x}
%inline tPlus: x=lT(TokPlus) {x}
%inline tPlusPlus: x=lT(TokPlusPlus) {x}
%inline tQues: x=lT(TokQues) {x}
%inline tRBrace: x=lT(TokRBrace) {x}
%inline tRBrack: x=lT(TokRBrack) {x}
%inline tRegexp: x=lT(TokRegexp) {x}
(* %inline tReturn: x=lT(TokReturn) {x} *)
%inline tRParen: x=lT(TokRParen) {x}
%inline tRShift: x=lT(TokRShift) {x}
%inline tSemi: x=lT(TokSemi) {x}
%inline tSet: x=lT(TokSet) {x}
%inline tSpRShift: x=lT(TokSpRShift) {x}
%inline tStrictEq: x=lT(TokStrictEq) {x}
%inline tStrictNEq: x=lT(TokStrictNEq) {x}
%inline tString: x=lT(TokString) {x}
%inline tSwitch: x=lT(TokSwitch) {x}
%inline tThis: x=lT(TokThis) {x}
(* %inline tThrow: x=lT(TokThrow) {x} *)
%inline tTilde: x=lT(TokTilde) {x}
%inline tTimes: x=lT(TokTimes) {x}
%inline tTrue: x=lT(TokTrue) {x}
%inline tTry: x=lT(TokTry) {x}
%inline tTypeof: x=lT(TokTypeof) {x}
%inline tVar: x=lT(TokVar) {x}
%inline tVoid: x=lT(TokVoid) {x}
%inline tWhile: x=lT(TokWhile) {x}
%inline tWith: x=lT(TokWith) {x}
(* %inline directSemi: x=TokSemi {x} *)


exprs: x=separated_list(tComma, assign_expr) {x}
cases: x=list(case) {x}
ids: x=separated_list(tComma, tId) {x}

(* How silly is this hack for dealing with get/set as keywords as well
as get/set property names? *)
prop:
  | id=tId { PropId id }
  | s=tString { PropString s }
  | tGet { PropString "get" }
  | tSet { PropString "set" }

%inline field:
  | p=prop tColon e=expr { (($startpos(p), $startpos(e)), p, e) }
  | tGet p=prop tLParen tRParen b=block { (($startpos, $endpos), p, GetterExpr (($startpos, $endpos), b)) }
  | tSet p=prop tLParen id=tId tRParen b=block { (($startpos, $endpos), p, SetterExpr (($startpos, $endpos), id, b)) }

%inline fields: x=separated_list(tComma, field) {x}

%inline varDecls: x=separated_nonempty_list(tComma, varDecl) {x}

element_list_rb:
  | tRBrack { [] }
  | e=assign_expr tRBrack { [e] }
  | e=assign_expr tComma el=element_list_rb { e::el }
  | c=tComma el=element_list_rb { (ConstExpr (($startpos(c), $endpos(c)), CUndefined))::el }

const:
  | tTrue { CBool true }
  | tFalse { CBool false }
  | tNull { CNull }
  | s=tString { CString s }
  | r=tRegexp { let re, g, ci = r in  CRegexp (re, g, ci) }
  | i=tInt { CInt i }
  | f=tFloat { CNum f }

primary_expr:
  | e=primary_expr_top { e }
  | tLBrace o=fields tRBrace { ObjectExpr (($startpos, $endpos), o) }
  | tLBrack a=element_list_rb { ArrayExpr (($startpos, $endpos), a) }
  | tLParen e=expr tRParen { ParenExpr (($startpos, $endpos), e) }

primary_expr_top:
  | c=const { ConstExpr (($startpos, $endpos), c) }
  | id=tId { VarExpr (($startpos, $endpos), id) }
  | h=tHINT e=primary_expr { HintExpr (($startpos, $endpos), h, e) }
  | tThis { ThisExpr (($startpos, $endpos)) }

function_expr:
  | tFunction tLParen ids=ids tRParen lb=tLBrace src=src_elts rb=tRBrace
    { FuncExpr (($startpos, $endpos), ids, 
                BlockStmt (($startpos(lb), $endpos(rb)), src)) }
  | tFunction tLParen ids=ids tRParen h=tHINT lb=tLBrace src=src_elts rb=tRBrace
      { HintExpr
          (($startpos(h), $endpos(h)), h,
           FuncExpr (($startpos, $endpos), ids,
                     BlockStmt (($startpos(lb), $endpos(rb)), src))) }
  | tFunction id=tId tLParen ids=ids tRParen lb=tLBrace src=src_elts rb=tRBrace
      { NamedFuncExpr (($startpos, $endpos), id, ids,
                       BlockStmt (($startpos(lb), $startpos(rb)), src)) }
  | tFunction id=tId tLParen ids=ids tRParen h=tHINT lb=tLBrace src=src_elts rb=tRBrace
      { HintExpr
          (($startpos(h), $endpos(h)), h,
           NamedFuncExpr (($startpos, $endpos), id, ids,
			  BlockStmt (($startpos(lb), $endpos(src)), src))) }

member_expr:
  | e=primary_expr { e }
  | e=function_expr { e }
  | e=member_expr tPeriod f=tId { DotExpr (($startpos, $endpos), e, f) }
  | e=member_expr tLBrack f=expr tRBrack { BracketExpr (($startpos, $endpos), e, f) }
  | tNew c=member_expr tLParen p=exprs tRParen { NewExpr (($startpos, $endpos), c, p) }

member_expr_top:
  | e=primary_expr_top { e }
  | e=function_expr { e }
  | e=member_expr_top tPeriod f=tId { DotExpr (($startpos, $endpos), e, f) }
  | e=member_expr_top tLBrack f=expr tRBrack { BracketExpr (($startpos, $endpos), e, f) }
  | tNew c=member_expr tLParen p=exprs tRParen { NewExpr (($startpos, $endpos), c, p) }

new_expr:
  | e=member_expr { e }
  | tNew e=new_expr { NewExpr (($startpos, $endpos), e, []) }

new_expr_top:
  | e=member_expr_top { e }
  | tNew e=new_expr { NewExpr (($startpos, $endpos), e, []) }


call_expr:
  | e1=member_expr tLParen e2=exprs tRParen { CallExpr (($startpos, $endpos), e1, e2) }
  | e1=call_expr tLParen e2=exprs tRParen { CallExpr (($startpos, $endpos), e1, e2) }
  | e1=call_expr tLBrack e2=expr tRBrack { BracketExpr (($startpos, $endpos), e1, e2) }
  | e=call_expr tPeriod id=tId { DotExpr (($startpos, $endpos), e, id) }

call_expr_top:
  | e1=member_expr_top tLParen e2=exprs tRParen { CallExpr (($startpos, $endpos), e1, e2) }
  | e1=call_expr_top tLParen e2=exprs tRParen { CallExpr (($startpos, $endpos), e1, e2) }
  | e1=call_expr_top tLBrack e2=expr tRBrack { BracketExpr (($startpos, $endpos), e1, e2) }
  | e=call_expr_top tPeriod id=tId { DotExpr (($startpos, $endpos), e, id) }

lhs_expr: e=new_expr | e=call_expr { e }

lhs_expr_top: e=new_expr_top | e=call_expr_top { e }

%inline postfix_op: tPlusPlus { PostfixInc } | tMinusMinus { PostfixDec }

postfix_expr:
  | e=lhs_expr %prec PrecLowerThanPostfix { e }
  | e=lhs_expr op=postfix_op { UnaryAssignExpr (($startpos, $endpos), op, expr_to_lvalue e) }

postfix_expr_top:
  | e=lhs_expr_top %prec PrecLowerThanPostfix { e }
  | e=lhs_expr_top op=postfix_op { UnaryAssignExpr (($startpos, $endpos), op, expr_to_lvalue e) }

%inline lprefix_op: tPlusPlus { PrefixInc } | tMinusMinus { PrefixDec }

%inline prefix_op:
  | tDelete { PrefixDelete }
  | tExclamation { PrefixLNot }
  | tMinus { PrefixMinus }
  | tPlus { PrefixPlus }
  | tTilde { PrefixBNot }
  | tTypeof { PrefixTypeof }
  | tVoid { PrefixVoid }

unary_expr:
  | e=postfix_expr { e }
  | op=lprefix_op e=unary_expr { UnaryAssignExpr (($startpos, $endpos), op, expr_to_lvalue e) }
  | op=prefix_op e=unary_expr { PrefixExpr (($startpos, $endpos), op, e) }

unary_expr_top:
  | e=postfix_expr_top { e }
  | op=lprefix_op e=unary_expr { UnaryAssignExpr (($startpos, $endpos), op, expr_to_lvalue e) }
  | op=prefix_op e=unary_expr { PrefixExpr (($startpos, $endpos), op, e) }

%inline infix_op:
  | tDiv { OpDiv }
  | tLShift { OpLShift }
  | tMinus { OpSub }
  | tMod { OpMod }
  | tPlus { OpAdd }
  | tRShift { OpZfRShift }
  | tSpRShift { OpSpRShift }
  | tTimes { OpMul }
  | tLT { OpLT }
  | tGT { OpGT }
  | tLEq { OpLEq }
  | tGEq { OpGEq }
  | tInstanceof { OpInstanceof }
  | tIn { OpIn }
  | tStrictEq { OpStrictEq }
  | tStrictNEq { OpStrictNEq }
  | tAbstractEq { OpEq }
  | tAbstractNEq { OpNEq }
  | tBAnd { OpBAnd }
  | tBXor { OpBXor }
  | tBOr { OpBOr }
  | tLAnd { OpLAnd }
  | tLOr { OpLOr }
(* Combines UnaryExpression, MultiplicativeExpression, AdditiveExpression, and
   ShiftExpression by using precedence and associativity rules. *)
op_expr:
  | e=unary_expr { e }
  | e1=op_expr op=infix_op e2=op_expr { InfixExpr (($startpos, $endpos), op, e1, e2) }

op_expr_top:
  | e=unary_expr_top { e }
  | e1=op_expr_top op=infix_op e2=op_expr { InfixExpr (($startpos, $endpos), op, e1, e2) }

cond_expr:
  | e=op_expr %prec PrecLowerThanInprefix { e }
  | e1=op_expr tQues e2=assign_expr tColon e3=assign_expr { IfExpr (($startpos, $endpos), e1, e2, e3) }

cond_expr_top:
  | e=op_expr_top %prec PrecLowerThanInprefix { e }
  | e1=op_expr_top tQues e2=assign_expr tColon e3=assign_expr { IfExpr (($startpos, $endpos), e1, e2, e3) }

assign_expr:
  | e=cond_expr { e }
  (* we need the use Assign (token for =) in other productions. *)
  | e1=lhs_expr op=tAssignOp e2=assign_expr { AssignExpr (($startpos, $endpos), op, expr_to_lvalue e1, e2) }
  | e1=lhs_expr tAssign e2=assign_expr { AssignExpr (($startpos, $endpos), OpAssign, expr_to_lvalue e1, e2) }

assign_expr_top:
  | e=cond_expr_top { e }
  (* we need the use Assign (token for =) in other productions. *)
  | e1=lhs_expr_top op=tAssignOp e2=assign_expr { AssignExpr (($startpos, $endpos), op, expr_to_lvalue e1, e2) }
  | e1=lhs_expr_top tAssign e2=assign_expr { AssignExpr (($startpos, $endpos), OpAssign, expr_to_lvalue e1, e2) }

expr:
  | e=assign_expr { e }
  | e1=expr tComma e2=assign_expr { ListExpr (($startpos, $endpos), e1, e2) }

expr_top:
  | e=assign_expr_top { e }
  | e1=expr_top tComma e2=assign_expr { ListExpr (($startpos, $endpos), e1, e2) }

%inline varDecl:
  | id=tId { VarDeclNoInit (($startpos, $endpos), id) }
  | id=tId tAssign e=assign_expr { VarDecl (($startpos, $endpos), id, e) }

case:
  | tCase e=expr tColon s=stmts 
      { CaseClause (($startpos, $endpos), e, BlockStmt (($startpos, $endpos), s)) }
  | tDefault tColon s=stmts
      { CaseDefault (($startpos, $endpos), BlockStmt (($startpos, $endpos), s)) }

for_clauses: tSemi e2=opt_expr tSemi e3=opt_expr { e2, e3 }

for_inpar:
  | v=tVar id=tId tIn e=expr { fun p s -> ForInStmt (p, VarForInInit (($startpos(v), $endpos(id)), id), e, s) }
  | tVar vd=varDecls tSemi e2=opt_expr tSemi e3=opt_expr {
      if varDecls_noin vd then
	fun p s -> ForStmt (p, VarForInit vd, e2, e3, s)
      else
	raise (Parse_failure "\"in\" in initializer of for(;;)")
    }
  | e1=expr f=for_clauses? {
      match f with
      | Some (e2, e3) ->
	  if expr_noin e1 then
	    fun p s -> ForStmt (p, ExprForInit e1, e2, e3, s)
	  else
	    raise (Parse_failure "\"in\" in initializer of for(;;)")
      | None ->
	  match forIn_init e1 with
	  | Some ((p0, id), Some e) ->
	      (fun p s -> ForInStmt (p, NoVarForInInit (p0, id), e, s))
	  | _ -> raise (Parse_failure "No \"in\" in initializer of for-in")
    }

catch: tCatch tLParen id=tId tRParen b=block { CatchClause (($startpos, $endpos), id, b) }

%inline block: s=delimited(tLBrace, stmts, tRBrace) { BlockStmt (($startpos, $endpos), s) }
%inline p_expr: e=delimited(tLParen, expr, tRParen) {e}
%inline paren_expr: e=p_expr { ParenExpr (($startpos, $endpos), e) }

opt_expr:
  | { ConstExpr (($startpos, $endpos), CUndefined) }
  | e=expr { e }

stmt_nosemi:
  | tContinue { ContinueStmt (($startpos, $endpos)) }
  | id=tContinueId { ContinueToStmt (($startpos, $endpos), id) }
  | tDo b=block tWhile e=paren_expr
      { DoWhileStmt (($startpos, $endpos), b, e) }
  | tBreak { BreakStmt (($startpos, $endpos)) }
  | id=tBreakId { BreakToStmt (($startpos, $endpos), id) }

stmt_semi:
  | e=expr_top { ExprStmt e }
  | TokThrow e=expr { ThrowStmt (($startpos, $endpos), e) }
  | TokReturn TokLineTerminator+ { ReturnStmt (($startpos, $endpos), ConstExpr (($endpos, $endpos), CUndefined)) }
  | TokReturn e=expr { ReturnStmt (($startpos, $endpos), e) }
  | tVar vd=varDecls { VarDeclStmt (($startpos, $endpos), vd) }
  | id=TokId tColon s=stmt { LabelledStmt (($startpos, $endpos), id, s) }

stmt_empty:  tSemi { EmptyStmt (($startpos, $endpos)) }

stmt0:
  | b=block { b }
  | tIf e=p_expr s=stmt %prec PrecLowerThanElse { IfSingleStmt (($startpos, $endpos), e, s) }
  | tIf e=p_expr s1=stmt tElse s2=stmt { IfStmt (($startpos, $endpos), e, s1, s2) }
  | tSwitch e=paren_expr tLBrace c=cases tRBrace { SwitchStmt (($startpos, $endpos), e, c) }
  | tWhile e=paren_expr s=stmt { WhileStmt (($startpos, $endpos), e, s) }
  | tFor tLParen f=for_inpar tRParen s=stmt { f ($startpos, $endpos) s }
  | tTry b=block c=catch* { TryStmt (($startpos, $endpos), b, c, EmptyStmt (($startpos, $endpos))) }
  | tTry b=block c=catch* tFinally f=block { TryStmt (($startpos, $endpos), b, c, f) }
  | tWith e=p_expr s=stmt { WithStmt (e, s) }
  | s=stmt_nosemi tSemi { s }
  | s=stmt_nosemi %prec PrecLowerThanSemi { s }

stmt:
  | s=stmt_empty
  | s=stmt0
  | s=stmt_semi tSemi
  | s=stmt_semi %prec PrecLowerThanSemi { s }

stmts_no_empty: (* do not start by tSemi *)
  | { [] }
  | s=stmt0 sl=stmts
  | s=stmt_semi tSemi sl=stmts
  | s=stmt_semi sl=stmts_no_empty { s::sl }

stmts:
  | tSemi sl=stmts
  | sl=stmts_no_empty { sl }

src_elts: sl=stmts { List.map src_elt_function sl }

program : TokLineTerminator* s=src_elts TokEOF { Prog (($startpos, $endpos), s) }

expression : TokLineTerminator* e=expr TokEOF { e }

%%
