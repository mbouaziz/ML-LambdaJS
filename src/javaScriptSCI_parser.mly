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

%token TokEOF TokLineTerminator TokNeverHappens

(* http://stackoverflow.com/questions/1737460/
   how-to-find-shift-reduce-conflict-in-this-yacc-file *)
%nonassoc TokGreaterThanColon
%nonassoc TokColon
%nonassoc TokLowerThanElse
%nonassoc TokElse

%left TokLOr
%left TokLAnd
%left TokBOr
%left TokBXor
%left TokBAnd
%left TokStrictEq TokStrictNEq TokAbstractEq TokAbstractNEq
%left TokLT TokLEq TokGT TokGEq TokIn TokInstanceof
%left TokLShift TokRShift TokSpRShift
%left TokPlus TokMinus
%left TokTimes TokDiv TokMod


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
%inline tReturn: x=lT(TokReturn) {x}
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
%inline tThrow: x=lT(TokThrow) {x}
%inline tTilde: x=lT(TokTilde) {x}
%inline tTimes: x=lT(TokTimes) {x}
%inline tTrue: x=lT(TokTrue) {x}
%inline tTry: x=lT(TokTry) {x}
%inline tTypeof: x=lT(TokTypeof) {x}
%inline tVar: x=lT(TokVar) {x}
%inline tVoid: x=lT(TokVoid) {x}
%inline tWhile: x=lT(TokWhile) {x}
%inline tWith: x=lT(TokWith) {x}
%inline directSemi: x=TokSemi {x}


exprs: x=separated_list(tComma, assign_expr) {x}
stmts: x=list(stmt) {x}
cases: x=list(case) {x}
catches: x=list(catch) {x}
ids: x=separated_list(tComma, tId) {x}

(* How silly is this hack for dealing with get/set as keywords as well
as get/set property names? *)
prop:
  | id=tId { PropId id }  %prec TokGreaterThanColon
  | s=tString { PropString s }
  | tGet { PropString "get" }
  | tSet { PropString "set" }

%inline ublock: s=delimited(tLBrace, stmt, tRBrace) {s}

%inline field:
  | p=prop tColon e=expr { (($startpos(p), $startpos(e)), p, e) }
  | tGet p=prop tLParen tRParen b=ublock { (($startpos, $endpos), p, GetterExpr (($startpos, $endpos), b)) }
  | tSet p=prop tLParen id=tId tRParen b=ublock { (($startpos, $endpos), p, SetterExpr (($startpos, $endpos), id, b)) }

%inline fields: x=separated_list(tComma, field) {x}

%inline varDecls: x=separated_nonempty_list(tComma, varDecl) {x}
%inline varDecls_noin: x=separated_nonempty_list(tComma, varDecl_noin) {x}

%inline elision_e: tComma { ConstExpr (($startpos, $endpos), CUndefined) }
%inline elision_opt: x=elision_e* {x}
%inline element_list_e: a=elision_opt e=assign_expr { a @ [e] }
%inline element_list: x=separated_nonempty_list(tComma, element_list_e) { List.flatten x }
%inline array_content:
  | a=elision_opt {a}
  | a=element_list {a}
  | a1=element_list tComma a2=elision_opt { a1 @ a2 }

const:
  | tTrue { CBool true }
  | tFalse { CBool false }
  | tNull { CNull }
  | s=tString { CString s }
  | r=tRegexp { let re, g, ci = r in  CRegexp (re, g, ci) }
  | i=tInt { CInt i }
  | f=tFloat { CNum f }

primary_expr:
  | c=const { ConstExpr (($startpos, $endpos), c) }
  | id=tId { VarExpr (($startpos, $endpos), id) }
  | tLBrack a=array_content tRBrack { ArrayExpr (($startpos, $endpos), a) }
  | tLBrace o=fields tRBrace { ObjectExpr (($startpos, $endpos), o) }
  | tLParen e=expr tRParen { ParenExpr (($startpos, $endpos), e) }
  | h=tHINT e=primary_expr { HintExpr (($startpos, $endpos), h, e) }
  | tThis { ThisExpr (($startpos, $endpos)) }

member_expr:
  | e=primary_expr { e }
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
  | e=member_expr tPeriod f=tId { DotExpr (($startpos, $endpos), e, f) }
  | e=member_expr tLBrack f=expr tRBrack { BracketExpr (($startpos, $endpos), e, f) }
  | tNew c=member_expr tLParen p=exprs tRParen { NewExpr (($startpos, $endpos), c, p) }

new_expr:
  | e=member_expr { e }
  | tNew e=new_expr { NewExpr (($startpos, $endpos), e, []) }


call_expr:
  | e1=member_expr tLParen e2=exprs tRParen { CallExpr (($startpos, $endpos), e1, e2) }
  | e1=call_expr tLParen e2=exprs tRParen { CallExpr (($startpos, $endpos), e1, e2) }
  | e1=call_expr tLBrack e2=expr tRBrack { BracketExpr (($startpos, $endpos), e1, e2) }
  | e=call_expr tPeriod id=tId { DotExpr (($startpos, $endpos), e, id) }

lhs_expr: e=new_expr | e=call_expr { e }

%inline postfix_op: tPlusPlus { PostfixInc } | tMinusMinus { PostfixDec }

postfix_expr:
  | e=lhs_expr { e }
  | e=lhs_expr op=postfix_op { UnaryAssignExpr (($startpos, $endpos), op, expr_to_lvalue e) }

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

%inline infix_op:
  | tDiv { OpDiv }
  | tLShift { OpLShift }
  | tMinus { OpSub }
  | tMod { OpMod }
  | tPlus { OpAdd }
  | tRShift { OpZfRShift }
  | tSpRShift { OpSpRShift }
  | tTimes { OpMul }
(* Combines UnaryExpression, MultiplicativeExpression, AdditiveExpression, and
   ShiftExpression by using precedence and associativity rules. *)
op_expr:
  | e=unary_expr { e }
  | e1=op_expr op=infix_op e2=op_expr
    { InfixExpr (($startpos, $endpos), op, e1, e2) }

(* ion stands for in or noin *)

%inline infix_ion(inop):
  | tLT { OpLT }
  | tGT { OpGT }
  | tLEq { OpLEq }
  | tGEq { OpGEq }
  | tInstanceof { OpInstanceof }
  | inop { OpIn }
  | tStrictEq { OpStrictEq }
  | tStrictNEq { OpStrictNEq }
  | tAbstractEq { OpEq }
  | tAbstractNEq { OpNEq }
  | tBAnd { OpBAnd }
  | tBXor { OpBXor }
  | tBOr { OpBOr }
  | tLAnd { OpLAnd }
  | tLOr { OpLOr }

ion_expr(inop):
  | e=op_expr { e }
  | e1=ion_expr(inop) op=infix_ion(inop) e2=ion_expr(inop)
    { InfixExpr (($startpos, $endpos), op, e1, e2) }

ion_cond_expr(inop):
  | e=ion_expr(inop) { e }
  | e1=ion_expr(inop) tQues e2=ion_assign_expr(inop) tColon e3=ion_assign_expr(inop)
      { IfExpr (($startpos, $endpos), e1, e2, e3) }


ion_assign_expr(inop):
  | e=ion_cond_expr(inop) { e }
  (* we need the use Assign (token for =) in other productions. *)
  | e1=lhs_expr op=tAssignOp e2=ion_assign_expr(inop)
    { AssignExpr (($startpos, $endpos), op, expr_to_lvalue e1, e2) }
  | e1=lhs_expr tAssign e2=ion_assign_expr(inop)
    { AssignExpr (($startpos, $endpos), OpAssign, expr_to_lvalue e1, e2) }

%inline assign_expr: e=ion_assign_expr(tIn) {e}

ion_expr_expr(inop):
  | e=ion_assign_expr(inop) { e }
  | e1=ion_expr_expr(inop) tComma e2=ion_assign_expr(inop) { ListExpr (($startpos, $endpos), e1, e2) }

%inline expr: e=ion_expr_expr(tIn) {e}
%inline expr_noin: e=ion_expr_expr(TokNeverHappens) {e}

%inline ion_varDecl(inop):
  | id=tId { VarDeclNoInit (($startpos, $endpos), id) }
  | id=tId tAssign e=ion_assign_expr(inop) { VarDecl (($startpos, $endpos), id, e) }

varDecl: x=ion_varDecl(tIn) {x}
varDecl_noin: x=ion_varDecl(TokNeverHappens) {x}

case:
  | tCase e=expr tColon s=stmts 
      { CaseClause (($startpos, $endpos), e, BlockStmt (($startpos, $endpos), s)) }
  | tDefault tColon s=stmts
      { CaseDefault (($startpos, $endpos), BlockStmt (($startpos, $endpos), s)) }

forInInit:
  | id=tId { NoVarForInInit (($startpos, $endpos), id) }
  | tVar id=tId { VarForInInit (($startpos, $endpos), id) }

forInit:
  | { NoForInit }
  | tVar vd=varDecls_noin { VarForInit vd }
  | e=expr_noin { ExprForInit e }

catch: tCatch tLParen id=tId tRParen b=block { CatchClause (($startpos, $endpos), id, b) }

%inline block: s=delimited(tLBrace, stmts, tRBrace) { BlockStmt (($startpos, $endpos), s) }
%inline p_expr: e=delimited(tLParen, expr, tRParen) {e}
%inline paren_expr: e=p_expr { ParenExpr (($startpos, $endpos), e) }

opt_expr:
  | { ConstExpr (($startpos, $endpos), CUndefined) }
  | e=expr { e }

stmt:
  | b=block { b }
  | tSemi { EmptyStmt (($startpos, $endpos)) }
  | e=expr tSemi { ExprStmt e }
  | tContinue tSemi { ContinueStmt (($startpos, $endpos)) }
  | id=tContinueId tSemi { ContinueToStmt (($startpos, $endpos), id) }
  | tIf e=p_expr s=stmt  %prec TokLowerThanElse { IfSingleStmt (($startpos, $endpos), e, s) }
  | tIf e=p_expr s1=stmt tElse s2=stmt { IfStmt (($startpos, $endpos), e, s1, s2) }

  | tSwitch e=paren_expr tLBrace c=cases tRBrace
      { SwitchStmt (($startpos, $endpos), e, c) }
  | tWhile e=paren_expr s=stmt
      { WhileStmt (($startpos, $endpos), e, s) }
  | tDo b=block tWhile e=paren_expr tSemi
      { DoWhileStmt (($startpos, $endpos), b, e) }
  | tBreak tSemi { BreakStmt (($startpos, $endpos)) }
  | id=tBreakId tSemi { BreakToStmt (($startpos, $endpos), id) }
  | id=tId tColon s=stmt { LabelledStmt (($startpos, $endpos), id, s) }
  | tFor tLParen e1=forInInit tIn e2=expr tRParen s=stmt { ForInStmt (($startpos, $endpos), e1, e2, s) }
  | tFor tLParen e1=forInit tSemi e2=opt_expr tSemi e3=opt_expr tRParen s=stmt { ForStmt (($startpos, $endpos), e1, e2, e3, s) }
  | tTry b=block c=catches { TryStmt (($startpos, $endpos), b, c, EmptyStmt (($startpos, $endpos))) }
  | tTry b=block c=catches tFinally f=block { TryStmt (($startpos, $endpos), b, c, f) }
  | tThrow e=expr tSemi { ThrowStmt (($startpos, $endpos), e) }
  | tReturn e=opt_expr tSemi { ReturnStmt (($startpos, $endpos), e) }
  | tVar vd=varDecls tSemi { VarDeclStmt (($startpos, $endpos), vd) }
  | tWith e=p_expr s=stmt { WithStmt (e, s) }

src_elt_block: x=delimited(tLBrace, src_elts, tRBrace) { BlockStmt (($startpos, $endpos), x) }
 
src_elts: x=src_elt* {x}

src_elt:
  | s=stmt { s }
  | tFunction id=tId tLParen ids=ids tRParen s=src_elt_block
    { FuncStmt (($startpos, $endpos), id, ids, s) } 
  | tFunction id=tId tLParen ids=ids tRParen h=tHINT s=src_elt_block
    { HintStmt (($startpos(h), $endpos(h)), h,
                FuncStmt (($startpos, $endpos), id, ids, s)) } 

program : TokLineTerminator* s=src_elts TokEOF { Prog (($startpos, $endpos), s) }

expression : TokLineTerminator* e=expr TokEOF { e }

%%
