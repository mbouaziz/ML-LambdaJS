open Prelude

type attr =
  | Value
  | Getter
  | Setter
  | Config
  | Writable
  | Enum

let string_of_attr attr = match attr with
  | Value -> "#value"
  | Getter -> "#getter"
  | Setter -> "#setter"
  | Config -> "#config"
  | Writable -> "#writable"
  | Enum -> "#enum"

(* Borrowed from prelude *)
module AttrOrderedType = struct
  type t = attr
  let compare = Pervasives.compare
end

module AttrMap = Map.Make (AttrOrderedType)

type op1 = [ `Op1Prefix of id | `Prim1 of string ]

type op2 = [ `Op2Infix of id | `Prim2 of string ]

type op3 = [ `Op3Prefix of id | `Prim3 of string ]

type ('op1, 'op2, 'op3) exp = { p : pos ; e : ('op1, 'op2, 'op3) a_exp }
and ('op1, 'op2, 'op3) a_exp =
  | EConst of JavaScript_syntax.const
  | EId of id
  | EObject of (string * ('op1, 'op2, 'op3) exp) list *
      (string * (attr * ('op1, 'op2, 'op3) exp) list) list
      (** object, field, new value, args object *)
  | EUpdateFieldSurface of ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
      (** object, field, args object *)
  | EGetFieldSurface of ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EAttr of attr * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | ESetAttr of attr * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EDeleteField of ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | ESet of id * ('op1, 'op2, 'op3) exp
  | EOp1 of 'op1 * ('op1, 'op2, 'op3) exp
  | EOp2 of 'op2 * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EOp3 of 'op3 * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EIf of ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EApp of ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp list
  | ESeq of ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | ELet of id * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EFix of id * ('op1, 'op2, 'op3) exp
  | ELabel of id * ('op1, 'op2, 'op3) exp
  | EBreak of id * ('op1, 'op2, 'op3) exp
  | ETryCatch of ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
      (** Catch block must be an [ELambda] *)
  | ETryFinally of ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EThrow of ('op1, 'op2, 'op3) exp
  | ELambda of id list * ('op1, 'op2, 'op3) exp


type src_exp = (op1, op2, op3) exp
type prim_exp = ([ `Prim1 of string], [ `Prim2 of string], [ `Prim3 of string]) exp


(******************************************************************************)

let substitute : 'a 'b 'c. ('a, 'b, 'c) exp -> ('a, 'b, 'c) exp -> ('a, 'b, 'c) exp -> ('a, 'b, 'c) exp = fun x y exp ->
  let rec subs exp =
    if exp = x then
      y
    else
      let e = match exp.e with
      | EConst _ as e -> e
      | EId _ as e -> e
      | EObject (attrs, fields) ->
	  let subs_attr (name, value) = (name, subs value) in
	  let subs_field (name, attrs) = (name, map subs_attr attrs) in
	  EObject (map subs_attr attrs, map subs_field fields)
      | EUpdateFieldSurface (o, e1, e2, args) -> EUpdateFieldSurface (subs o, subs e1, subs e2, subs args)
      | EGetFieldSurface (o, e, args) -> EGetFieldSurface (subs o, subs e, subs args)
      | EDeleteField (o, e) ->	EDeleteField (subs o, subs e)
      | EAttr (a, o, f) -> EAttr (a, subs o, subs f)
      | ESetAttr (a, o, f, v) -> ESetAttr (a, subs o, subs f, subs v)
      | EOp1 (o, e) -> EOp1 (o, subs e)
      | EOp2 (o, e1, e2) -> EOp2 (o, subs e1, subs e2)
      | EOp3 (o, e1, e2, e3) -> EOp3 (o, subs e1, subs e2, subs e3)
      | EIf (e1, e2, e3) -> EIf (subs e1, subs e2, subs e3)
      | EApp (f, args) -> EApp (subs f, map subs args)
      | ESeq (e1, e2) -> ESeq (subs e1, subs e2)
      | ESet (z, e) -> ESet (z, subs e)
      | ELet (z, e1, e2) -> ELet (z, subs e1, subs e2)
      | EFix (z, body) -> EFix (z, subs body)
      | ELabel (l, e) -> ELabel (l, subs e)
      | EBreak (l, e) -> EBreak (l, subs e)
      | ETryCatch (e1, e2) -> ETryCatch (subs e1, subs e2)
      | ETryFinally (e1, e2) -> ETryFinally (subs e1, subs e2)
      | EThrow e -> EThrow (subs e)
      | ELambda (args, body) -> ELambda (args, subs body)
      in
      { p = exp.p ; e }
  in subs exp


let rename (x : id) (y : id) exp =
  let rec ren exp =
    let e = match exp.e with
    | EConst _ as e -> e
    | EId z when z = x -> EId y
    | EId _ as e -> e
    | EObject (attrs, fields) -> 
	let ren_attr (name, value) = (name, ren value) in
	let ren_field (name, attrs) = (name, map ren_attr attrs) in
	  EObject (map ren_attr attrs, map ren_field fields)
    | EUpdateFieldSurface (o, e1, e2, args) -> EUpdateFieldSurface (ren o, ren e1, ren e2, ren args)
    | EGetFieldSurface (o, e, args) -> EGetFieldSurface (ren o, ren e, ren args)
    | EDeleteField (o, e) -> EDeleteField (ren o, ren e)
    | EAttr (a, o, f) -> EAttr (a, ren o, ren f)
    | ESetAttr (a, o, f, v) -> ESetAttr (a, ren o, ren f, ren v)
    | EOp1 (o, e) -> EOp1 (o, ren e)
    | EOp2 (o, e1, e2) -> EOp2 (o, ren e1, ren e2)
    | EOp3 (o, e1, e2, e3) -> EOp3 (o, ren e1, ren e2, ren e3)
    | EIf (e1, e2, e3) -> EIf (ren e1, ren e2, ren e3)
    | EApp (f, args) -> EApp (ren f, map ren args)
    | ESeq (e1, e2) -> ESeq (ren e1, ren e2)
    | ESet (z, e) when z = x -> ESet (y, ren e)
    | ESet (z, e) -> ESet (z, ren e)
    | ELet (z, e1, e2) when z = x -> ELet (z, ren e1, e2)
    | ELet (z, e1, e2) -> ELet (z, ren e1, ren e2)
    | EFix (z, _) as e when z = x -> e
    | EFix (z, body) -> EFix (z, ren body)
    | ELabel (l, e) -> ELabel (l, ren e)
    | EBreak (l, e) -> EBreak (l, ren e)
    | ETryCatch (e1, e2) -> ETryCatch (ren e1, ren e2)
    | ETryFinally (e1, e2) -> ETryFinally (ren e1, ren e2)
    | EThrow e -> EThrow (ren e)
    | ELambda (args, _) as e when List.mem x args -> e
    | ELambda (args, body) -> ELambda (args, ren body)
    in { p = exp.p ; e }
  in ren exp


let rec fv exp : IdSet.t = match exp.e with
  | EConst _ -> IdSet.empty
  | EId x -> IdSet.singleton x
  | EObject (attrs, fields) -> 
      let attr (name, value) = fv value in
      let field (name, attrs) = 
	IdSet.unions (map attr attrs) in
	IdSet.unions (List.append (map attr attrs) (map field fields))
  | EUpdateFieldSurface (o, e1, e2, args) -> IdSet.unions (map fv [o; e1; e2; args])
  | EGetFieldSurface (o, e, args) -> IdSet.unions (map fv [o; e; args])
  | EDeleteField (o, e) -> IdSet.union (fv o) (fv e)
  | EAttr (_, o, f) -> IdSet.unions (map fv [o; f])
  | ESetAttr (_, o, f, v) -> IdSet.unions (map fv [o; f; v])
  | EOp1 (_, e) -> fv e
  | EOp2 (_, e1, e2) -> IdSet.union (fv e1) (fv e2)
  | EOp3 (_, e1, e2, e3) -> IdSet.unions (map fv [e1; e2; e3])
  | EIf (e1, e2, e3) -> IdSet.unions (map fv [e1; e2; e3])
  | EApp (f, args) -> IdSet.unions (map fv (f :: args))
  | ESeq (e1, e2) -> IdSet.union (fv e1) (fv e2)
  | ESet (x, e) -> IdSet.union (fv e) (IdSet.singleton x)
  | ELet (x, e1, e2) -> IdSet.union (fv e1) (IdSet.remove x (fv e2))
  | EFix (x, body) -> IdSet.union (fv body) (IdSet.remove x (fv body))
  | ELabel (_, e) -> fv e
  | EBreak (_, e) -> fv e
  | ETryCatch (e1, e2) -> IdSet.union (fv e1) (fv e2)
  | ETryFinally (e1, e2) -> IdSet.union (fv e1) (fv e2)
  | EThrow e ->  fv e
  | ELambda (args, body) -> IdSet.diff (fv body) (IdSet.from_list args)
