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

type ('op1, 'op2, 'op3) exp =
  | EConst of pos * JavaScript_syntax.const
  | EId of pos * id
  | EObject of pos * (string * ('op1, 'op2, 'op3) exp) list *
      (string * (attr * ('op1, 'op2, 'op3) exp) list) list
      (** object, field, new value, args object *)
  | EUpdateFieldSurface of pos * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
      (** object, field, args object *)
  | EGetFieldSurface of pos * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EAttr of pos * attr * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | ESetAttr of pos * attr * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EDeleteField of pos * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | ESet of pos * id * ('op1, 'op2, 'op3) exp
  | EOp1 of pos * 'op1 * ('op1, 'op2, 'op3) exp
  | EOp2 of pos * 'op2 * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EOp3 of pos * 'op3 * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EIf of pos * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EApp of pos * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp list
  | ESeq of pos * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | ELet of pos * id * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EFix of pos * id * ('op1, 'op2, 'op3) exp
  | ELabel of pos * id * ('op1, 'op2, 'op3) exp
  | EBreak of pos * id * ('op1, 'op2, 'op3) exp
  | ETryCatch of pos * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
      (** Catch block must be an [ELambda] *)
  | ETryFinally of pos * ('op1, 'op2, 'op3) exp * ('op1, 'op2, 'op3) exp
  | EThrow of pos * ('op1, 'op2, 'op3) exp
  | ELambda of pos * id list * ('op1, 'op2, 'op3) exp


type src_exp = (op1, op2, op3) exp
type prim_exp = ([ `Prim1 of string], [ `Prim2 of string], [ `Prim3 of string]) exp


(******************************************************************************)

let substitute : 'a 'b 'c. id -> ('a, 'b, 'c) exp -> ('a, 'b, 'c) exp -> ('a, 'b, 'c) exp = fun x y exp ->
  let rec subs exp = match exp with
    | EConst _ -> exp
    | EId (_, z) when z = x -> y
    | EId _ -> exp
    | EObject (p, attrs, fields) ->
	let subs_attr (name, value) = (name, subs value) in
	let subs_field (name, attrs) = (name, map subs_attr attrs) in
	  EObject (p, map subs_attr attrs, map subs_field fields)
    | EUpdateFieldSurface (p, o, e1, e2, args) -> EUpdateFieldSurface (p, subs o, subs e1, subs e2, subs args)
    | EGetFieldSurface (p, o, e, args) -> EGetFieldSurface (p, subs o, subs e, subs args)
    | EDeleteField (p, o, e) ->	EDeleteField (p, subs o, subs e)
    | EAttr (p, a, o, f) -> EAttr (p, a, subs o, subs f)
    | ESetAttr (p, a, o, f, v) -> ESetAttr (p, a, subs o, subs f, subs v)
    | EOp1 (p, o, e) -> EOp1 (p, o, subs e)
    | EOp2 (p, o, e1, e2) -> EOp2 (p, o, subs e1, subs e2)
    | EOp3 (p, o, e1, e2, e3) -> EOp3 (p, o, subs e1, subs e2, subs e3)
    | EIf (p, e1, e2, e3) -> EIf (p, subs e1, subs e2, subs e3)
    | EApp (p, f, args) -> EApp (p, subs f, map subs args)
    | ESeq (p, e1, e2) -> ESeq (p, subs e1, subs e2)
    | ESet (_, z, _) when z = x -> failwith "subs"
    | ESet (p, z, e) -> ESet (p, z, subs e)
    | ELet (_, z, _, _) when z = x -> failwith "subs"
    | ELet (p, z, e1, e2) -> ELet (p, z, subs e1, subs e2)
    | EFix (_, z, _) when z = x -> failwith "subs"
    | EFix (p, z, body) -> EFix (p, z, subs body)
    | ELabel (p, l, e) -> ELabel (p, l, subs e)
    | EBreak (p, l, e) -> EBreak (p, l, subs e)
    | ETryCatch (p, e1, e2) -> ETryCatch (p, subs e1, subs e2)
    | ETryFinally (p, e1, e2) -> ETryFinally (p, subs e1, subs e2)
    | EThrow (p, e) -> EThrow (p, subs e)
    | ELambda (_, args, _) when List.mem x args -> failwith "subs"
    | ELambda (p, args, body) -> ELambda (p, args, subs body)
  in subs exp


let rename (x : id) (y : id) exp =
  let rec ren exp = match exp with
    | EConst _ -> exp
    | EId (p, z) -> EId (p, if z = x then y else z)
    | EObject (p, attrs, fields) -> 
	let ren_attr (name, value) = (name, ren value) in
	let ren_field (name, attrs) = (name, map ren_attr attrs) in
	  EObject (p, map ren_attr attrs, map ren_field fields)
    | EUpdateFieldSurface (p, o, e1, e2, args) ->
	EUpdateFieldSurface (p, ren o, ren e1, ren e2, ren args)
    | EGetFieldSurface (p, o, e, args) ->
	EGetFieldSurface (p, ren o, ren e, ren args)
    | EDeleteField (p, o, e) ->
	EDeleteField (p, ren o, ren e)
    | EAttr (p, a, o, f) ->
	EAttr (p, a, ren o, ren f)
    | ESetAttr (p, a, o, f, v) ->
	ESetAttr (p, a, ren o, ren f, ren v)
    | EOp1 (p, o, e) -> EOp1 (p, o, ren e)
    | EOp2 (p, o, e1, e2) -> EOp2 (p, o, ren e1, ren e2)
    | EOp3 (p, o, e1, e2, e3) -> EOp3 (p, o, ren e1, ren e2, ren e3)
    | EIf (p, e1, e2, e3) -> EIf (p, ren e1, ren e2, ren e3)
    | EApp (p, f, args) -> EApp (p, ren f, map ren args)
    | ESeq (p, e1, e2) -> ESeq (p, ren e1, ren e2)
    | ESet (p, z, e) -> 
	if x = z then ESet (p, y, ren e) else ESet (p, z, ren e)
    | ELet (p, z, e1, e2) -> 
        ELet (p, z, ren e1, if x = z then e2 else ren e2)
    | EFix (p, z, body) ->
        if z = x then exp
        else EFix (p, z, ren body)
    | ELabel (p, l, e) -> ELabel (p, l, ren e)
    | EBreak (p, l, e) -> EBreak (p, l, ren e)
    | ETryCatch (p, e1, e2) -> ETryCatch (p, ren e1, ren e2)
    | ETryFinally (p, e1, e2) -> ETryFinally (p, ren e1, ren e2)
    | EThrow (p, e) -> EThrow (p, ren e)
    | ELambda (p, args, body) ->
        if List.mem x args then exp
        else ELambda (p, args, ren body)
  in ren exp


let rec fv exp : IdSet.t = match exp with
  | EConst _ -> IdSet.empty
  | EId (_, x) -> IdSet.singleton x
  | EObject (_, attrs, fields) -> 
      let attr (name, value) = fv value in
      let field (name, attrs) = 
	IdSet.unions (map attr attrs) in
	IdSet.unions (List.append (map attr attrs) (map field fields))
  | EUpdateFieldSurface (_, o, e1, e2, args) -> 
      IdSet.unions (map fv [o; e1; e2; args])
  | EGetFieldSurface (_, o, e, args) ->
      IdSet.unions (map fv [o; e; args])
  | EDeleteField (_, o, e) -> IdSet.union (fv o) (fv e)
  | EAttr (_, _, o, f) ->
      IdSet.unions (map fv [o; f])
  | ESetAttr (_, _, o, f, v) ->
      IdSet.unions (map fv [o; f; v])
  | EOp1 (_, _, e) -> fv e
  | EOp2 (_, _, e1, e2) -> IdSet.union (fv e1) (fv e2)
  | EOp3 (_, _, e1, e2, e3) -> IdSet.unions (map fv [e1; e2; e3])
  | EIf (_, e1, e2, e3) -> IdSet.unions (map fv [e1; e2; e3])
  | EApp (_, f, args) -> IdSet.unions (map fv (f :: args))
  | ESeq (_, e1, e2) -> IdSet.union (fv e1) (fv e2)
  | ESet (_, x, e) -> IdSet.union (fv e) (IdSet.singleton x)
  | ELet (_, x, e1, e2) -> IdSet.union (fv e1) (IdSet.remove x (fv e2))
  | EFix (_, x, body) ->
      IdSet.union (fv body) (IdSet.remove x (fv body))
  | ELabel (_, _, e) -> fv e
  | EBreak (_, _, e) -> fv e
  | ETryCatch (_, e1, e2) -> IdSet.union (fv e1) (fv e2)
  | ETryFinally (_, e1, e2) -> IdSet.union (fv e1) (fv e2)
  | EThrow (_, e) ->  fv e
  | ELambda (_, args, body) -> IdSet.diff (fv body) (IdSet.from_list args)
