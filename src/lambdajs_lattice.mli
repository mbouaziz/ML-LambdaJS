open Prelude
open Lambdajs_cps
open Format

type loc = 
  | Loc of int
  | LocField of int * string

module Loc : sig
  type t = loc
  val compare : t -> t -> int
  val pp : t -> Format.printer
end


module AV : sig
  type t = 
    | ANumber
    | ABool
    | AString
    | AConst of JavaScript_syntax.const
    | ARef of loc
    | AObj of loc IdMap.t
    | AClosure of int * id list * cpsexp
  
  val compare : t -> t -> int

  val pp : t -> printer
end

module RT : sig
  type t =
    | Number
    | String
    | Boolean
    | Function
    | Object
    | Undefined

  val compare : t -> t -> int
end

module RTSet : Set.S with type elt = RT.t

module AVSet : Set.S with type elt = AV.t

module Heap : Map.S with type key = Loc.t

type heap

module Type : sig

  type t =
    | Set of AVSet.t
    | LocTypeof of Loc.t
    | LocTypeIs of Loc.t * RTSet.t
    | Deref of Loc.t

  val compare : t -> t -> int
  val up : heap -> t -> AVSet.t
  val union : heap -> t -> t -> t
  val pp : t -> printer

end

type env

val singleton : AV.t -> Type.t

val empty : Type.t

val union_env : heap -> env -> env -> env

val lookup : id -> env -> Type.t

val bind : id -> Type.t -> env -> env

val p_env : env -> Format.printer

val p_heap : heap -> Format.printer

val empty_env : env

val deref : Loc.t -> heap -> AVSet.t

val set_ref : Loc.t -> AVSet.t -> heap -> heap

val union_heap : heap -> heap -> heap

val empty_heap : heap

val compare_heap : heap -> heap -> int

val compare_env : env -> env -> int

val locations : AVSet.t -> Loc.t list
