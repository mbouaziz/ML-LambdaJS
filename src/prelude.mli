type id = string

type pos = Lexing.position * Lexing.position

val dummy_pos : Lexing.position * Lexing.position

module IdOrderedType : sig
  type t = id
  val compare : t -> t -> int
end

module Pos : sig
  type t = pos
  val compare : t -> t -> int
  val before : t -> t -> bool
end


module Format :
sig
  include module type of Format

  type printer = formatter -> unit
  val nest : printer -> printer
  val sep : printer list -> printer
  val squish : printer list -> printer
  val vert : printer list -> printer
  val horz : printer list -> printer
  val text : string -> printer
  val int : int -> printer
  val enclose : string -> string -> printer -> printer
  val parens : printer -> printer
  val braces : printer -> printer
  val brackets : printer -> printer
  val angles : printer -> printer
    (** [to_string f x] uses [Format.str_formatter] as the buffer for printing [x]
	with [f]. *)
  val to_string : ('a -> printer) -> 'a -> string
end

module Set : sig
  open Format
  module type OrderedType = Set.OrderedType
  module type S =
  sig
    include Set.S
      
    val unions : t list -> t
    val from_list : elt list -> t
    val to_list : t -> elt list
    val p_set : (elt -> printer) -> t -> printer
    val fold_i : (int -> elt -> 'a -> 'a) -> t -> 'a -> 'a
  end
  module Make (Ord : OrderedType) : S with type elt = Ord.t
end

module Map :
sig
  open Format
  module type OrderedType = Map.OrderedType
  module type S =
  sig
    include Map.S

    val add_opt : key -> 'a option -> 'a t -> 'a t
    val find_opt : key -> 'a t -> 'a option
    val mem_binding : key -> 'a -> 'a t -> bool
    val from_list : (key * 'a) list -> 'a t
    val to_list : 'a t -> (key * 'a) list
    val keys : 'a t -> key list
    val values : 'a t -> 'a list
    val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val join : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val p_map : (key -> printer) -> ('a -> printer) -> 'a t -> printer
    val diff : 'a t -> 'a t -> 'a t
    val fold_i : (int -> key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end
  module Make (Ord : OrderedType) : S with type key = Ord.t
end

module IntSet : Set.S
  with type elt = int

module IdSet : Set.S 
  with type elt = id

module PosSet : Set.S 
  with type elt = pos

module PosMap : Map.S
  with type key = pos

module IdMap : Map.S
  with type key = id

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val map : ('a -> 'b) -> 'a list -> 'b list

val second2 : ('b -> 'c) -> 'a * 'b -> 'a * 'c

val third3 : ('c -> 'd) -> 'a * 'b * 'c -> 'a * 'b * 'd

val string_of_position : pos -> string

val snd3 : 'a * 'b * 'c -> 'b

val snd2 : 'a * 'b -> 'b

val fst2 : 'a * 'b -> 'a

val fst3 : 'a * 'b * 'c -> 'a

val thd3 : 'a * 'b * 'c -> 'c

val printf : ('a, out_channel, unit) format -> 'a

val eprintf : ('a, out_channel, unit) format -> 'a

val sprintf : ('a, unit, string) format -> 'a

val intersperse : 'a -> 'a list -> 'a list

val take_while : ('a -> bool) -> 'a list -> 'a list * 'a list

val match_while : ( 'a -> 'b option) -> 'a list -> 'b list * 'a list

(** [nub lst] removes duplicates from the [lst]. Duplicates are identified by
    structural equality. *)
val nub : 'a list -> 'a list

(** [iota n] returns the list of integers [0] through [n-1], inclusive. *)
val iota : int -> int list
