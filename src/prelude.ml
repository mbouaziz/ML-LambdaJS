open Lexing

type id = string

(** We track the start and end position of each syntactic form. *)
type pos = Lexing.position * Lexing.position 

let dummy_pos = (Lexing.dummy_pos, Lexing.dummy_pos)

module IdOrderedType = struct
  type t = id
  let compare = Pervasives.compare
end

module Pos = struct

  type t = pos

  let compare = Pervasives.compare

  let before (_, p1_end) (p2_start, _) = 
    p1_end.pos_cnum < p2_start.pos_cnum
end

module Int = struct
  type t = int
  let compare = Pervasives.compare
end

module Format = struct
  include Format

  type printer = formatter -> unit
    
  let nest (p : printer) (fmt : formatter) : unit =
    pp_open_vbox fmt 2;
    p fmt;
    pp_close_box fmt ()
      
  let rec sep (lst : printer list) (fmt : formatter) : unit = match lst with
    x1 :: x2 :: xs' ->
      pp_open_box fmt 2;
      x1 fmt; 
      pp_close_box fmt ();
      pp_print_space fmt (); 
      sep (x2 :: xs') fmt
  | [x] -> 
      pp_open_box fmt 2;
      x fmt;
      pp_close_box fmt ()
  | [] -> ()

  let rec squish (lst : printer list) (fmt : formatter) : unit = match lst with
  | x :: xs -> x fmt; squish xs fmt
  | [] -> ()
      
      
  let vert (p : printer list) (fmt : formatter) : unit = 
    pp_open_vbox fmt 0;
    sep p fmt;
    pp_close_box fmt ()
      
  let horz (p : printer list) (fmt : formatter) : unit = 
    pp_open_hbox fmt ();
    sep p fmt;
    pp_close_box fmt ()
      
  let text s fmt = pp_print_string fmt s
    
  let int n fmt = pp_print_int fmt n
    
  let enclose l r (inner : printer) (fmt : formatter) = 
    pp_open_box fmt 2;
    pp_print_string fmt l;
    inner fmt;
    pp_print_string fmt r;
    pp_close_box fmt ()
      
  let parens = enclose "(" ")"
    
  let braces = enclose "{" "}"
    
  let brackets = enclose "[" "]"

  let angles = enclose "<" ">"

  let to_string (f : 'a -> printer) (x : 'a) : string  =
    f x str_formatter;
    flush_str_formatter ()
end

module Set =
struct
  open Format

  module type OrderedType = Set.OrderedType

  module type S =
  sig
    include Set.S

    val unions : t list -> t
    val from_list : elt list -> t
    val to_list : t -> elt list
    val p_set : (elt -> printer) -> t -> printer
  end

  module Make (Ord : OrderedType) : S with type elt = Ord.t =
  struct
    include Set.Make(Ord)

    let unions lst = List.fold_left union empty lst

    let from_list lst = 
      List.fold_left (fun set x -> add x set) empty lst

    let to_list set =
      fold (fun e lst -> e :: lst) set []    

    let p_set p_elt set = 
      braces (horz (List.map p_elt (to_list set)))
  end
end

module Map =
struct
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
  end

  module Make (Ord : OrderedType) : S with type key = Ord.t =
  struct
    include Map.Make(Ord)

    let add_opt k = function
    | Some v -> add k v
    | None -> fun m -> m

    let find_opt k m = try Some (find k m) with Not_found -> None

    let mem_binding k v m = match find_opt k m with
    | Some v' -> v = v'
    | _ -> false

    let from_list lst = 
      List.fold_left (fun m (k, v) -> add k v m) empty lst

    let to_list m = 
      fold (fun k v lst -> (k, v) :: lst) m []

    let keys m =
      fold (fun k _ lst -> k :: lst) m []

    let values m =
      fold (fun _ v lst -> v :: lst) m []

    let union f m1 m2 = 
      let rec g (k1, v1) (k2, v2) =
	if Ord.compare k1 k2 = 0 then (k1, f v1 v2)
	else raise Not_found
      in from_list (List.map2 g (to_list m1) (to_list m2))

    let join f m1 m2 =
      let mk k v acc = 
	if mem k acc then 
          add k (f k v (find k acc)) acc (* f m1-val  m2-val *)
	else 
          add k v acc
      in fold mk m1 m2 (* m2 is the accumulator *)

    let p_map p_key p_val t = 
      vert (List.map (fun (k, v) -> brackets (horz [ p_key k; p_val v ]))
              (to_list t))

    let diff m1 m2 = 
      let fn key v acc =
	if mem key m2 then acc else add key v acc in
      fold fn m1 empty
  end
end

module IntSet = Set.Make (Int)
module IdSet = Set.Make (IdOrderedType)
module PosSet = Set.Make (Pos)
module PosMap = Map.Make (Pos)
module IdMap = Map.Make (IdOrderedType)

let fold_left = List.fold_left

let fold_right = List.fold_right

let map = List.map

let printf = Printf.printf

let eprintf = Printf.eprintf

let sprintf = Printf.sprintf

let second2 f (a, b) = (a, f b)

let third3 f (a, b, c) = (a, b, f c)

let string_of_position (p, e) = 
  Format.sprintf "%s:%d:%d-%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
    (e.pos_cnum - e.pos_bol)

let snd3 (a, b, c) = b

let snd2 (a, b) = b

let fst2 (a, b) = a

let fst3 (a, _, _) = a

let thd3 (_, _, c) = c

let rec intersperse a lst = match lst with
    [] -> []
  | [x] -> [x]
  | x :: xs -> x :: a :: (intersperse a xs)

let rec take_while f xs = match xs with
    [] -> [], []
  | x :: xs' -> 
      if f x then
        let lhs, rhs = take_while f xs' in
          x :: lhs, rhs
      else
        [], xs

let rec match_while f xs = match xs with
    [] -> [], []
  | x :: xs' -> begin match f x with
        Some y ->
          let ys, xs'' = match_while f xs' in
            y :: ys, xs''
      | None -> [], xs
    end



let rec rem (elt : 'a) (lst : 'a list) : 'a list = match lst with
    [] -> []
  | x :: xs -> if elt = x then rem elt xs else x :: (rem elt xs)

let rec nub (lst : 'a list) : 'a list = match lst with
    [] -> []
  | x :: xs -> x :: (nub (rem x xs))

let rec iota' m lst = 
  if m < 0 then lst
  else iota' (m - 1) (m :: lst)

let iota n = iota' (n - 1) []
