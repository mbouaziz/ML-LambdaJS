open Prelude
open Es5_syntax
open JavaScript_syntax
open Es5_values

let undef = Const JavaScript_syntax.CUndefined

let str s = Const (JavaScript_syntax.CString s)

let num f = Const (JavaScript_syntax.CNum f)

let bool b = Const (JavaScript_syntax.CBool b)

let get_const v = match v with
  | Const c -> c
  | _ -> raise (Throw (str "expected primtive constant"))

let to_int v = match v with
  | Const (CInt n) -> n
  | Const (CNum x) -> int_of_float x
  | _ -> raise (Throw (str ("expected number, got " ^ pretty_value v)))

let to_float v = match v with
  | Const (CInt n) -> float_of_int n
  | Const (CNum x) -> x
  | _ -> raise (Throw (str ("expected number, got " ^ pretty_value v)))

let bool_neg v = match v with
  | Const (CBool b) -> Const (CBool (not b))
  | _ -> failwith "negation of non-boolean"

let typeof v = str begin match v with
  | Const c -> begin match c with
      | CUndefined -> "undefined"
      | CNull -> "null"
      | CString _ -> "string"
      | CNum _ -> "number"
      | CInt _ -> "number"
      | CBool _ -> "boolean"
      | CRegexp _ -> failwith "typeof CRegexp"
    end
  | ObjCell _ -> "object"
  | Closure _ -> "lambda"
  | Fail _ -> failwith "typeof Fail"
end

let surface_typeof v = str begin match v with
  | Const c -> begin match c with
      | CUndefined -> "undefined"
      | CNull -> "null"
      | CString _ -> "string"
      | CNum _ -> "number"
      | CInt _ -> "number"
      | CBool _ -> "boolean"
      | CRegexp _ -> failwith "surface_typeof regexp"
    end
  | ObjCell o -> let { code ;  _ } = !o in
    if code = None then "object" else "function"
  | _ -> raise (Throw (str "surface_typeof"))
end
  
let is_primitive v = match v with
  | Const _ -> Const (CBool true)
  | _ -> Const (CBool false)

let float_str n = 
  if n == nan then "NaN"
  else
    if n == infinity then "Infinity"
    else
      if float_of_int (int_of_float n) = n
      then string_of_int (int_of_float n) 
      else string_of_float n


let prim_to_str v = str begin match v with
  | Const c -> begin match c with
      | CUndefined -> "undefined"
      | CNull -> "null"
      | CString s -> s
      | CNum n -> float_str n
      | CInt n -> string_of_int n
      | CBool b -> string_of_bool b
      | CRegexp _ -> failwith "prim_to_str regexp"
    end
  | _ -> raise (Throw (str "prim_to_str"))
end

(* Section 9.3, excluding objects *)
let prim_to_num v = num begin match v with
  | Const c -> begin match c with
      | CUndefined -> nan 
      | CNull -> 0.0
      | CBool true -> 1.0
      | CBool false -> 0.0
      | CNum x -> x
      | CInt n -> float_of_int n
      | CString s -> begin try float_of_string s
        with Failure _ -> nan end
      | CRegexp _ -> failwith "prim_to_num regexp"
    end
  | _ -> raise (Throw (str "prim_to_str"))
end
  
let prim_to_bool v = bool begin match v with
  | Const c -> begin match c with
      | CBool b -> b
      | CUndefined -> false
      | CNull -> false
      | CNum x -> not (x == nan || x = 0.0 || x = -0.0)
      | CInt n -> not (n = 0)
      | CString s -> not (String.length s = 0)
      | CRegexp _ -> failwith "prim_to_bool regexp"
    end
  | _ -> true
end

let is_callable obj = bool begin match obj with
  | ObjCell o -> let { code ; _ } = !o in code <> None
  | _ -> false
end

let print v = match v with
  | Const (CString s) -> 
      printf "%S\n" s; Const CUndefined
  | _ -> failwith ("[interp] Print received non-string: " ^ pretty_value v)

let is_extensible obj = match obj with
  | ObjCell o -> let { extensible ; _ } = !o in bool extensible
  | _ -> raise (Throw (str "is-extensible"))

let prevent_extensions obj = match obj with
  | ObjCell o ->
      o := { !o with extensible = false };
      obj
  | _ -> raise (Throw (str "prevent-extensions"))
	  

let get_proto obj = match obj with
  | ObjCell o -> 
      let { proto ; _ } = !o in
      begin match proto with
      | None -> undef
      | Some p -> ObjCell p
      end
  | _ -> raise (Throw (str "get-proto"))


(* All the enumerable property names of an object *)
let rec get_property_names obj = match obj with
  | ObjCell o ->
      let protos = o::(all_protos o) in
      let folder o set =
	let { props ; _ } = !o in
	IdMap.fold (fun k v s -> 
		      if v.enum then IdSet.add k s else s) props set
      in
      let name_set = List.fold_right folder protos IdSet.empty in
      let name_list= IdSet.elements name_set in
      let prop_folder num name props = 
	IdMap.add (string_of_int num) 
	  (mk_data_prop (Const (CString name))) props in
      let name_props = List.fold_right2 prop_folder 
	(iota (List.length name_list))
	name_list
	IdMap.empty in
	ObjCell (ref (mk_obj name_props ))
  | _ -> raise (Throw (str "get-property-names"))

and all_protos cell = 
  let { proto ; _ } = !cell in
  match proto with
  | None -> []
  | Some c -> c::(all_protos c)

let get_own_property_names obj = match obj with
  | ObjCell o ->
      let { props ; _ } = !o in
      let add_name n x m = 
	IdMap.add (string_of_int x) (mk_data_prop (str n)) m in
      let namelist = IdMap.fold (fun k v l -> (k :: l)) props [] in
      let props = 
	List.fold_right2 add_name namelist (iota (List.length namelist)) IdMap.empty
      in
	ObjCell (ref (mk_obj props ))
  | _ -> raise (Throw (str "own-property-names"))

(* Implement this here because there's no need to expose the class
   property outside of the delta function *)
let object_to_string obj = match obj with
  | ObjCell o -> let { _class ; _ } = !o in str ("[object " ^ _class ^ "]")
  | _ -> raise (Throw (str "object-to-string, wasn't given object"))	

let is_array obj = match obj with
  | ObjCell o -> let { _class ; _ } = !o in bool (_class = "Array")
  | _ -> raise (Throw (str "is-array"))	


let to_int32 v = match v with
  | Const (CInt d) -> v
  | Const (CNum d) -> Const (CInt (int_of_float d))
  | _ -> raise (Throw (str "to-int"))

let fail v = match v with
  | Fail _ -> Const (CBool true)
  | _ -> Const (CBool false)

let op1 op = match op with
  | "bool!" -> bool_neg
  | "typeof" -> typeof
  | "surface-typeof" -> surface_typeof
  | "primitive?" -> is_primitive
  | "prim->str" -> prim_to_str
  | "prim->num" -> prim_to_num
  | "prim->bool" -> prim_to_bool
  | "is-callable" -> is_callable
  | "is-extensible" -> is_extensible
  | "prevent-extensions" -> prevent_extensions
  | "print" -> print
  | "get-proto" -> get_proto
  | "property-names" -> get_property_names
  | "own-property-names" -> get_own_property_names
  | "object-to-string" -> object_to_string
  | "is-array" -> is_array
  | "to-int32" -> to_int32
  | "fail?" -> fail
  | _ -> failwith ("no implementation of unary operator: " ^ op)

let arith i_op f_op v1 v2 = match v1, v2 with
  | Const (CInt m), Const (CInt n) -> Const (CInt (i_op m n))
  | Const (CNum x), Const (CNum y) -> Const (CNum (f_op x y))
  | Const (CNum x), Const (CInt n) -> Const (CNum (f_op x (float_of_int n)))
  | Const (CInt m), Const (CNum y) -> Const (CNum (f_op (float_of_int m) y))
  | _ -> raise (Throw (str "arithmetic operator"))

let arith_sum = arith (+) (+.)

let arith_sub = arith (-) (-.)

let arith_mul = arith ( * ) ( *. )

let arith_div x y = try arith (/) (/.) x y
with Division_by_zero -> Const (CNum infinity)

let arith_mod x y = try arith (mod) mod_float x y
with Division_by_zero -> Const (CNum nan)

let arith_lt x y = bool (to_float x < to_float y)

let arith_le x y = bool (to_float x <= to_float y)

let arith_gt x y = bool (to_float x > to_float y)

let arith_ge x y = bool (to_float x >= to_float y)

let bitwise_and v1 v2 = Const (CInt ((to_int v1) land (to_int v2)))

let bitwise_or v1 v2 = Const (CInt ((to_int v1) lor (to_int v2)))

let bitwise_xor v1 v2 = Const (CInt ((to_int v1) lxor (to_int v2)))

let bitwise_shiftl v1 v2 = Const (CInt ((to_int v1) lsl (to_int v2)))

let bitwise_zfshiftr v1 v2 = Const (CInt ((to_int v1) lsr (to_int v2)))

let bitwise_shiftr v1 v2 = Const (CInt ((to_int v1) asr (to_int v2)))

let string_plus v1 v2 = match v1, v2 with
  | Const (CString s1), Const (CString s2) ->
      Const (CString (s1 ^ s2))
  | _ -> raise (Throw (str "string concatenation"))

let stx_eq v1 v2 = bool begin match v1, v2 with
  | Const (CNum x1), Const (CInt x2) 
  | Const (CInt x2), Const (CNum x1) -> 
      float_of_int x2 = x1
  | Const c1, Const c2 -> c1 = c2 (* syntactic on primitives *)
  | ObjCell c1, ObjCell c2 -> c1 == c2 (* otherwise, pointer equality *)
  | Closure c1, Closure c2 -> c1 == c2
  | _ -> false
end

(* Algorithm 11.9.3, steps 1 through 19. Steps 20 and 21 are desugared to
   access the heap. *)
let abs_eq v1 v2 = bool begin
  let c1 = get_const v1 in
  let c2 = get_const v2 in
    if c1 = c2 then (* works correctly on floating point values *)
      true
    else match c1, c2 with
      | CNull, CUndefined
      | CUndefined, CNull -> true
      | CString s, CNum x
      | CNum x, CString s ->
          (try x = float_of_string s with Failure _ -> false)
      | CString s, CInt n
      | CInt n, CString s ->
          (try float_of_int n = float_of_string s with Failure _ -> false)
      | CNum x, CBool b
      | CBool b, CNum x -> x = if b then 1.0 else 0.0
      | CInt n, CBool b
      | CBool b, CInt n -> n = if b then 1 else 0
      | CNum x1, CInt x2
      | CInt x2, CNum x1 -> float_of_int x2 = x1
      | CBool b, CString s
      | CString s, CBool b -> (try float_of_string s = if b then 1.0 else 0.0 with Failure _ -> false)
      | _ -> false
end

let has_property obj field = match obj, field with
  | ObjCell o, Const (CString s) ->
      let rec obj_has_property o =
	let { proto ; props ; _ } = !o in
	if IdMap.mem s props then bool true
	else match proto with
	| Some c -> obj_has_property c
	| None -> bool false
      in
      obj_has_property o
  | _ -> bool false

let has_own_property obj field = match obj, field with
  | ObjCell o, Const (CString s) -> 
      let { props ; _ } = !o in
	bool (IdMap.mem s props)
  | _ -> raise (Throw (str "has-own-property?"))

let op2 op = match op with
  | "+" -> arith_sum
  | "-" -> arith_sub
  | "/" -> arith_div
  | "*" -> arith_mul
  | "%" -> arith_mod
  | "&" -> bitwise_and
  | "|" -> bitwise_or
  | "^" -> bitwise_xor
  | "<<" -> bitwise_shiftl
  | ">>" -> bitwise_shiftr
  | ">>>" -> bitwise_zfshiftr
  | "<" -> arith_lt
  | "<=" -> arith_le
  | ">" -> arith_gt
  | ">=" -> arith_ge
  | "stx=" -> stx_eq
  | "abs=" -> abs_eq
  | "has-property?" -> has_property
  | "has-own-property?" -> has_own_property
  | "string+" -> string_plus
  | _ -> failwith ("no implementation of binary operator: " ^ op)

let op3 op _ _ _ = match op with
  | _ -> failwith ("no ternary operators yet, so what's this: " ^ op)
