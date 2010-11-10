open Prelude
open Es5_syntax
open Exprjs_syntax

module S = JavaScript_syntax

let true_c p = { p ; e = EConst (S.CBool true) }
let false_c p = { p ; e = EConst (S.CBool false) }

let undef_c p = { p ; e = EConst S.CUndefined }

let str p s = { p ; e = EConst (S.CString s) }

let num_c p d = { p ; e = EConst (S.CNum d) }

let int_c p d = { p ; e = EConst (S.CInt d) }

 (* WHY is are there a difference here? *)
let obj_proto ?(from_parser=false) p = { p ; e = EId (if from_parser then "Object_prototype" else "[[Object_prototype]]") }
let fun_proto ?(from_parser=false) p = { p ; e = EId (if from_parser then "Function_prototype" else "[[Function_prototype]]") }

let to_object p e = { p ; e = EApp ({ p ; e = EId "[[ToObject]]" }, [e]) }
let to_string p e = { p ; e = EApp ({ p ; e = EId "[[ToString]]" }, [e]) }

let rec mk_val p v =
  [(Value, v);
   (Enum, true_c p);
   (Config, true_c p);
   (Writable, true_c p)]

let mk_field (p, s, e) =
  match e with
    | _ -> (s, mk_val p e)

let mk_array (p, exps) =
  let mk_num_field n v = (string_of_int n, mk_val p v) in
  { p ; e = EObject ([("proto", { p ; e = EId "[[Array_prototype]]" });
		      ("extensible", true_c p);
		      ("class", str p "Array")],
		     ((mk_field (p, "length", int_c p (List.length exps)))
		      :: List.map2 mk_num_field (iota (List.length exps)) exps)) }

(* 10.6 *)
let args_obj ?(from_parser=false) p arg_list =
  let mk_field n v = (string_of_int n,  mk_val p v) in
  { p ; e = EObject 
      (* 10.6 steps 4, 6 *)
      ([("proto", obj_proto ~from_parser p);
	("class", str p "Arguments");
	("extensible", false_c p)],
       (* 10.6 steps 1, 7 *)
       (("length", [(Value, int_c p (List.length arg_list));
		    (Writable, true_c p);
		    (Enum, false_c p);
		    (Config, true_c p)])
	  (* 10.6 step 13a *)
	::("callee", [(Getter, { p ; e = EId "[[ThrowTypeError]]" });
		      (Setter, { p ; e = EId "[[ThrowTypeError]]" });
		      (Enum, false_c p);
		      (Config, false_c p)])
	::("caller", [(Getter, { p ; e = EId "[[ThrowTypeError]]" });
		      (Setter, { p ; e = EId "[[ThrowTypeError]]" });
		      (Enum, false_c p);
		      (Config, false_c p)])
	:: (List.map2 mk_field (iota (List.length arg_list)) arg_list))) }


(* Used by getters and setters---the function will be known at
runtime *)
let args_thunk ?(from_parser=false) p arg_list =
  { p ; e = ELambda (["func"], args_obj ~from_parser p arg_list) }


(* Same idea as in original \JS --- use the args array *)
let func_expr_lambda ?(from_parser=false) p ids body =
  let folder id ix e =
    { p ; e = ELet (
	id,
	{ p ; e = EGetFieldSurface (
	    { p ; e = EId "arguments" },
	    { p ; e = EConst (S.CString (string_of_int ix)) },
	    args_thunk ~from_parser p []) },
	e) } in
    { p ; e = ELambda (
	["this"; "arguments"],
	List.fold_right2 folder ids (iota (List.length ids)) body) }


(* Small extension to allow a named function to call itself.  I have
   not considered the repercussions of functions with the same name as
   an argument. *)

let func_stmt_lambda p func_name ids body = func_expr_lambda p ids body
(*  ELet (p, 
	func_name, 
	EId (p, "$funobj"),
	func_expr_lambda p ids body) *)
    
(* 13.2 *)
let func_object ?(from_parser=false) p ids lambda_exp =
  { p ; e = ELet (
      "$prototype",
      { p ; e = EObject (
	  [("proto", obj_proto ~from_parser p);
	   ("extensible", true_c p);
	   ("class", str p "Object")],
	  [("constructor",
	    [(Value, { p ; e = EConst S.CUndefined });
	     (Writable, true_c p);
	     (Enum, false_c p);
	     (Config, true_c p)])]) },
      { p ; e = ELet (
	  "$funobj",
	  { p ; e = EObject (
	      [("code", lambda_exp);
	       ("proto", fun_proto ~from_parser p);
	       ("extensible", true_c p);
	       ("class", str p "Function")],
	      [("length",
		[(Value, { p ; e = EConst (S.CNum (float_of_int (List.length ids))) });
		 (Writable, false_c p);
		 (Enum, false_c p);
		 (Config, false_c p)]);
	       ("prototype",
		[(Value, { p ; e = EId "$prototype" });
		 (Writable, true_c p);
		 (Config, false_c p);
		 (Enum, false_c p)])]) },
	  { p ; e = ESeq ({ p ; e = EUpdateFieldSurface (
			      { p ; e = EId "$prototype" },
			      { p ; e = EConst (S.CString "constructor") },
			      { p ; e = EId "$funobj" },
			      args_thunk ~from_parser p [{ p ; e = EId "$funobj" }]) },
			  { p ; e = EId "$funobj"}) }) }) }

let new_obj p proto_id = 
  { p ; e = EObject (
      [("proto", { p ; e = EId proto_id });
       ("extensible", true_c p);
       ("class", str p "Object")],
      []) }
