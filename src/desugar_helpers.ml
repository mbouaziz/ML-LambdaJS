open Prelude
open Es5_syntax
open Exprjs_syntax

module S = JavaScript_syntax

let true_c p = { p ; e = EConst (S.CBool true) }
let false_c p = { p ; e = EConst (S.CBool false) }
let bool_c p b = { p ; e = EConst (S.CBool b) }

let undef_c p = { p ; e = EConst S.CUndefined }

let str p s = { p ; e = EConst (S.CString s) }

let num_c p d = { p ; e = EConst (S.CNum d) }

let int_c p d = { p ; e = EConst (S.CInt d) }

 (* WHY is are there a difference here? *)
let obj_proto ?(from_parser=false) p = { p ; e = EId (if from_parser then "Object_prototype" else "[[Object_prototype]]") }
let fun_proto ?(from_parser=false) p = { p ; e = EId (if from_parser then "Function_prototype" else "[[Function_prototype]]") }

let to_object p e = { p ; e = EApp ({ p ; e = EId "[[ToObject]]" }, [e]) }
let to_string p e = { p ; e = EApp ({ p ; e = EId "[[ToString]]" }, [e]) }

let mk_val ?(config=true) ?(enum=true) p v =
  [(Value, v);
   (Enum, bool_c p enum);
   (Config, bool_c p config);
   (Writable, true_c p)]

let mk_field ?(config=true) ?(enum=true) (p, s, e) =
  match e with
    | _ -> (s, mk_val ~config ~enum p e)

let mk_array (p, exps) =
  let mk_num_field n v = (string_of_int n, mk_val p v) in
  { p ; e = EObject ([("proto", { p ; e = EId "[[Array_prototype]]" });
		      ("extensible", true_c p);
		      ("class", str p "Array")],
		     ((mk_field ~config:false ~enum:false (p, "length", int_c p (List.length exps)))
		      :: List.map2 mk_num_field (iota (List.length exps)) exps)) }

(* 10.6 *)
let args_obj ?(from_parser=false) ?(strict_mode=false) p arg_list =
  let mk_field n v = (string_of_int n,  mk_val p v) in
  let attrs =
    [("proto", obj_proto ~from_parser p);
     ("class", str p "Arguments");
     ("extensible", if strict_mode then false_c p else true_c p)]
      (* Need true here because of function body desugaring. Turned into false into every desugared function *)
  in
  { p ; e = EObject 
      (* 10.6 steps 4, 6 *)
      (
	attrs,
       (* 10.6 steps 1, 7 *)
       (("length", [(Value, int_c p (List.length arg_list));
		    (Writable, true_c p);
		    (Enum, false_c p);
		    (Config, true_c p)])
	  (* 10.6 step 13a *)
	::("callee", [(Getter, { p ; e = EId "[[ThrowTypeError]]" });
		      (Setter, { p ; e = EId "[[ThrowTypeError]]" });
		      (Enum, false_c p);
		      (Config, true_c p)])
	::("caller", [(Getter, { p ; e = EId "[[ThrowTypeError]]" });
		      (Setter, { p ; e = EId "[[ThrowTypeError]]" });
		      (Enum, false_c p);
		      (Config, false_c p)])
	:: (List.map2 mk_field (iota (List.length arg_list)) arg_list))) }


(* Used by getters and setters---the function will be known at
runtime *)
let args_thunk ?(from_parser=false) ?(strict_mode=false) p arg_list =
  { p ; e = ELambda (["func"], args_obj ~from_parser ~strict_mode p arg_list) }

    
(* 13.2 *)
let func_object ?(from_parser=false) ?(strict_mode=false) p ids lambda_exp =
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
			      args_thunk ~from_parser ~strict_mode p [{ p ; e = EId "$funobj" }]) },
			  { p ; e = EId "$funobj"}) }) }) }

let new_obj p proto_id = 
  { p ; e = EObject (
      [("proto", { p ; e = EId proto_id });
       ("extensible", true_c p);
       ("class", str p "Object")],
      []) }


(* Same idea as in original \JS --- use the args array *)
let func_expr_lambda ?(from_parser=false) ?(strict_mode=false) p ids body =
  let e_args = { p ; e = EId "arguments" } in
  let folder id ix e =
    let e_six = { p ; e = EConst (S.CString (string_of_int ix)) } in
    { p ; e = ELet (
	id,
	{ p ; e = EGetFieldSurface (e_args, e_six, args_thunk ~from_parser ~strict_mode p []) },
	if strict_mode then
	  e
	else
	(* partial "arguments"-bug fix:
	   instead of just let x = arguments[0] in
	   we remove
	   arguments[0]
	   and add it again as an accessor, the getter returns x, the setter sets x
	*)
	  let e_getter = func_object ~from_parser ~strict_mode p [] { p ; e = ELambda (["this"; "arguments"], { p ; e = EId id }) } in
	  let e_setter = func_object ~from_parser ~strict_mode p [""] { p ; e = ELambda (["this"; "arguments"], { p ; e = ESet(id, { p ; e = EGetFieldSurface(e_args, { p ; e = EConst (S.CString "0") }, { p ; e = EConst S.CUndefined }) }) }) } in
	  { p ; e = ESeq (
	      { p ; e = ESeq (
		  { p ; e = EDeleteField (e_args, e_six) },
		  { p ; e = ESeq (
		      { p ; e = ESetAttr (Getter, e_args, e_six, e_getter) },
		      { p ; e = ESetAttr (Setter, e_args, e_six, e_setter) }) }) },
	      e) }
      ) } in
  let body = if strict_mode then
    body
  else
    { p ; e = ESeq ({ p ; e = EOp1(`Prim1 "prevent-extensions", e_args) }, body) }
  in
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
