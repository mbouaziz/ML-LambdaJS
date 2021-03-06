open Prelude
open Es5_syntax
open JavaScript_syntax
open Es5_values
open Es5_delta

let rec interp_error pos message =
  "[interp] (" ^ string_of_position pos ^ ") " ^ message

let rec apply func args = match func with
  | Closure c -> c args
  | _ -> failwith ("[interp] Applied non-function, was actually " ^ 
		     pretty_value func)

(* args is the "arguments" object *)
let rec apply_obj o this args = match o with
  | ObjCell c -> 
      let { code ; _ } = !c in
      begin match code with
      | Some cl -> apply (Closure cl) [this; args]
      | None -> Fail "Applying inapplicable object!"
      end
  | _ -> Fail "Applying non-object!"
	  

let rec obj_get_field cell1 obj_this field args =
  let { props ; proto ; _ } = !cell1 in
  match IdMap.find_opt field props with
  | Some prop ->
      begin match prop.value with
      | Some value -> value
      | None -> match prop.getter with
	| Some getter -> apply_obj (ObjCell getter) obj_this (apply args [ObjCell getter])
	| None -> Const CUndefined
      end
  | None -> match proto with
      | Some c -> obj_get_field c obj_this field args
      | None -> Const CUndefined (* No prototype found *)

(* EUpdateField-Add *)
(* ES5 8.12.5, step 6 *)
let rec add_field obj field newval = match obj with
  | ObjCell c -> let { extensible ; props ; _ } as o = !c in
      if extensible then begin
	c := { o with props = IdMap.add field (mk_data_prop ~b:true newval) props };
	newval
      end else
	Const CUndefined	
  | _ -> failwith ("[interp] add_field given non-object.")

(* Both functions (because a property can satisfy writable and not_writable) *)
let rec writable prop = 
  (AttrMap.mem Writable prop) &&
    ((AttrMap.find Writable prop) = Const (CBool true))

let rec not_writable prop = 
  (AttrMap.mem Writable prop) &&
    ((AttrMap.find Writable prop) = Const (CBool false))

(* EUpdateField *)
(* ES5 8.12.4, 8.12.5 *)
let rec obj_update_field cell1 obj_this field newval args =
  let { props ; proto ; _ } as o = !cell1 in
  begin match IdMap.find_opt field props with
  | None ->
      begin match proto with
      | Some c -> obj_update_field c obj_this field newval args (* EUpdateField-Proto *)
      | None -> add_field obj_this field newval (* 8.12.4, step 4, sort of.  Handles if proto doesn't exist *)
      end
  | Some prop ->
      if prop.writable then
	begin match obj_this with
	| ObjCell cell2 when cell1 != cell2 ->
	    (* 8.12.4, last step where inherited.[[writable]] is true *)
	    add_field obj_this field newval
	| _ ->
	    (* 8.12.5, step 3 *)
	    cell1 := { o with props = IdMap.add field { prop with value = Some newval } props };
	    newval
	end
      else begin match prop.setter with (* 8.12.5, step 5 *)
      | Some setter -> apply_obj (ObjCell setter) obj_this (apply args [ObjCell setter])
      | None -> Fail "Field not writable!"
      end
  end

let rec get_attr attr obj field = match obj, field with
  | ObjCell c, Const (CString s) ->
      let { props ; _ } = !c in
      begin match IdMap.find_opt s props with
      | None ->	undef
      | Some prop ->
	  match attr with
	  | Value -> (match prop.value with Some v -> v | None -> undef)
	  | Setter -> (match prop.setter with Some o -> ObjCell o | None -> undef)
	  | Getter -> (match prop.getter with Some o -> ObjCell o | None -> undef)
	  | Writable -> Const (CBool prop.writable)
	  | Config -> Const (CBool prop.config)
	  | Enum -> Const (CBool prop.enum)
      end
  | _ -> failwith ("[interp] get-attr didn't get an object and a string.")

let to_acc prop = { prop with value = None; writable = false }
let to_data prop = match prop.value with
| Some _ -> { prop with setter = None; getter = None }
| None -> { prop with setter = None; getter = None; value = Some undef }
let is_data prop = prop.value <> None


let fun_obj objcell =
  let { code ; _ } = !objcell in code <> None

let prop_add_attr prop attr newval ~config ~writable =
  match attr, newval, config, writable with
  | Enum, Const (CBool b), true, _ -> { prop with enum = b }
  | Config, Const (CBool b) , true, _ -> { prop with config = b }
  | Writable, Const (CBool b), true, _ -> { (to_data prop) with writable = b }
  | Writable, Const (CBool false), _, true when is_data prop -> { prop with writable = false }
  | Value, v, _, true -> { (to_data prop) with value = Some v }
  | Setter, ObjCell c, true, _ when fun_obj c -> { (to_acc prop) with setter = Some c }
  | Setter, Const CUndefined, true, _ -> { (to_acc prop) with setter = None }
  | Getter, ObjCell c, true, _ when fun_obj c -> { (to_acc prop) with getter = Some c }
  | Getter, Const CUndefined, true, _ -> { (to_acc prop) with getter = None }
  | _ -> prop

(* 
   The goal here is to maintain a few invariants (implied by 8.12.9
   and 8.10.5), while keeping things simple from a semantic
   standpoint.  The errors from 8.12.9 and 8.10.5 can be defined in
   the environment and enforced that way.  The invariants here make it
   more obvious that the semantics can't go wrong.  In particular, a
   property

   1.  Has to be either an accessor or a data property, and;

   2.  Can't change attributes when Config is false, except for 
       a. Value, which checks Writable
       b. Writable, which can change true->false
*)
let set_attr attr obj field newval = match obj, field with
  | ObjCell c, Const (CString f_str) ->
      let { props ; extensible ; _ } as o = !c in
      begin match IdMap.find_opt f_str props with
      | None ->
	  if extensible then begin
	    let new_prop = prop_add_attr empty_prop_true attr newval ~config:true ~writable:true in
	    c := { o with props = IdMap.add f_str new_prop props };
	    newval
	  end else
	    failwith ("[interp] Extensible not true on object to extend with an attr")
      | Some prop ->
	  (* 8.12.9: "If a field is absent, then its value is considered to be false" *)
	  let new_prop = prop_add_attr prop attr newval ~config:prop.config ~writable:prop.writable in
	  c := { o with props = IdMap.add f_str new_prop props };
	  newval
      end
  | _ -> failwith ("[interp] set-attr didn't get an object and a string")

(* 8.10.5, steps 7/8 "If iscallable(getter) is false and getter is not
   undefined..." *)
	  

let rec eval ({ p ; e } : prim_exp) env =
  (* prerr_endline (string_of_position p); *)
  match e with
  | EConst c -> Const c
  | EId x -> begin
      try
	let varcell = IdMap.find x env in
	!varcell
      with Not_found ->
	failwith ("[interp] Unbound identifier: " ^ x ^ " in identifier lookup at " ^
		    (string_of_position p))
    end
  | ESet (x, e) -> begin
      try
	let varcell = IdMap.find x env in
	varcell := eval e env; !varcell
      with Not_found ->
	failwith ("[interp] Unbound identifier: " ^ x ^ " in set! at " ^
		    (string_of_position p))
    end
  | EObject (attrs, props) ->
      let eval_obj_attr obj (name, e) = match name, eval e env with
      | "proto", Const (CUndefined | CNull) -> { obj with proto = None }
      | "proto", ObjCell c -> { obj with proto = Some c }
      | "proto", _ -> failwith (sprintf "[interp] Internal property \"proto\" must have type object or null, at %s" (string_of_position p))
      | "extensible", Const (CBool extensible) -> { obj with extensible }
      | "extensible", _ -> failwith (sprintf "[interp] Internal property \"extensible\" must have type bool, at %s" (string_of_position p))
      | "class", Const (CString _class) -> { obj with _class }
      | "class", _ -> failwith (sprintf "[interp] Internal property \"class\" must have type string, at %s" (string_of_position p))
      | "code", Const (CUndefined | CNull) -> { obj with code = None }
      | "code", Closure c -> { obj with code = Some c }
      | "code", _ -> failwith (sprintf "[interp] Internal property \"code\" must be a closure or undefined, at %s" (string_of_position p))
      | _ -> failwith (sprintf "[interp] Unknown internal property %S, at %s" name (string_of_position p))
      in
      let eval_prop_attr prop (attr, e) = prop_add_attr prop attr (eval e env) ~config:true ~writable:true in
      let eval_prop m (name, attrs) = IdMap.add name (fold_left eval_prop_attr empty_prop attrs) m in
      ObjCell (ref { (fold_left eval_obj_attr empty_obj attrs) with
		       props = fold_left eval_prop IdMap.empty props })
  | EUpdateFieldSurface (obj, f, v, args) ->
      let obj_value = eval obj env in
      let f_value = eval f env in
      let v_value = eval v env in
      let args_value = eval args env in
      begin match obj_value, f_value with
      | ObjCell o, Const (CString field) -> obj_update_field o obj_value field v_value args_value
      | _ -> failwith ("[interp] Update field didn't get an object and a string" ^ string_of_position p)
      end
  | EGetFieldSurface (obj, f, args) ->
      let obj_value = eval obj env in
      let f_value = eval f env in 
      let args_value = eval args env in
      begin match obj_value, f_value with
      | Const CNull, Const (CString _) -> Const CUndefined
      | ObjCell o, Const (CString s) -> obj_get_field o obj_value s args_value
      | _ -> failwith ("[interp] Get field didn't get an object and a string at " 
		       ^ string_of_position p 
		       ^ ". Instead, it got " 
		       ^ pretty_value obj_value 
		       ^ " and " 
		       ^ pretty_value f_value)
      end
  | EDeleteField (obj, f) ->
      let obj_val = eval obj env in
      let f_val = eval f env in
      begin match obj_val, f_val with
      | ObjCell c, Const (CString s) ->
	  let { props ; _ } as o = !c in
	  begin match IdMap.find_opt s props with
	  | Some { config = true ; _ } ->
	      c := { o with props = IdMap.remove s props };
	      Const (CBool true)
	  | _ -> Const (CBool false)
	  end
      | _ -> failwith ("[interp] EDeleteField didn't get an object and string at " ^ string_of_position p)
      end
  | EAttr (attr, obj, field) ->
      let obj_val = eval obj env in
      let f_val = eval field env in
	get_attr attr obj_val f_val
  | ESetAttr (attr, obj, field, newval) ->
      let obj_val = eval obj env in
      let f_val = eval field env in
      let v_val = eval newval env in
	set_attr attr obj_val f_val v_val
  | EOp1 (`Prim1 str, e) ->
      let e_val = eval e env in
      op1 str e_val
  | EOp2 (`Prim2 str, e1, e2) ->
      let e1_val = eval e1 env in
      let e2_val = eval e2 env in
      op2 str e1_val e2_val
  | EOp3 (`Prim3 str, e1, e2, e3) ->
      let e1_val = eval e1 env in
      let e2_val = eval e2 env in
      let e3_val = eval e3 env in
      op3 str e1_val e2_val e3_val
  | EIf (c, t, e) ->
      let c_val = eval c env in
	if (c_val = Const (CBool true))
	then eval t env
	else eval e env
  | EApp (func, args) -> 
      let func_value = eval func env in
      let args_values = map (fun e -> eval e env) args in begin
	match func_value, args_values with
	  | ObjCell o, [this; args] -> 
	      apply_obj func_value this args
	  | Closure c, _ -> apply func_value args_values
	  | ObjCell o, _ ->
	      failwith ("[interp] Need to provide this and args for a call to a function object at " ^ string_of_position p)
	  | _, _ -> failwith ("[interp] Inapplicable value: " ^ pretty_value func_value ^ ", applied to " ^ pretty_value_list args_values ^ ", at " ^ string_of_position p)
	end
  | ESeq (e1, e2) -> 
      ignore (eval e1 env);
      eval e2 env
  | ELet (x, e, body) ->
      let e_val = eval e env in
	eval body (IdMap.add x (ref e_val) env)
  | EFix (x, e) ->
      let x_var = ref (Const CUndefined) in
      let e_val = eval e (IdMap.add x x_var env) in begin
	  x_var := e_val;
	  e_val
	end
  | ELabel (l, e) -> begin
      try
	eval e env
      with Break (l', v) ->
	if l = l' then v
	else raise (Break (l', v))
    end
  | EBreak (l, e) ->
      raise (Break (l, eval e env))
  | ETryCatch (body, catch) -> begin
      try
	eval body env
      with Throw v -> apply (eval catch env) [v]
    end
  | ETryFinally (body, fin) -> begin
      try
	ignore (eval body env)
      with
	| Throw v -> ignore (eval fin env); raise (Throw v)
	| Break (l, v) -> ignore (eval fin env); raise (Break (l, v))
    end;
      eval fin env
  | EThrow e -> raise (Throw (eval e env))
  | ELambda (xs, e) -> 
      let set_arg arg x m = IdMap.add x (ref arg) m in
	Closure (fun args -> 
		     if (List.length args) != (List.length xs) then
		       arity_mismatch_err p xs args
		     else
		     eval e (List.fold_right2 set_arg args xs env))

and arity_mismatch_err p xs args = failwith ("Arity mismatch, supplied " ^ string_of_int (List.length args) ^ " arguments and expected " ^ string_of_int (List.length xs) ^ " at " ^ string_of_position p ^ ". Arg names were: " ^ (List.fold_right (^) (map (fun s -> " " ^ s ^ " ") xs) "") ^ ". Values were: " ^ (List.fold_right (^) (map (fun v -> " " ^ pretty_value v ^ " ") args) ""))

let rec eval_expr expr = try 
  eval expr IdMap.empty
with
  | Throw v ->
      let err_msg = 
	match v with
	  | ObjCell c ->
	      let { props ; _ } = !c in
	      begin match IdMap.find_opt "message" props with
	      | Some { value = Some msg_val ; _ } -> pretty_value msg_val
	      | _ -> pretty_value v
	      end
	  | v -> pretty_value v
      in
      failwith ("Uncaught exception: " ^ err_msg)
  | Break (l, v) -> failwith ("Broke to top of execution, missed label: " ^ l)
