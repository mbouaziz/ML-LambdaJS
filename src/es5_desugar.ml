open Prelude
open Exprjs_syntax
open Es5_syntax
open Desugar_helpers
module S = JavaScript_syntax

type env = bool IdMap.t

let id_counter = ref 0
let mk_id s = 
  id_counter := !id_counter + 1;
  s ^ (string_of_int !id_counter)

let rec ds (expr: Exprjs_syntax.expr) : Es5_syntax.src_exp =
  match expr with
    | ConstExpr (p,c) -> { p ; e = EConst c }
	
    | ArrayExpr (p,expr_list) -> mk_array (p, map ds expr_list)

    | ObjectExpr (p, exprs) -> 
        let (binder, obj_fields) = ds_fields exprs in
	  binder ({ p ; e = EObject (
		      [("proto", obj_proto p);
		       ("extensible", true_c p);
		       ("class", str p "Object")],
		      obj_fields) })
	    
    | ThisExpr (p) -> { p ; e = EId "this" }
	
    | VarExpr (p, x) -> { p ; e = EId x }
	
    | IdExpr (p, x) -> { p ; e = EId x }

    | BracketExpr (p, obj, f) ->
	{ p ; e = EGetFieldSurface (to_object p (ds obj), to_string p (ds f), args_thunk p []) }
	  
    | AssignExpr (p1, VarLValue (p2, x), e) ->
	{ p = p1 ; e = ESet (x, ds e) }

    | AssignExpr (p1, PropLValue (p2, obj, f), e) ->
	{ p = p1 ; e = ELet (
	    "$newVal",
	    ds e,
	    { p = p1 ; e = ELet (
		"$set-field-result",
		{ p = p1 ; e = EUpdateFieldSurface (
		    to_object p2 (ds obj),
		    to_string p2 (ds f),
		    { p = p1 ; e = EId "$newVal" },
		    args_thunk p1 [{ p = p1 ; e = EId "$newVal" }]) },
		{ p = p1 ; e = EIf (
		    { p = p1 ; e = EOp1 (`Prim1 ("fail?"), { p = p1 ; e = EId "$set-field-result" }) },
		    { p = p1 ; e = EApp ({ p = p1 ; e = EId "[[ThrowTypeError]]"}, [undef_c p1; str p1 ((string_of_position p1) ^ "unable to set")]) },
		    { p = p1 ; e = EId "$set-field-result" }) }) }) }

    (* 11.2.2 *)
    | NewExpr (p, e, args) ->
	{ p ; e = ELet (
	    "$constructor",
	    ds e,
	    { p ; e = ELet (
		"$proto",
		{ p ; e = EGetFieldSurface (
		    { p ; e = EId "$constructor" },
		    str p "prototype",
		    args_thunk p []) },
		{ p ; e = ELet (
		    "$newObj",
		    new_obj p "$proto",
		    { p ; e = ELet (
			"$resObj",
			{ p ; e = EApp (
				    { p ; e = EId "$constructor" },
				    [{ p ; e = EId "$newObj" };
				     args_obj p (map ds args)]) },
			{ p ; e = EIf (
			    { p ; e = EOp2 (
				`Prim2 ("stx="),
				{ p ; e = EOp1 (
				    `Op1Prefix ("typeof"),
				    { p ; e = EId "$resObj" }) },
				{ p ; e = EConst (S.CString "object") }) },
			    { p ; e = EId "$resObj" },
			    { p ; e = EId "$newObj" } ) }) }) }) }) }



    | PrefixExpr (p, op, e) ->
	{ p ; e = EOp1 (`Op1Prefix op, ds e) }

    | InfixExpr (p, op, e1, e2) ->
	{ p ; e = EOp2 (`Op2Infix op, ds e1, ds e2) }

    | IfExpr (p, c, t, e) ->
	{ p ; e = EIf (ds c, ds t, ds e) }

    | AppExpr (p, BracketExpr (p', obj, prop), es) ->
	{ p ; e = ELet (
	    "$obj",
	    ds obj,
	    { p ; e = ELet ("$fun", { p = p' ; e = EGetFieldSurface ({ p ; e = EId "$obj" }, ds prop, args_thunk p []) },
			    { p ; e = EApp ({ p = p' ; e = EId "$fun" },
					    [{ p = p' ; e = EId "$obj" }; args_obj p (map ds es)]) }) }) }

    | AppExpr (p, func, es) ->
	{ p ; e = ELet (
	    "$fun",
	    ds func,
	    { p ; e = EApp (
		{ p ; e = EId "$fun" },
		[{ p ; e = EId "[[global]]" }; args_obj p (map ds es)]) }) }

    | FuncExpr (p, ids, body) ->
	func_object p ids (func_expr_lambda p ids (var_lift body))

    | FuncStmtExpr (p, func_name, ids, body) ->
	{ p ; e = ESeq (
	    { p ; e = ESet (
		func_name,
		func_object p ids (func_stmt_lambda p func_name ids (var_lift body))) },
	    { p ; e = EId func_name }) }

    | LetExpr (p, x, e1, e2) -> { p ; e = ELet (x, ds e1, ds e2) }

    | SeqExpr (p, e1, e2) -> { p ; e = ESeq (ds e1, ds e2) }

    | WhileExpr (p, check, body) ->
	{ p ; e = ELet (
	    "$check",
	    { p ; e = EFix (
		"$check",
		{ p ; e = ELambda (
		    [],
		    { p ; e = EIf (
			ds check,
			{ p ; e = ESeq (
			    ds body,
			    { p ; e = EApp ({ p ; e = EId "$check" }, []) }) },
			{ p ; e = EConst S.CUndefined }) }) }) },
	    { p ; e = EApp ({ p ; e = EId "$check" }, []) }) }

    | DoWhileExpr (p, body, check) ->
	let body_exp = ds body in
	{ p ; e = ESeq (
	    body_exp,
	    { p ; e = ELet (
		"$check",
		{ p ; e = EFix (
		    "$check",
		    { p ; e = ELambda (
			[],
			{ p ; e = EIf (
			    ds check,
			    { p ; e = ESeq (
				body_exp,
				{ p ; e = EApp ({ p ; e = EId "$check" }, []) })},
			    { p ; e = EConst S.CUndefined }) }) }) },
		{ p ; e = EApp ({ p ; e = EId "$check" }, []) }) }) }

    | LabelledExpr (p, l, e) ->
	{ p ; e = ELabel (l, ds e) }
    | BreakExpr (p, l, e) ->
	{ p ; e = EBreak (l, ds e) }
    | ForInExpr (p, x, obj, body) ->
	let body_fun = { p ; e = ELambda ([], ds body) } in
	let set_fun = { p ; e = ELambda (["%new-index"], { p ; e = ESet (x, { p ; e = EId "%new-index" }) }) } in
	{ p ; e = EApp ({ p ; e = EId "[[forin]]" }, [ds obj; set_fun; body_fun]) }
    | VarDeclExpr (p, x, e) ->
	{ p ; e = ESet (x, ds e) }
    | TryCatchExpr (p, body, x, catch) ->
	{ p ; e = ETryCatch (ds body, { p ; e = ELambda ([x], ds catch) }) }
    | TryFinallyExpr (p, body, fin) ->
	{ p ; e = ETryFinally (ds body, ds fin) }
    | ThrowExpr (p, e) ->
	{ p ; e = EThrow (ds e) }
    | HintExpr (p, e1, e2) -> str p "NYI---Hints"

and ds_fields fields = 
    (* Builds a cascading let that evaluates the pieces of the object
    literal in the correct order, and fills in the generated
    identifiers in the correct attribute/field position in the map of
    fields.  This is to preserve evaluation order --- the order of the
    fields and attributes is irrelevant in an object *value*, but
    needs to be respected when evaluating an object literal *)
  let bind_attr (binder, fld_map) (p, name, expr) = 
    let ident = mk_id ("$ds_" ^ name) in
      (* For setters, we don't have a writable property, so we
         remove it from the defaults *)
    let this_fld_for a = match a with
      | Setter -> AttrMap.remove Writable (IdMap.find name fld_map)
      | _ -> IdMap.find name fld_map in
    let add_attr a v = 
      IdMap.add name (AttrMap.add a v (this_fld_for a)) fld_map in
    let mk_bind attr_expr obj =
      binder ({ p ; e = ELet (ident, ds attr_expr, obj) }) in
    match expr with
      | SetterExpr (p, setter_exp) ->
          mk_bind setter_exp, add_attr Setter ({ p ; e = EId ident })
      | GetterExpr (p, getter_exp) -> 
          mk_bind getter_exp, add_attr Getter ({ p ; e = EId ident })
      | value_exp ->
          mk_bind value_exp, add_attr Value ({ p ; e = EId ident })
  in
  let defaults = 
    AttrMap.add Config (true_c dummy_pos)
      (AttrMap.add Writable (true_c dummy_pos)
	 (AttrMap.add Enum (true_c dummy_pos) AttrMap.empty)) in
    (* Map with all fields found mapping to default attr lists *)
  let fields_init =
    let add_fld fld_map (p, name, e) =
      IdMap.add name defaults fld_map
    in List.fold_left add_fld IdMap.empty fields
  in
  let (binder, obj_map) = 
    List.fold_left bind_attr ((fun x -> x), fields_init) fields
  in
  let fold_fld name attr_map new_fields =
    (name, (AttrMap.fold (fun a v l -> (a,v)::l) attr_map [])) :: new_fields
  in
  let obj = IdMap.fold fold_fld obj_map [] in
    (binder, obj)

and var_lift expr =
  let folder (p,id) e = 
    { p ; e = ELet (id, undef_c p, e) } in
    List.fold_right folder (vars_in expr) (ds expr)

(* Collect all the vars (and their source locations) in the
   expression.  Recur inside everything that isn't a function.  We
   just collect the names, and add them as undefined, let-alloced
   values at the top level.  In desugaring, we turn the VarDeclExpr
   into an assignment statement. *)

and vars_in expr = match expr with
    (* remember the source loc of vars *)
  | VarDeclExpr (p,x,e) -> [(p,x)]
      (* don't go inside functions *)
  | FuncExpr (_, _, _) -> []
  | FuncStmtExpr (p,name,_,_) -> [(p,name)]
      (* the rest is boilerplate *)
  | ConstExpr (_, _) -> []
  | ArrayExpr (_, elts) -> List.concat (map vars_in elts)
  | ObjectExpr (_, fields) ->
      let field_vars (p,n,v) = vars_in v in
	List.concat (map field_vars fields)
  | ThisExpr (_) -> []
  | VarExpr (_,_) -> []
  | IdExpr (_,_) -> []
  | BracketExpr (_, o, f) -> List.concat (map vars_in [o; f])
  | NewExpr (_, o, args) -> List.concat (map vars_in (o :: args))
  | PrefixExpr (_, _, e) -> vars_in e
  | InfixExpr (_, _, e1, e2) -> List.concat (map vars_in [e1; e2])
  | IfExpr (_,c,t,e) -> List.concat (map vars_in [c; t; e])
  | AssignExpr (_,_,e) -> vars_in e
  | AppExpr (_, f, args) -> List.concat (map vars_in (f :: args))
  | LetExpr (_, _, e, body) -> List.concat (map vars_in [e; body])
  | SeqExpr (_, e1, e2) -> List.concat (map vars_in [e1; e2])
  | WhileExpr (_, e1, e2) -> List.concat (map vars_in [e1; e2])
  | DoWhileExpr (_, e1, e2) -> List.concat (map vars_in [e1; e2])
  | LabelledExpr (_, _, e) -> vars_in e
  | BreakExpr (_, _, e) -> vars_in e
  | ForInExpr (_, _, e1, e2) -> List.concat (map vars_in [e1; e2])
  | TryCatchExpr (_, e1, _, e2) -> List.concat (map vars_in [e1; e2])
  | TryFinallyExpr (_, e1, e2) -> List.concat (map vars_in [e1; e2])
  | ThrowExpr (_, e) -> vars_in e
  | HintExpr (_,_,_) -> []


let rec ds_op ({ p ; e } : src_exp) : prim_exp = match e with
| EOp1 (`Op1Prefix op, e) ->
    begin match op with
    | "prefix:delete" ->
	begin match e with
	| { p ; e = EGetFieldSurface (obj, field, args) } ->
	    { p ; e = EDeleteField (ds_op obj, ds_op field) }
	| _ -> { p ; e = ESeq (ds_op e, true_c p) }
	end
    | "prefix:!" ->
	{ p ; e = EIf ({ p ; e = EOp1 (`Prim1 "prim->bool", ds_op e) },
		       false_c p,
		       true_c p) }
    | "prefix:~" ->
	{ p ; e = EOp1 (`Prim1 "~",
			{ p ; e = EApp ({ p ; e = EId "[[toInt]]" }, [ ds_op e ]) }) }
    | "prefix:+" ->
	{ p ; e = EApp ({ p ; e = EId "[[ToNumber]]" }, [ ds_op e ]) }
    | "prefix:-" ->
	{ p ; e = EOp2 (`Prim2 "-",
			num_c p 0.0,
			{ p ; e = EApp ({ p ; e = EId "[[ToNumber]]" }, [ ds_op e ]) }) }
    | "typeof" ->
	{ p ; e = EOp1 (`Prim1 "typeof", ds_op e) }
    | "prefix:typeof" ->
	{ p ; e = EOp1 (`Prim1 "surface-typeof", ds_op e) }
    | "prefix:void" ->
	{ p ; e = ESeq (ds_op e, { p ; e = EConst S.CUndefined }) }
    | _ -> failwith ("unknown prefix operator: " ^ op)
    end
| EOp2 (`Op2Infix op, e1, e2) ->
    begin match op with
    | "*" -> numnum p op e1 e2
    | "/" -> numnum p op e1 e2
    | "%" -> numnum p op e1 e2
    | "-" -> numnum p op e1 e2
    | "&" -> int_int p op e1 e2
    | "|" -> int_int p op e1 e2
    | "^" -> int_int p op e1 e2
    | "<<" -> int_uint p op e1 e2
    | ">>" -> int_uint p op e1 e2
    | ">>>" -> uint_uint p op e1 e2
    | "<" -> { p ; e = EOp2 (`Prim2 "<", ds_op e1, ds_op e2) }
    | ">" -> { p ; e = EOp2 (`Prim2 ">", ds_op e1, ds_op e2) }
    | "<=" -> { p ; e = EOp2 (`Prim2 "<=", ds_op e1, ds_op e2) }
    | ">=" -> { p ; e = EOp2 (`Prim2 ">=", ds_op e1, ds_op e2) }
    (* | ">=" -> { p ; e = EIf ({ p ; e = EOp2 (`Prim2 "<", ds_op e1, ds_op e2) }, *)
    (* 			     false_c p, true_c p) } *)
    (* | "<=" -> { p ; e = EIf ({ p ; e = EOp2 (`Prim2 ">", ds_op e1, ds_op e2) }, *)
    (* 			     false_c p, true_c p) } *)
    | "instanceof" -> { p ; e = EApp ({ p ; e = EId "[[instanceof]]" },
				      [ ds_op e1; ds_op e2]) }
    | "in" -> { p ; e = EApp ({ p ; e = EId "[[in]]" },
			      [ ds_op e1; ds_op e2]) }
	(* The equality operators are implemented in \JS *)
    | "==" -> { p ; e = EOp2 (`Prim2 "abs=", ds_op e1, ds_op e2) }
    | "!=" -> { p ; e = EIf ({ p ; e = EOp2 (`Prim2 "==", ds_op e1, ds_op e2) },
			     false_c p, true_c p) }
    | "===" -> { p ; e = EOp2 (`Prim2 "stx=", ds_op e1, ds_op e2) }
    | "!==" -> { p ; e = EIf ({ p ; e = EOp2 (`Prim2 "stx=", ds_op e1, ds_op e2) },
			      false_c p, true_c p) }
	(* 11.11 *)
    | "&&" -> { p ; e = ELet ("$lAnd", ds_op e1,
			      { p ; e = EIf ({ p ; e = EApp ({ p ; e = EId "[[toBoolean]]" },
							     [{ p ; e = EId "$lAnd" }]) },
					     ds_op e2,
					     { p ; e = EId "$lAnd" }) }) }
    | "||" -> { p ; e = ELet ("$lOr", ds_op e1,
			      { p ; e = EIf ({ p ; e = EApp ({ p ; e = EId "[[toBoolean]]"},
							     [{ p ; e = EId "$lOr" }]) },
					     { p ; e = EId "$lOr" },
					     ds_op e2) }) }
    | "+" -> { p ; e = EApp ({ p ; e = EId "[[plus]]" }, [ds_op e1; ds_op e2]) }
    | _ -> failwith ("unknown infix operator: " ^ op)
    end
| EOp1 (`Prim1 op, e) -> { p ; e = EOp1 (`Prim1 op, ds_op e) }
| EOp2 (`Prim2 op, e1, e2) -> { p ; e = EOp2 (`Prim2 op, ds_op e1, ds_op e2) }
| EOp3 (`Op3Prefix op, e1, e2, e3) -> failwith ("unknown prefix operator: " ^ op)
| EOp3 (`Prim3 op, e1, e2, e3) -> { p ; e = EOp3 (`Prim3 op, ds_op e1, ds_op e2, ds_op e3) }
| EConst c -> { p ; e = EConst c }
| EId x -> { p ; e = EId x }
| EObject (internals, fields) ->
    let ds_op_attr (name, value) = (name, ds_op value) in
    let ds_op_field (name, attrs) = (name, map ds_op_attr attrs) in
    { p ; e = EObject (map ds_op_attr internals, map ds_op_field fields) }
| EUpdateFieldSurface (o, f, v, args) ->
    { p ; e = EUpdateFieldSurface (ds_op o, ds_op f, ds_op v, ds_op args) }
| EGetFieldSurface (o, f, args) ->
    { p ; e = EGetFieldSurface (ds_op o, ds_op f, ds_op args) }
| EDeleteField (o, f) ->
    { p ; e = EDeleteField (ds_op o, ds_op f) }
| EAttr (a, o, f) ->
    { p ; e = EAttr (a, ds_op o, ds_op f) }
| ESetAttr (a, o, f, v) ->
    { p ; e = ESetAttr (a, ds_op o, ds_op f, ds_op v) }
| ESet (x, v) ->
    { p ; e = ESet (x, ds_op v) }
| EIf (c, t, e) ->
    { p ; e = EIf (ds_op c, ds_op t, ds_op e) }
| EApp (func, args) ->
    { p ; e = EApp (ds_op func, map ds_op args) }
| ESeq (e1, e2) ->
    { p ; e = ESeq (ds_op e1, ds_op e2) }
| ELet (x, e1, body) ->
    { p ; e = ELet (x, ds_op e1, ds_op body) }
| EFix (x, e) -> { p ; e = EFix (x, ds_op e) }
| ELabel (l, e) -> { p ; e = ELabel (l, ds_op e) }
| EBreak (l, e) -> { p ; e = EBreak (l, ds_op e) }
| ETryCatch (body, catch) ->
    { p ; e = ETryCatch (ds_op body, ds_op catch) }
| ETryFinally (body, fin) ->
    { p ; e = ETryFinally (ds_op body, ds_op fin) }
| EThrow e -> { p ; e = EThrow (ds_op e) }
| ELambda (ids, body) -> { p ; e = ELambda (ids, ds_op body) }

and numnum p op e1 e2 =
  { p ; e = EOp2 (`Prim2 op,
        { p ; e = EApp ({ p ; e = EId "[[ToNumber]]" }, [ ds_op e1 ]) },
        { p ; e = EApp ({ p ; e = EId "[[ToNumber]]" }, [ ds_op e2 ]) }) }

and int_int p op e1 e2 =
  { p ; e = EOp2 (`Prim2 op,
        { p ; e = EApp ({ p ; e = EId "[[toInt]]" }, [ ds_op e1 ]) },
        { p ; e = EApp ({ p ; e = EId "[[toInt]]" }, [ ds_op e2 ]) }) }

and int_uint p op e1 e2 =
  { p ; e = EOp2 (`Prim2 op,
        { p ; e = EApp ({ p ; e = EId "[[toInt]]" }, [ ds_op e1 ]) },
        { p ; e = EApp ({ p ; e = EId "[[toUInt]]" }, [ ds_op e2 ]) }) }

and uint_uint p op e1 e2 = 
  { p ; e = EOp2 (`Prim2 op,
        { p ; e = EApp ({ p ; e = EId "[[toUInt]]" }, [ ds_op e1 ]) },
        { p ; e = EApp ({ p ; e = EId "[[toUInt]]" }, [ ds_op e2 ]) }) }

let rec ds_global { p ; e } env = match e with
  | EApp (e, es) ->
      { p ; e = EApp (ds_global e env, map (fun e -> ds_global e env) es) }
  | EId (x) -> begin
      try
	if IdMap.find x env
	then { p ; e = EId x }
	else { p ; e = EGetFieldSurface ({ p ; e = EId "[[global]]" }, str p x, args_thunk p []) }
      with Not_found ->
	{ p ; e = EGetFieldSurface ({ p ; e = EId "[[global]]" }, str p x, args_thunk p []) }
    end
  | ESet (x, e) -> begin
      try
	if IdMap.find x env
	then { p ; e = ESet (x, ds_global e env) }
	else
	  { p ; e = ELet ("$newVal", ds_global e env,
		{ p ; e = EUpdateFieldSurface (
				     { p ; e = EId "[[global]]" },
				     str p x,
				     { p ; e = EId "$newVal" },
				     args_thunk p [{ p ; e = EId "$newVal" }]) }) }
      with Not_found ->
	{ p ; e = ELet ("$newVal", ds_global e env,
	      { p ; e = EUpdateFieldSurface (
				   { p ; e = EId "[[global]]" },
				   str p x,
				   { p ; e = EId "$newVal" },
				   args_thunk p [{ p ; e = EId "$newVal" }]) }) }
    end
  | ELambda (ids, e) ->
      let new_env = fold_left (fun env x -> IdMap.add x true env) env ids in
	{ p ; e = ELambda (ids, ds_global e new_env) }
  | ELet (x, e1, e2) ->
      { p ; e = ELet (x, ds_global e1 env, ds_global e2 (IdMap.add x true env)) }
  | EFix (x, e) -> 
      { p ; e = EFix (x, ds_global e (IdMap.add x true env)) }
  | EConst _ -> { p ; e }
  | EObject (attrs, props) ->
      let attr (name, value) = (name, ds_global value env) in
      let prop (name, attrs) = (name, map attr attrs) in
	{ p ; e = EObject (map attr attrs, map prop props) }
  | EUpdateFieldSurface (o, f, e, args) ->
      { p ; e = EUpdateFieldSurface (ds_global o env,
				     ds_global f env,
				     ds_global e env,
				     ds_global args env) }
  | EGetFieldSurface (o, f, args) ->
      { p ; e = EGetFieldSurface (ds_global o env, ds_global f env, ds_global args env) }
  | EDeleteField (o, f) ->
      { p ; e = EDeleteField (ds_global o env, ds_global f env) }
  | EAttr (a, o, f) ->
      { p ; e = EAttr (a, ds_global o env, ds_global f env) }
  | ESetAttr (a, o, f, v) ->
      { p ; e = ESetAttr (a, ds_global o env, ds_global f env, ds_global v env) }
  | EOp1 (op, e) -> { p ; e = EOp1 (op, ds_global e env) }
  | EOp2 (op, e1, e2) -> { p ; e = EOp2 (op, ds_global e1 env, ds_global e2 env) }
  | EOp3 (op, e1, e2, e3) -> { p ; e = EOp3 (op, ds_global e1 env,
					     ds_global e2 env,
					     ds_global e3 env) }
  | EIf (c, t, e) ->
      { p ; e = EIf (ds_global c env, ds_global t env, ds_global e env) }
  | ESeq (e1, e2) -> { p ; e = ESeq (ds_global e1 env, ds_global e2 env) }
  | ELabel (l, e) -> { p ; e = ELabel (l, ds_global e env) }
  | EBreak (l, e) -> { p ; e = EBreak (l, ds_global e env) }
  | ETryCatch (e1, e2) -> { p ; e = ETryCatch (ds_global e1 env, ds_global e2 env) }
  | ETryFinally (e1, e2) ->
      { p ; e = ETryFinally (ds_global e1 env, ds_global e2 env) }
  | EThrow e -> { p ; e = EThrow (ds_global e env) }


let ds_top expr = ds expr
let desugar expr = ds_global (ds_op expr) IdMap.empty


let rec check_op ({ p ; e } : src_exp) : prim_exp =
  let e = match e with
  | EOp1 (`Op1Prefix op, _) -> failwith ("[check_op] prefix operator: " ^ op)
  | EOp2 (`Op2Infix op, _, _) -> failwith ("[check_op] infix operator: " ^ op)
  | EOp3 (`Op3Prefix op, _, _, _) -> failwith ("[check_op] prefix operator: " ^ op)
  | EOp1 (`Prim1 op, e) -> EOp1 (`Prim1 op, check_op e)
  | EOp2 (`Prim2 op, e1, e2) -> EOp2 (`Prim2 op, check_op e1, check_op e2)
  | EOp3 (`Prim3 op, e1, e2, e3) -> EOp3 (`Prim3 op, check_op e1, check_op e2, check_op e3)
  | EConst c -> EConst c
  | EId x -> EId x
  | EObject (internals, fields) ->
      let check_op_attr (name, value) = (name, check_op value) in
      let check_op_field (name, attrs) = (name, map check_op_attr attrs) in
	EObject (map check_op_attr internals, map check_op_field fields)
  | EUpdateFieldSurface (o, f, v, args) -> EUpdateFieldSurface (check_op o, check_op f, check_op v, check_op args)
  | EGetFieldSurface (o, f, args) -> EGetFieldSurface (check_op o, check_op f, check_op args)
  | EDeleteField (o, f) -> EDeleteField (check_op o, check_op f)
  | EAttr (a, o, f) -> EAttr (a, check_op o, check_op f)
  | ESetAttr (a, o, f, v) -> ESetAttr (a, check_op o, check_op f, check_op v)
  | ESet (x, v) -> ESet (x, check_op v)
  | EIf (c, t, e) -> EIf (check_op c, check_op t, check_op e)
  | EApp (func, args) -> EApp (check_op func, map check_op args)
  | ESeq (e1, e2) -> ESeq (check_op e1, check_op e2)
  | ELet (x, e1, body) -> ELet (x, check_op e1, check_op body)
  | EFix (x, e) -> EFix (x, check_op e)
  | ELabel (l, e) -> ELabel (l, check_op e)
  | EBreak (l, e) -> EBreak (l, check_op e)
  | ETryCatch (body, catch) -> ETryCatch (check_op body, check_op catch)
  | ETryFinally (body, fin) -> ETryFinally (check_op body, check_op fin)
  | EThrow e -> EThrow (check_op e)
  | ELambda (ids, body) -> ELambda (ids, check_op body)
  in { p ; e }
