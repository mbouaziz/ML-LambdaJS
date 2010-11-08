open JavaScript
open Exprjs
open Prelude
open Printf
open Format
open Lambdajs_cps
open Exprjs_syntax
open Format
open Lambdajs_lattice
open Lambdajs_env
open Lambdajs_syntax
open Lambdajs_eval
open Lexing
module ES5 = Es5
module ES5s = Es5_syntax
module ES5ds = Es5_desugar
module ES5e = Es5_env
module ES5pp = Es5_pretty
module ES5p = Es5_parser
module ES5eval = Es5_eval
module ES5v = Es5_values

module H = Hashtbl

let p = (Lexing.dummy_pos, Lexing.dummy_pos)

let env_special_id = "**ENV-SPECIAL-ID**"
let env_special_node = ES5s.EOp1(p, `Prim1 env_special_id, ES5s.EConst (p, JavaScript_syntax.CUndefined))

type exptype =
  | Nothing
  | PrimEnv of ES5s.prim_exp
  | SrcExp of ES5s.src_exp
  | PrimExp of ES5s.prim_exp

let prim_to_src (e : ES5s.prim_exp) : ES5s.src_exp = (e :> ES5s.src_exp)

let primexp = function
  | SrcExp e -> ES5ds.check_op e
  | PrimExp e -> e
  | _ -> assert false
let srcexp = function
  | SrcExp e -> e
  | PrimExp e -> prim_to_src e
  | _ -> assert false

let srcLJS = ref (EConst (p, JavaScript_syntax.CUndefined))
let srcES5 = ref Nothing
let lang = ref "es5"

let action_set_lang (lang_in : string) : unit = 
  match lang_in with
    | "ljs" -> lang := "ljs"
    | "es5" -> lang := "es5"
    | _ -> failwith ("unknown language: " ^ lang_in)
     
 
let xopen_in path = if path = "STDIN" then stdin else open_in path


let load_js (path : string) : unit = 
  let js = parse_javascript_from_channel (xopen_in path) path in
    match !lang with
      |	"ljs" ->
	  srcLJS := 
	    ESeq (p, !srcLJS,
		  Lambdajs_syntax.desugar (Exprjs_syntax.from_javascript js))
      | "es5" ->
	  let newES5 = ES5ds.ds_top (Exprjs_syntax.from_javascript js) in
	  srcES5 := begin match !srcES5 with
	  | Nothing -> SrcExp newES5
	  | PrimEnv _ -> failwith "Env applied to nothing followed by a JS file"
	  | SrcExp e -> SrcExp (ES5s.ESeq (p, e, newES5))
	  | PrimExp e -> SrcExp (ES5s.ESeq (p, prim_to_src e, newES5))
	  end
      | _ -> failwith ("Unknown language: " ^ !lang)

let load_lambdajs (path : string) : unit =
  srcLJS := ESeq (p, !srcLJS,
		  Lambdajs.parse_lambdajs (xopen_in path) path)
    
let load_es5 (path : string) : unit =
  let parsed = ES5.parse_es5 (xopen_in path) path in
  srcES5 := match !srcES5 with
  | Nothing -> PrimExp parsed
  | PrimEnv _ -> failwith "Env applied to nothing followed by an ES5 file"
  | SrcExp e -> SrcExp (ES5s.ESeq (p, e, prim_to_src parsed))
  | PrimExp e -> PrimExp (ES5s.ESeq (p, e, parsed))

let load_file (path : string) : unit =
  if Filename.check_suffix path ".js" then
    load_js path
  else if Filename.check_suffix path ".jsl" then
    load_lambdajs path
  else if Filename.check_suffix path ".es5" then
    load_es5 path
  else 
    failwith ("unknown file extention; try -js or -jsl")

let desugar () : unit =
  match !lang with
    | "ljs" -> srcLJS := Lambdajs_desugar.desugar_op !srcLJS
    | "es5" -> srcES5 := begin match !srcES5 with
      | Nothing -> failwith "Nothing to desugar"
      | PrimEnv e -> PrimEnv (ES5ds.desugar (prim_to_src e))
      | SrcExp e -> PrimExp (ES5ds.desugar e)
      | PrimExp e -> PrimExp (ES5ds.desugar (prim_to_src e))
      end
    | _ -> failwith ("Unknown language: " ^ !lang)

let apply_env env x = match x with
  | Nothing -> PrimEnv env
  | PrimEnv env0 -> PrimEnv (ES5s.substitute env_special_node env0 env)
  | SrcExp e -> SrcExp (ES5s.substitute env_special_node e (prim_to_src env))
  | PrimExp e -> PrimExp (ES5s.substitute env_special_node e env)

let set_env (s : string) =
  match !lang with
    | "ljs" -> srcLJS := enclose_in_env (parse_env (xopen_in s) s) !srcLJS
    | "es5" -> srcES5 := apply_env (ES5e.parse_env (xopen_in s) s env_special_node) !srcES5
    | _ -> failwith ("Unknown language: " ^ !lang)

let read_env_from_cache_file (s : string) =
  match !lang with
  | "ljs" -> failwith ("Read env from cache not implemented for " ^ !lang)
  | "es5" ->
      let cin = open_in_bin s in
      let cached = Marshal.from_channel cin in
      let _ = close_in cin in
      srcES5 := begin match !srcES5, cached with
      | _, Nothing -> failwith ("Nothing in file " ^ s)
      | Nothing, _ -> cached
      | x, PrimEnv env -> apply_env env x
      | SrcExp e1, SrcExp e2 -> SrcExp (ES5s.ESeq (p, e1, e2))
      | SrcExp e1, PrimExp e2 -> SrcExp (ES5s.ESeq (p, e1, prim_to_src e2))
      | PrimExp e1, SrcExp e2 -> SrcExp (ES5s.ESeq (p, prim_to_src e1, e2))
      | PrimExp e1, PrimExp e2 -> PrimExp (ES5s.ESeq (p, e1, e2))
      | PrimEnv _, _ -> failwith "Unapplied environment followed by code"
      end
  | _ -> failwith ("Unknown language: " ^ !lang)

let write_env_to_cache_file (s : string) =
  match !lang with
  | "ljs" -> failwith ("Writing env to cache not implemented for " ^ !lang)
  | "es5" ->
      let cout = open_out_bin s in
      Marshal.to_channel cout !srcES5 [Marshal.Closures];
      close_out cout
  | _ -> failwith ("Unknown language: " ^ !lang)

let action_pretty () : unit =
  match !lang with
    | "ljs" -> failwith ("Pretty not implemented for " ^ !lang)
    | "es5" -> Es5_pretty.exp (srcexp !srcES5) std_formatter;
	print_newline ()
    | _ -> failwith ("Unknown language: " ^ !lang)

let action_cps () : unit =
  let cpslambdajs = Lambdajs_cps.cps !srcLJS in
    Lambdajs_cps.p_cpsexp cpslambdajs std_formatter

let action_eval () : unit =
  match !lang with
    | "es5" -> ignore (ES5eval.eval_expr (primexp !srcES5)); print_newline ()
    | _ -> failwith ("Not implemented for language: " ^ !lang)

let action_operators () : unit =
  let ops = operators !srcLJS in
    IdSet.p_set text ops std_formatter;
    print_newline ()
      
let action = ref (fun () -> ())

let set_action (thunk : unit -> unit) (() : unit) : unit =
  let prev = !action in
  action :=  fun () -> prev (); thunk ()

let main () : unit =
  Arg.parse
    [ 
      ("-lang", Arg.String action_set_lang,
       "(ljs|es5) select the language to use.  Not all operations work with
all languages");

      ("-js", Arg.String load_js,
       "<file> Load <file> as JavaScript");

      ("-jsl", Arg.String load_lambdajs,
       "Load <file> as LambdaJS");

      ("-env", Arg.String set_env,
      "<file> load <file> as environment");

      ("-env-rc", Arg.String read_env_from_cache_file,
       "<file> load <file> as a cached environment");

      ("-env-wc", Arg.String write_env_to_cache_file,
       "<file> write <file> as a cached environment");

      ("-full-desugar", Arg.Unit (set_action desugar), "like it says");

      ("-operators", Arg.Unit (set_action action_operators),
       "list operators used");

       ("-cps", Arg.Unit (set_action action_cps),
       "convert program to CPS");

       ("-eval", Arg.Unit (set_action action_eval),
	"run the program");

       ("-pretty", Arg.Unit (set_action action_pretty),
	"Pretty print the current source")
    ]
    load_file
    "Usage: jsc <action> [path] ...";;

let _ =
  Printexc.record_backtrace true;
  let _ = try main (); !action () with
    e ->
      print_endline (Printexc.to_string e);
      Printexc.print_backtrace stdout
  in
  pp_print_flush std_formatter ();
  pp_print_flush err_formatter ()
