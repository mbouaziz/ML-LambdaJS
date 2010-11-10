open JavaScript
open Prelude
module ES5 = Es5
module ES5s = Es5_syntax
module ES5ds = Es5_desugar
module ES5e = Es5_env
module ES5pp = Es5_pretty
module ES5p = Es5_parser
module ES5eval = Es5_eval
module ES5v = Es5_values


let p = Lexing.dummy_pos, Lexing.dummy_pos

let env_special_id = "**ENV-SPECIAL-ID**"
let env_special_node = ES5s.( { p ; e = EOp1(`Prim1 env_special_id, { p ; e = EConst JavaScript_syntax.CUndefined }) } )

type exptype =
  | Nothing
  | PrimEnv of ES5s.prim_exp
  | SrcExp of ES5s.src_exp
  | PrimExp of ES5s.prim_exp

let eseq e1 e2 = ES5s.( { p ; e = ESeq (e1, e2) } )

let prim_to_src (e : ES5s.prim_exp) : ES5s.src_exp = (e :> ES5s.src_exp)

let primexp = function
  | SrcExp e -> ES5ds.check_op e
  | PrimExp e -> e
  | _ -> assert false
let srcexp = function
  | SrcExp e -> e
  | PrimExp e -> prim_to_src e
  | _ -> assert false

let srcES5 = ref Nothing

let xopen_in path = if path = "STDIN" then stdin else open_in path


let load_js (path : string) : unit = 
  let js = parse_javascript_from_channel (xopen_in path) path in
  let newES5 = ES5ds.ds_top (Exprjs_syntax.from_javascript js) in
  srcES5 := match !srcES5 with
  | Nothing -> SrcExp newES5
  | PrimEnv _ -> failwith "Env applied to nothing followed by a JS file"
  | SrcExp e -> SrcExp (eseq e newES5)
  | PrimExp e -> SrcExp (eseq (prim_to_src e) newES5)


let load_es5 (path : string) : unit =
  let parsed = ES5.parse_es5 (xopen_in path) path in
  srcES5 := match !srcES5 with
  | Nothing -> PrimExp parsed
  | PrimEnv _ -> failwith "Env applied to nothing followed by an ES5 file"
  | SrcExp e -> SrcExp (eseq e (prim_to_src parsed))
  | PrimExp e -> PrimExp (eseq e parsed)


let load_file (path : string) : unit =
  if Filename.check_suffix path ".js" then
    load_js path
  else if Filename.check_suffix path ".es5" then
    load_es5 path
  else 
    failwith (sprintf "Unknown file extention for file \"%s\"; try -js or -ljs" path)


let desugar () : unit =
  srcES5 := match !srcES5 with
  | Nothing -> failwith "Nothing to desugar"
  | PrimEnv e -> PrimEnv (ES5ds.desugar (prim_to_src e))
  | SrcExp e -> PrimExp (ES5ds.desugar e)
  | PrimExp e -> PrimExp (ES5ds.desugar (prim_to_src e))


let apply_env env x = match x with
  | Nothing -> PrimEnv env
  | PrimEnv env0 -> PrimEnv (ES5s.substitute env_special_node env0 env)
  | SrcExp e -> SrcExp (ES5s.substitute env_special_node e (prim_to_src env))
  | PrimExp e -> PrimExp (ES5s.substitute env_special_node e env)


let set_env (s : string) =
  srcES5 := apply_env (ES5e.parse_env (xopen_in s) s env_special_node) !srcES5


let read_env_from_cache_file (s : string) =
  let cin = open_in_bin s in
  let cached = Marshal.from_channel cin in
  let _ = close_in cin in
  srcES5 := match !srcES5, cached with
  | _, Nothing -> failwith ("Nothing in file " ^ s)
  | Nothing, _ -> cached
  | x, PrimEnv env -> apply_env env x
  | SrcExp e1, SrcExp e2 -> SrcExp (eseq e1 e2)
  | SrcExp e1, PrimExp e2 -> SrcExp (eseq e1 (prim_to_src e2))
  | PrimExp e1, SrcExp e2 -> SrcExp (eseq (prim_to_src e1) e2)
  | PrimExp e1, PrimExp e2 -> PrimExp (eseq e1 e2)
  | PrimEnv _, _ -> failwith "Unapplied environment followed by code"


let write_env_to_cache_file (s : string) =
  let cout = open_out_bin s in
  Marshal.to_channel cout !srcES5 [Marshal.Closures];
  close_out cout


let action_pretty () : unit =
  Es5_pretty.exp (srcexp !srcES5) Format.std_formatter;
  print_newline ()


let action_eval () : unit =
  ignore (ES5eval.eval_expr (primexp !srcES5));
  print_newline ()


let action = ref (fun () -> ())

let set_action (thunk : unit -> unit) (() : unit) : unit =
  let prev = !action in
  action :=  fun () -> prev (); thunk ()


let main () : unit =
  Arg.parse
    [ 
      ("-js", Arg.String load_js,
       "<file> Load <file> as JavaScript");

      ("-ljs", Arg.String load_es5,
       "Load <file> as LambdaJS-ES5");

      ("-env", Arg.String set_env,
      "<file> load <file> as environment");

      ("-env-rc", Arg.String read_env_from_cache_file,
       "<file> load <file> as a cached environment");

      ("-env-wc", Arg.String write_env_to_cache_file,
       "<file> write <file> as a cached environment");

      ("-full-desugar", Arg.Unit (set_action desugar), "like it says");

       ("-eval", Arg.Unit (set_action action_eval),
	"run the program");

       ("-pretty", Arg.Unit (set_action action_pretty),
	"Pretty print the current source")
    ]
    load_file
    "Usage: jsc <action> [path] ...";;

let _ =
  Printexc.record_backtrace true;
  try main (); !action () with
    e ->
      print_endline (Printexc.to_string e);
      Printexc.print_backtrace stdout;
      exit 1
