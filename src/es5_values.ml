open Prelude
open JavaScript_syntax
open Es5_syntax

type obj = {
  props : prop IdMap.t;
  proto : obj ref option;
  extensible : bool;
  _class : string;
  code : (value list -> value) option;
}
and prop = {
  value : value option;
  getter : obj ref option;
  setter : obj ref option;
  config : bool;
  writable : bool;
  enum : bool;
}
and value =
  | Const of JavaScript_syntax.const
  | ObjCell of obj ref
  | Closure of (value list -> value)
  | Fail of string

type env = value IdMap.t
type label = string

exception Break of label * value
exception Throw of value

let empty_obj = {
  props = IdMap.empty; proto = None;
  extensible = false; _class = "Object"; code = None;
}
let mk_obj props = {
  props; proto = None;
  extensible = false; _class = "Object"; code = None;
}

let mk_empty_prop b = {
  value = None; getter = None; setter = None;
  config = b; writable = b; enum = b;
}
let empty_prop = mk_empty_prop false
let empty_prop_true = mk_empty_prop true
let mk_data_prop ?(b=false) v = {
  value = Some v; getter = None; setter = None;
  config = b; writable = b; enum = b;
}

let rec pretty_value v = match v with 
  | Const c -> begin match c with
      | CInt d -> string_of_int d
      | CNum d -> string_of_float d
      | CString s -> "\"" ^ s ^ "\""
      | CBool b -> string_of_bool b
      | CUndefined -> "undefined"
      | CNull -> "null"
      | CRegexp (re, g, i) -> "/" ^ re ^ "/" ^ (if g then "g" else "") ^ (if i then "i" else "")
    end
  | Closure c -> "function"
  | ObjCell o -> "object"
  | Fail s -> "[fail: " ^ s ^ "]"

let rec pretty_value_list vs = match vs with
  | (v::vs) -> pretty_value v ^ ", " ^ pretty_value_list vs
  | [] -> ""
