{
open Prelude
open Lexing
open JavaScriptSCI_parser
open JavaScript_syntax

module S = String

let parse_re = ref false

(* TODO: if integer conversions overflow, treat as a float *)
let parse_num_lit (s : string) : token =
  if S.contains s 'x' || S.contains s 'X'
    then TokInt (int_of_string s)
    else if S.contains s '.'
           then TokFloat (float_of_string s)
           else if S.contains s 'e' || S.contains s 'E'
                  then TokFloat (float_of_string s)
                  else TokInt (int_of_string s)

let mk_loc (buf : lexbuf) : pos =
  Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf

let block_comment_buf = Buffer.create 120

let string_buf = Buffer.create 100

let comment_start_p = ref dummy_pos

let get_string () = 
  let s = Buffer.contents string_buf in
    Buffer.clear string_buf;
    s
}

(* dec_digit+ corresponds to DecimalDigits in the spec. *)
let dec_digit = ['0'-'9']

let signed_int = dec_digit+ | ('+' dec_digit+) | ('-' dec_digit+)

let expt_part = ['e' 'E'] signed_int

let dec_int_lit = '0' | (['1'-'9'] dec_digit*)

let hex = ['0'-'9' 'A'-'F' 'a'-'f']

let hex_lit = ("0x" | "0X") hex+

let dec_lit = 
  (dec_int_lit '.' dec_digit* expt_part?) | 
  ('.' dec_digit+ expt_part?) |
  (dec_int_lit expt_part?)

let num_lit = dec_lit | hex_lit

let ident = ['a'-'z' 'A'-'Z' '$' '_']['a'-'z' 'A'-'Z' '0'-'9' '$' '_']*

let digit = ['0'-'9']

let char = [^ '"' '\\']

let blank = [ ' ' '\t' ]

let escape_sequence
  = [^ '\r' '\n'] | ('x' hex hex) | ('u' hex hex hex hex)

let double_quoted_string_char = 
  [^ '\r' '\n' '"' '\\'] | ('\\' escape_sequence)

let line_terminator = "\n\r" | '\n' | "\r\n" | '\r'

rule token = parse
   | blank + { token lexbuf }
   | line_terminator { new_line lexbuf; TokLineTerminator }
   | "/*" { block_comment lexbuf }
   | "//"[^ '\r' '\n']* line_terminator { new_line lexbuf; token lexbuf }

   | "/*:" { parse_re := false; comment_start_p := lexeme_start_p lexbuf;
             hint lexbuf }

   (* ContinueId and BreakId are tokens for labelled break and continue.  They
    * include their target label.
    *)
   | "continue" [ ' ' '\t' ]+ (ident as x) { parse_re := false; TokContinueId x }
   | "break" [ ' ' '\t' ]+ (ident as x) { parse_re := false; TokBreakId x }

   | '/' {if !parse_re then (parse_re := false; regexp lexbuf) else TokDiv }

   | '"' { parse_re := false; string_lit '"' lexbuf }
   | '\'' { parse_re := false; string_lit '\'' lexbuf }
   
   | num_lit as x {  parse_re := false; parse_num_lit x }
   | "{" { parse_re := false; TokLBrace }
   | "}" { parse_re := false; TokRBrace }
   | '(' { parse_re := true; TokLParen }
   | ')' {  parse_re := false; TokRParen }
   | "|=" { parse_re := false; TokAssignOp OpAssignBOr }
   | "^=" { parse_re := false; TokAssignOp OpAssignBXor }
   | "&=" { parse_re := false; TokAssignOp OpAssignBAnd }
   | "<<=" { parse_re := false; TokAssignOp OpAssignLShift }
   | ">>=" { parse_re := false; TokAssignOp OpAssignZfRShift }
   | ">>>=" { parse_re := false; TokAssignOp OpAssignSpRShift }
   | "+=" { parse_re := false; TokAssignOp OpAssignAdd }
   | "-=" { parse_re := false; TokAssignOp OpAssignSub }
   | "*=" { parse_re := false; TokAssignOp OpAssignMul }
   | "/=" { parse_re := false; TokAssignOp OpAssignDiv }
   | "%=" { parse_re := false; TokAssignOp OpAssignMod }
   | "%" { parse_re := false; TokMod }
   | "=" { parse_re := true; TokAssign }
   | ";" { parse_re := false; TokSemi }
   | "," { parse_re := true; TokComma }
   | "?" { parse_re := true; TokQues }
   | ":" { parse_re := true; TokColon }
   | "||" { parse_re := true; TokLOr }
   | "&&" { parse_re := false; TokLAnd }
   | "|" { parse_re := false; TokBOr }
   | "^" { parse_re := false; TokBXor }
   | "&" { parse_re := false; TokBAnd }
   | "===" { parse_re := false; TokStrictEq }
   | "==" { parse_re := false; TokAbstractEq }
   | "!=" { parse_re := false; TokAbstractNEq }
   | "!==" { parse_re := false; TokStrictNEq }
   | "<<" { parse_re := false; TokLShift }
   | ">>" { parse_re := false; TokRShift }
   | ">>>" { parse_re := false; TokSpRShift }
   | "<=" { parse_re := false; TokLEq }
   | "<" { parse_re := false; TokLT }
   | ">=" { parse_re := false; TokGEq }
   | ">" { parse_re := false; TokGT }
   | "++" { parse_re := false; TokPlusPlus }
   | "--" { parse_re := false; TokMinusMinus }
   | "+" { parse_re := false; TokPlus }
   | "-" { parse_re := false; TokMinus }
   | "*" { parse_re := false; TokTimes }
   | "!" { parse_re := true; TokExclamation }
   | "~" { parse_re := false; TokTilde }
   | "." { parse_re := false; TokPeriod }
   | "[" { parse_re := false; TokLBrack }
   | "]" { parse_re := false; TokRBrack }

   | "if" { parse_re := false; TokIf  }
   | "else" { parse_re := false; TokElse  }
   | "true" { parse_re := false; TokTrue  }
   | "false" { parse_re := false; TokFalse  }
   | "new" { parse_re := false; TokNew  }
   | "instanceof" { parse_re := false; TokInstanceof  }
   | "this" { parse_re := false; TokThis  }
   | "null" { parse_re := false; TokNull  }
   | "function" { parse_re := false; TokFunction  }
   | "typeof" { parse_re := false; TokTypeof  }
   | "void" { parse_re := false; TokVoid  }
   | "delete" { parse_re := false; TokDelete  }
   | "switch" { parse_re := false; TokSwitch  }
   | "default" { parse_re := false; TokDefault  }
   | "case" { parse_re := false; TokCase  }
   | "while" { parse_re := false; TokWhile  }
   | "do" { parse_re := false; TokDo  }
   | "break" { parse_re := false; TokBreak  }
   | "var" { parse_re := false; TokVar  }
   | "in" { parse_re := false; TokIn  }
   | "for" { parse_re := false; TokFor  }
   | "try" { parse_re := false; TokTry  }
   | "catch" { parse_re := false; TokCatch  }
   | "finally" { parse_re := false; TokFinally  }
   | "throw" { parse_re := false; TokThrow  }
   | "return" { parse_re := false; TokReturn  }
   | "with" { parse_re := false; TokWith  }
   | "continue" { parse_re := false; TokContinue  }
   | "instanceof" { parse_re := false; TokInstanceof  }
   | "get" { parse_re := false; TokGet }
   | "set" { parse_re := false; TokSet }
   | ident as x { parse_re := false; TokId x }
   | eof { TokEOF }

and block_comment = parse
  | "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | line_terminator { new_line lexbuf; block_comment lexbuf; }
  | [^ '\n' '\r' '*'] { block_comment lexbuf }

and hint = parse
  | "*/" { let str = Buffer.contents block_comment_buf in
             Buffer.clear block_comment_buf; TokHINT str }
  | '*' { Buffer.add_char block_comment_buf '*'; hint lexbuf }
  | line_terminator { new_line lexbuf; Buffer.add_char block_comment_buf '\n'; hint lexbuf }
  | ([^ '\n' '\r' '*'])+ as txt { Buffer.add_string block_comment_buf txt;
                                  hint lexbuf }

and string_lit end_ch = parse
  (* multi-line *)
  | "\\\r" { string_lit end_ch lexbuf }
  | "\\\n" { string_lit end_ch lexbuf }
  (* escape codes *)
  | "\\'"  { Buffer.add_char string_buf '\''; string_lit end_ch lexbuf }
  | "\\\"" { Buffer.add_char string_buf '\"'; string_lit end_ch lexbuf }
  | "\\\\" { Buffer.add_char string_buf '\\'; string_lit end_ch lexbuf }
  | "\\b" { Buffer.add_char string_buf '\b'; string_lit end_ch lexbuf }
  | "\\n" { Buffer.add_char string_buf '\n'; string_lit end_ch lexbuf }
  | "\\r" { Buffer.add_char string_buf '\r'; string_lit end_ch lexbuf }
  | "\\t" { Buffer.add_char string_buf '\t'; string_lit end_ch lexbuf }
  (* NOTE: OCaml does not support Unicode characters. See the OCaml "Batteries"
     for a library that does. *)
  | "\\v" { Buffer.add_char string_buf '\x0B'; string_lit end_ch lexbuf }
  | "\\ " { Buffer.add_char string_buf ' '; string_lit end_ch lexbuf }
  | "\\0" { Buffer.add_char string_buf '\x00'; string_lit end_ch lexbuf }
  | "\\x" (hex hex as ascii)
      { Buffer.add_char string_buf (char_of_int (int_of_string ("0x" ^ ascii)));
        string_lit end_ch lexbuf }
  (* NOTE: This is probably wrong, due to lack of Unicode support. *)
  | "\\u" (hex hex hex hex as uni)
      { Buffer.add_char string_buf (char_of_int (int_of_string ("0x" ^ uni)));
        string_lit end_ch lexbuf }
  | _ as ch
      { if end_ch = ch then
          TokString (get_string ())
        else
          (Buffer.add_char string_buf ch; 
           string_lit end_ch lexbuf)
      }

and regexp = parse
  | "/" { TokRegexp (get_string (), false, false) }
  | "/mg" { TokRegexp (get_string (), true, false) } (* TODO: m-flag ignored *)
  | "/gi" { TokRegexp (get_string (), true, true) }
  | "/g" { TokRegexp (get_string (), true, false) }
  | "/i" { TokRegexp (get_string (), false, true) }
  | '\\' (_ as ch) { Buffer.add_char string_buf ch; regexp lexbuf }
  | _ as ch { Buffer.add_char string_buf ch; regexp lexbuf }
