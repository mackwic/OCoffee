
{
  open Lexing
  open Tokens

  let indent_width = ref None
  let base_indent = ref 0

  let reset =
    indent_width := None;
    base_indent := 0;
    true

  let whitespace lexbuf continue_f =
    let pos = Lexing.lexeme_start_p lexbuf in
    (* find the column of the start of lexeme. If 0, we got indentation, else 
      * we treat it just as a separator *)
    if (pos.pos_cnum - pos.pos_bol) <> 0 then continue_f lexbuf
    else
      let input_width =
        (Lexing.lexeme_end lexbuf) - (Lexing.lexeme_start lexbuf)
      in
      let ref_width = match !indent_width with
        (* first indentation will set the indent width *)
        | None -> indent_width := Some(input_width); input_width
        | Some(width) -> width
      in
        (*if input_width mod ref_width <> 0
        then (* FIXME error *) *)
        let diff_indent = (input_width / ref_width) - !base_indent in
        match diff_indent with
        (* no indentation difference, carry on *)
        | 0 -> continue_f lexbuf
        | 1 -> incr base_indent; INDENT
        | -1 -> decr base_indent; DEDENT (* FIXME assert base_indent > 0 *)
        | _ -> failwith "syntax error: indent error" (* FIXME better error *)
}

let t_white   = ['\t' ' ']
let t_eol     = '\n'|'\r'|"\r\n"
let t_digit   = ['0'-'9']
let t_int     = '-'?t_digit+
let t_float_mantiss = '-'?t_digit+
let t_float_decimal = '.'t_digit+
let t_float_exponent = 'e't_digit+
let t_float   = "NaN"|('-'?"Infinity")|(t_float_mantiss? t_float_decimal)|
  (t_float_mantiss t_float_decimal t_float_exponent?)|
  (t_float_mantiss t_float_exponent)|
  (t_float_decimal t_float_exponent)
let t_bool    = ("true"|"false")
let t_escape  =  '\\' ['b' 't' 'n' 'f' 'r' '"' '/' '\\']
let t_alpha   = ['A'-'Z' 'a'-'z']
let t_accents =
  "à"|"á"|"â"|"ã"|"ä"|"è"|"é"|"ê"|"ë"|"ì"|"í"|"î"|"ï"|"ò"|"ó"|"ô"|"õ"|"ö"|"ù"|
  "ú"|"û"|"ü"|"ÿ"|"æ"|"Æ"|
  "À"|"Á"|"Â"|"Ã"|"Ä"|"È"|"É"|"Ê"|"Ë"|"Ì"|"Í"|"Î"|"Ï"|"Ò"|"Ó"|"Ô"|"Õ"|"Ö"|"Ù"|
  "Ú"|"Û"|"Ü"|"Ÿ"
let t_symbols =
  "$"|"£"|"€"|"¡"|"™"|"£"|"¢"|"∞"|"§"|"¶"|"•"|"ª"|"º"|"–"|"≠"|"“"|"‘"|"”"|"’"
  
let t_alphanum= t_alpha | t_digit | t_accents
let t_unicode = "\\u" t_alphanum t_alphanum t_alphanum t_alphanum
let t_ident   = ('_'|t_alpha|t_symbols|t_accents)('_'|t_alphanum|t_symbols)*

rule tokenize = parse
| eof { EOF }
| t_float as value { FLOAT(float_of_string value) }
| t_int as value { INT(int_of_string value) }
| t_bool as value { BOOL(bool_of_string value) }
| t_white+ { whitespace lexbuf tokenize }
(* punctuation *)
| ';' { SEMICOLON}
| "::" { DOUBLE_COMMA }
| ':' { COMMA }
| "..." { TRIPLE_DOT }
| ".." { DOUBLE_DOT }
| '.' { DOT }
| '{' { L_BRACE }
| '}' { R_BRACE }
| '[' { L_BRACK }
| ']' { R_BRACK }
| '@' { AT }
(* keywords *)
| "yield" { YIELD }
| "new" { NEW }
| "this" { THIS }
| "try" { TRY }
| "catch" { CATCH }
| "finally" { FINALLY }
| "class" { CLASS }
| "extends" { EXTENDS }
| "super" { SUPER }
| "if" { IF }
| "unless" { UNLESS }
| "then" { THEN }
| "else" { ELSE }
| "switch" { SWITCH }
| "when" { WHEN }
| "while" { WHILE }
| "until" { UNTIL }
| "for" { FOR }
| "in" { FOR_IN }
| "of" { FOR_OF }
| "do" { FOR_DO }
| "break" { BREAK }
| "continue" { CONTINUE }
| "return" { RETURN }
| "true" { BOOL(true) }
| "false" { BOOL(false) }
| "null" { NULL }
| "undefined" { UNDEFINED }
(* operators *)
| "=="|"is" { OPBO_EQUAL }
| "!="|"isnt" { OPBO_NOT_EQUAL }
| '!'|"not" { OPBO_NOT }
| "&&"|"and" { OPBO_AND }
| "||"|"or" { OPBO_OR }
| "->" { OPF_THIN_ROCKET }
| "=>" { OPF_FAT_ROCKET }
| "||=" { OPAS_ASSIGN_OR }
| "&&=" { OPAS_ASSIGN_AND }
| "?=" { OPAS_ASSIGN_IF }
| "+=" { OPAS_ASSIGN_PLUS }
| "-=" { OPAS_ASSIGN_MINUS }
| "*=" { OPAS_ASSIGN_MULTIPLY }
| "/=" { OPAS_ASSIGN_DIVIDE }
| "^=" { OPAS_ASSIGN_OR_BIN }
| "&=" { OPAS_ASSIGN_AND_BIN }
| ">>=" { OPAS_ASSIGN_SHIFTR }
| "<<=" { OPAS_ASSIGN_SHIFTL }
| "%=" { OPAS_ASSIGN_MODULO }
| "**=" { OPAS_ASSIGN_EXPONENT }
| "%%=" { OPAS_ASSIGN_MODULO_POSITIVE }
| "//=" { OPAS_ASSIGN_DIVIDE_INTEGER }
| ">=" { OPBO_GREATER_EQUAL }
| ">>" { OPBI_SHIFTR }
| '>' { OPBO_GREATER }
| "<=" { OPBO_LESS_EQUAL }
| "<<" { OPBI_SHIFTL }
| '<' { OPBO_LESS }
| '^' { OPBI_OR }
| '&' { OPBI_AND }
| '+'  { OPAR_PLUS }
| '-' { OPAR_MINUS }
| "**" { OPAR_EXPONENT }
| '*' { OPAR_MULTIPLY }
| "//" { OPAR_DIVIDE_INTERGER }
| '/' { OPAR_SLASH } (* XXX: both arithmetic division AND regexp delimiter ! *)
| "%%" { OPAR_MODULO_POSITIVE }
| '%' { OPAR_MODULO }
| '?' { OP_EXISTS }
| "\"\"\"" { TRIPLE_DOUBLE_QUOTE } 
| "'''" { TRIPLE_SIMPLE_QUOTE }
| "###" { TRIPLE_HASH }
| '\'' { SIMPLE_QUOTE }
| '"' { DOUBLE_QUOTE }
| "#{" { START_INTERPOLATE }
| '#' { commentify (Buffer.create 20) lexbuf }
| t_eol+ { tokenize lexbuf }
| t_ident as value { ID(value) }

and commentify buff = parse
| t_eol { COMMENT (Buffer.contents buff) }
| eof { COMMENT (Buffer.contents buff) }
| _ as c { Buffer.add_char buff c; commentify buff lexbuf }

(*
and base_stringify end_char buff = parse
| t_escape as value
  { Buffer.add_string buff (Scanf.unescaped value); stringify buff lexbuf }
| t_unicode as value
   { Buffer.add_string buff (Utf8.to_utf8 value); stringify buff lexbuf }
(* skip invalid escape codes *)
| '\\' { stringify buff lexbuf }
| eof { failwith Errors.UnterminatedString }
| end_char { STRING (Buffer.contents buff) }
| _ as c { Buffer.add_char buff c; stringify buff lexbuf }
*)

{
  (* trailer *)
}

