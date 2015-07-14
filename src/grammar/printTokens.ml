open Tokens

(* START: stolen from github.com/mackwic/toml src/tomlrinter.ml *)
let maybe_escape_char formatter ch =
  match ch with
  | '"'  -> Format.pp_print_string formatter "\\\""
  | '\\' -> Format.pp_print_string formatter "\\\\"
  | '\n' -> Format.pp_print_string formatter "\\n"
  | '\t' -> Format.pp_print_string formatter "\\t"
  | _    ->
    let code = Char.code ch in
    if code <= 31
    then Format.fprintf formatter "\\u%04x" code
    else Format.pp_print_char formatter ch

let print_bool formatter value = Format.pp_print_bool formatter value

let print_int formatter value = Format.pp_print_int formatter value

let print_float formatter value =
  let fractional = abs_float (value -. (floor value)) in
  (* Even 1.'s fractional value is not equal to 0. *)
  if fractional <= epsilon_float then
    Format.fprintf formatter "%.1f" value
  else
    Format.pp_print_float formatter value

let print_string formatter value =
  Format.pp_print_char formatter '"' ;
  String.iter (maybe_escape_char formatter) value ;
  Format.pp_print_char formatter '"'

(*
* This function is a shim for Format.pp_print_list from ocaml 4.02
*)
let pp_print_list ~pp_sep print_item_func formatter values =
  match values with
  | []    -> ()
  | e::[] -> print_item_func formatter e
  | e::l  ->
    print_item_func formatter e;
    List.iter (fun v -> pp_sep formatter (); print_item_func formatter v) l

(* END stolen *)

let print_type formatter typeName value =
  Format.fprintf formatter "%s(" typeName;
  print_string formatter value;
  Format.fprintf formatter ")"

let print_token formatter token = Format.fprintf formatter "[%s]" token

let pp_print_token formatter = function
| EOF -> Format.pp_print_string formatter "EOF"
| FLOAT(value) -> print_float formatter value
| INT(value) -> print_int formatter value
| BOOL(value) -> print_bool formatter value
| COMMENT(value) -> print_type formatter "comment" value
| ID(value) -> print_type formatter "id" value
| INDENT -> print_token formatter "indent"
| DEDENT -> print_token formatter "dedent"
| SEMICOLON -> print_token formatter ";"
| DOUBLE_COMMA  -> print_token formatter "::"
| COMMA  -> print_token formatter ":"
| TRIPLE_DOT  -> print_token formatter "..."
| DOUBLE_DOT  -> print_token formatter ".."
| DOT  -> print_token formatter "."
| L_BRACE  -> print_token formatter "{"
| R_BRACE  -> print_token formatter "}"
| L_BRACK  -> print_token formatter "["
| R_BRACK  -> print_token formatter "]"
| AT  -> print_token formatter "@"
| YIELD  -> print_token formatter "yield"
| NEW  -> print_token formatter "new"
| THIS  -> print_token formatter "this"
| TRY  -> print_token formatter "try"
| CATCH  -> print_token formatter "catch"
| FINALLY  -> print_token formatter "finally"
| CLASS  -> print_token formatter "class"
| EXTENDS  -> print_token formatter "extends"
| SUPER  -> print_token formatter "super"
| IF  -> print_token formatter "if"
| UNLESS  -> print_token formatter "unless"
| THEN  -> print_token formatter "then"
| ELSE  -> print_token formatter "else"
| SWITCH  -> print_token formatter "switch"
| WHEN  -> print_token formatter "when"
| WHILE  -> print_token formatter "while"
| UNTIL  -> print_token formatter "until"
| FOR  -> print_token formatter "for"
| FOR_IN  -> print_token formatter "in"
| FOR_OF  -> print_token formatter "of"
| FOR_DO  -> print_token formatter "do"
| BREAK  -> print_token formatter "break"
| CONTINUE  -> print_token formatter "continue"
| RETURN  -> print_token formatter "return"
| NULL  -> print_token formatter "null"
| UNDEFINED  -> print_token formatter "undefined"
| OPBO_EQUAL  -> print_token formatter "=="
| OPBO_NOT_EQUAL  -> print_token formatter "!="
| OPBO_NOT  -> print_token formatter "!"
| OPBO_AND  -> print_token formatter "&&"
| OPBO_OR  -> print_token formatter "or"
| OPF_THIN_ROCKET  -> print_token formatter "->"
| OPF_FAT_ROCKET  -> print_token formatter "=>"
| OPAS_ASSIGN_OR  -> print_token formatter "||="
| OPAS_ASSIGN_AND  -> print_token formatter "&&="
| OPAS_ASSIGN_IF  -> print_token formatter "?="
| OPAS_ASSIGN_PLUS  -> print_token formatter "+="
| OPAS_ASSIGN_MINUS  -> print_token formatter "-="
| OPAS_ASSIGN_MULTIPLY  -> print_token formatter "*="
| OPAS_ASSIGN_DIVIDE  -> print_token formatter "/="
| OPAS_ASSIGN_OR_BIN  -> print_token formatter "^="
| OPAS_ASSIGN_AND_BIN  -> print_token formatter "&="
| OPAS_ASSIGN_SHIFTR  -> print_token formatter ">>="
| OPAS_ASSIGN_SHIFTL  -> print_token formatter "<<="
| OPAS_ASSIGN_MODULO  -> print_token formatter "%="
| OPAS_ASSIGN_EXPONENT  -> print_token formatter "**="
| OPAS_ASSIGN_MODULO_POSITIVE  -> print_token formatter "%%="
| OPAS_ASSIGN_DIVIDE_INTEGER  -> print_token formatter "//="
| OPBO_GREATER_EQUAL  -> print_token formatter ">="
| OPBI_SHIFTR  -> print_token formatter ">>"
| OPBO_GREATER  -> print_token formatter ">"
| OPBO_LESS_EQUAL  -> print_token formatter "<="
| OPBI_SHIFTL  -> print_token formatter "<<"
| OPBO_LESS  -> print_token formatter "<"
| OPBI_OR  -> print_token formatter "^"
| OPBI_AND  -> print_token formatter "&"
| OPAR_PLUS  -> print_token formatter "+" 
| OPAR_MINUS  -> print_token formatter "-"
| OPAR_EXPONENT  -> print_token formatter "**"
| OPAR_MULTIPLY  -> print_token formatter "*"
| OPAR_DIVIDE_INTERGER  -> print_token formatter "//"
| OPAR_SLASH  -> print_token formatter "/"
| OPAR_MODULO_POSITIVE  -> print_token formatter "%%"
| OPAR_MODULO  -> print_token formatter "%"
| OP_EXISTS  -> print_token formatter "?"
| TRIPLE_DOUBLE_QUOTE   -> print_token formatter "\"\"\""
| TRIPLE_SIMPLE_QUOTE  -> print_token formatter "'''"
| TRIPLE_HASH  -> print_token formatter "###"
| SIMPLE_QUOTE  -> print_token formatter "'"
| DOUBLE_QUOTE  -> print_token formatter "\""
| START_INTERPOLATE  -> print_token formatter "#{"
| _ -> failwith "ooops"

let string_of_token token =
  let buff = Buffer.create 20 in
    pp_print_token (Format.formatter_of_buffer buff) token;
    Buffer.contents buff

