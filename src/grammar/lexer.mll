
{
  open Lexing
  open Parser
}

let t_white   = ['\t', ' ']
let t_eol     = ['\n', '\r', "\r\n"]
let t_digit   = ['0'-'9']
let t_int     = '-'?t_digit+
let t_float   = '-'?t_digit+'.'t_digit+
let t_bool    = ("true"|"false")
let t_escape  =  '\\' ['b' 't' 'n' 'f' 'r' '"' '/' '\\']
let t_alpha   = ['A'-'Z' 'a'-'z']
let t_alphanum= t_alpha | t_digit
let t_unicode = "\u" t_alphanum t_alphanum t_alphanum t_alphanum
let t_ident   = ['_' t_alpha]['_' t_alphanum]

rule token = parse
| eof { EOF }
| t_int as value { INT(int_of_string value) }
| t_float as value { FLOAT(float_of_string value) }
| t_bool as value { BOOL(bool_of_string value) }
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
| "true" { TRUE }
| "false" { FALSE }
| "null" { NULL }
| "undefined" { UNDEFINED }
(* operators *)
| ["==", "is"] { OPBO_EQUAL }
| ["!=", "isnt"] { OPBO_NOT_EQUAL }
| ['!', "not"] { OPBO_NOT }
| ["&&", "and"] { OPBO_AND }
| ["||", "or"] { OPBO_OR }
| "->" { OPF_THIN_ROCKET }
| "=>" { OPF_FAT_ROCKET }
| "|=" { OPAS_ASSING_OR }
| "&=" { OPAS_ASSING_AND }
| "?=" { OPAS_ASSING_IF }
| "+=" { OPAS_ASSING_PLUS }
| "-=" { OPAS_ASSING_MINUS }
| "*=" { OPAS_ASSING_MULTIPLY }
| "/=" { OPAS_ASSING_DIVIDE }
| "^=" { OPAS_ASSING_AND }
| "&=" { OPAS_ASSING_OR }
| ">>=" { OPAS_ASSING_SHIFTR }
| "<<=" { OPAS_ASSING_SHIFTR }
| "%=" { OPAS_ASSING_MODULO }
| "**=" { OPAS_ASSING_EXPONENT }
| "%%=" { OPAS_ASSING_MODULO_POSITIVE }
| "//=" { OPAS_ASSSIGN_DIVIDE_INTEGER }
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
| '/' { OPAR_DIVIDE }
| "%%" { OPAR_MODULO_POSITIVE }
| '%' { OPAR_MODULO }
| "?" { OP_EXISTS }

and strict_stringify buff = parse
| t_escape as value

{
  (* trailer *)
}

