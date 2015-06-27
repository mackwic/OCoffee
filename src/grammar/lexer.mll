
{
  open Lexing
  open Parser
}

rule token = parse
| eof { EOF }
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
| ['!', "not"] { OPBO_NOT }
| ["&&", "and"] { OPBO_AND }
| ["||", "or"] { OPBO_OR }
| ["==", "is"] { OPBO_EQUAL }
| ["!=", "isnt"] { OPBO_NOT_EQUAL }
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
| "->" { OPF_THIN_ROCKET }
| "=>" { OPF_FAT_ROCKET }
| "|=" { OPAS_ASSING_OR }
| "&=" { OPAS_ASSING_AND }
| "?=" { OPAS_ASSING_IF }
| "+=" { OPAS_ASSING_PLUS }
| "-=" { OPAS_ASSING_MINUS }
| "*=" { OPAS_ASSING_MULTIPLY }
| "/=" { OPAS_ASSING_DIVIDE }
| "%=" { OPAS_ASSING_MODULO }
| "**=" { OPAS_ASSING_EXPONENT }
| "%%=" { OPAS_ASSING_MODULO_POSITIVE }
| "//=" { OPAS_ASSSIGN_DIVIDE_INTEGER }
| "?" { OP_EXISTS }

{
  (* trailer *)
}

