
{
  open Lexing
  open Tokens

  let log = new LogO.logger __MODULE__

  let reset () = LexerIndent.reset ()

  let emit_log ?(show_emit = true) pos str =
    if true
    then log#info pos ((if show_emit then "Emit " else "") ^ str)
  let emit pos tok =
    emit_log pos (PrintTokens.string_of_token tok);
    tok
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
| eof { emit __POS__ EOF }
| t_float as value { emit __POS__ (FLOAT (float_of_string value)) }
| t_int as value { emit __POS__ (INT (int_of_string value)) }
| t_white+ as value {
  emit_log ~show_emit:false __POS__ ("(value is '" ^ value ^ "')");
  let tok = match LexerIndent.whitespace lexbuf with 
  | LexerIndent.W_INDENT -> INDENT
  | LexerIndent.W_DEDENT -> DEDENT
  | _ ->
      emit_log __POS__ "(bubble-ing next emit)";
      let len = (String.length value) in
      tokenize {lexbuf with
        lex_curr_pos = lexbuf.lex_curr_pos + len;
        lex_abs_pos = lexbuf.lex_abs_pos + len;
        lex_start_pos = lexbuf.lex_start_pos + len;
        lex_curr_p = {lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum + len}
      }
  in
  emit __POS__ tok
}
(* punctuation *)
| ';' { emit __POS__ SEMICOLON}
| "::" { emit __POS__ DOUBLE_COMMA }
| ':' { emit __POS__ COMMA }
| "..." { emit __POS__ TRIPLE_DOT }
| ".." { emit __POS__ DOUBLE_DOT }
| '.' { emit __POS__ DOT }
| '{' { emit __POS__ L_BRACE }
| '}' { emit __POS__ R_BRACE }
| '[' { emit __POS__ L_BRACK }
| ']' { emit __POS__ R_BRACK }
| '@' { emit __POS__ AT }
(* keywords *)
| "yield" { emit __POS__ YIELD }
| "new" { emit __POS__ NEW }
| "this" { emit __POS__ THIS }
| "try" { emit __POS__ TRY }
| "catch" { emit __POS__ CATCH }
| "finally" { emit __POS__ FINALLY }
| "class" { emit __POS__ CLASS }
| "extends" { emit __POS__ EXTENDS }
| "super" { emit __POS__ SUPER }
| "if" { emit __POS__ IF }
| "unless" { emit __POS__ UNLESS }
| "then" { emit __POS__ THEN }
| "else" { emit __POS__ ELSE }
| "switch" { emit __POS__ SWITCH }
| "when" { emit __POS__ WHEN }
| "while" { emit __POS__ WHILE }
| "until" { emit __POS__ UNTIL }
| "for" { emit __POS__ FOR }
| "in" { emit __POS__ FOR_IN }
| "of" { emit __POS__ FOR_OF }
| "do" { emit __POS__ FOR_DO }
| "break" { emit __POS__ BREAK }
| "continue" { emit __POS__ CONTINUE }
| "return" { emit __POS__ RETURN }
| "true" { emit __POS__ (BOOL(true)) }
| "false" { emit __POS__ (BOOL(false)) }
| "null" { emit __POS__ NULL }
| "undefined" { emit __POS__ UNDEFINED }
(* operators *)
| "=="|"is" { emit __POS__ OPBO_EQUAL }
| "!="|"isnt" { emit __POS__ OPBO_NOT_EQUAL }
| '!'|"not" { emit __POS__ OPBO_NOT }
| "&&"|"and" { emit __POS__ OPBO_AND }
| "||"|"or" { emit __POS__ OPBO_OR }
| "->" { emit __POS__ OPF_THIN_ROCKET }
| "=>" { emit __POS__ OPF_FAT_ROCKET }
| "||=" { emit __POS__ OPAS_ASSIGN_OR }
| "&&=" { emit __POS__ OPAS_ASSIGN_AND }
| "?=" { emit __POS__ OPAS_ASSIGN_IF }
| "+=" { emit __POS__ OPAS_ASSIGN_PLUS }
| "-=" { emit __POS__ OPAS_ASSIGN_MINUS }
| "*=" { emit __POS__ OPAS_ASSIGN_MULTIPLY }
| "/=" { emit __POS__ OPAS_ASSIGN_DIVIDE }
| "^=" { emit __POS__ OPAS_ASSIGN_OR_BIN }
| "&=" { emit __POS__ OPAS_ASSIGN_AND_BIN }
| ">>=" { emit __POS__ OPAS_ASSIGN_SHIFTR }
| "<<=" { emit __POS__ OPAS_ASSIGN_SHIFTL }
| "%=" { emit __POS__ OPAS_ASSIGN_MODULO }
| "**=" { emit __POS__ OPAS_ASSIGN_EXPONENT }
| "%%=" { emit __POS__ OPAS_ASSIGN_MODULO_POSITIVE }
| "//=" { emit __POS__ OPAS_ASSIGN_DIVIDE_INTEGER }
| ">=" { emit __POS__ OPBO_GREATER_EQUAL }
| ">>" { emit __POS__ OPBI_SHIFTR }
| '>' { emit __POS__ OPBO_GREATER }
| "<=" { emit __POS__ OPBO_LESS_EQUAL }
| "<<" { emit __POS__ OPBI_SHIFTL }
| '<' { emit __POS__ OPBO_LESS }
| '^' { emit __POS__ OPBI_OR }
| '&' { emit __POS__ OPBI_AND }
| '+'  { emit __POS__ OPAR_PLUS }
| '-' { emit __POS__ OPAR_MINUS }
| "**" { emit __POS__ OPAR_EXPONENT }
| '*' { emit __POS__ OPAR_MULTIPLY }
| "//" { emit __POS__ OPAR_DIVIDE_INTERGER }
| '/' { emit __POS__ OPAR_SLASH } (* XXX: both arithmetic division AND regexp delimiter ! *)
| "%%" { emit __POS__ OPAR_MODULO_POSITIVE }
| '%' { emit __POS__ OPAR_MODULO }
| '?' { emit __POS__ OP_EXISTS }
| "\"\"\"" { emit __POS__ TRIPLE_DOUBLE_QUOTE } 
| "'''" { emit __POS__ TRIPLE_SIMPLE_QUOTE }
| "###" { emit __POS__ TRIPLE_HASH }
| '\'' { emit __POS__ SIMPLE_QUOTE }
| '"' { emit __POS__ DOUBLE_QUOTE }
| "#{" { emit __POS__ START_INTERPOLATE }
| '#' { emit __POS__ (commentify (Buffer.create 20) lexbuf) }
| t_eol {
  emit_log __POS__ "newline (bubble-ing next emit)";
  Lexing.new_line lexbuf;
  tokenize lexbuf
}
| t_ident as value { emit __POS__ (ID(value)) }

and commentify buff = parse
| t_eol { Lexing.new_line lexbuf; COMMENT (Buffer.contents buff) }
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

