open OUnit2
open Tokens

type expected_pair = string * Tokens.token

let keywords = [
  (";", SEMICOLON);
  ("::", DOUBLE_COMMA);
  (":", COMMA);
  ("...", TRIPLE_DOT);
  ("..", DOUBLE_DOT);
  (".", DOT);
  ("{", L_BRACE);
  ("}", R_BRACE);
  ("[", L_BRACK);
  ("]", R_BRACK);
  ("@", AT);
  ("yield", YIELD);
  ("new", NEW);
  ("this", THIS);
  ("try", TRY);
  ("catch", CATCH);
  ("finally", FINALLY);
  ("class", CLASS);
  ("extends", EXTENDS);
  ("super", SUPER);
  ("if", IF);
  ("unless", UNLESS);
  ("then", THEN);
  ("else", ELSE);
  ("switch", SWITCH);
  ("when", WHEN);
  ("while", WHILE);
  ("until", UNTIL);
  ("for", FOR);
  ("in", FOR_IN);
  ("of", FOR_OF);
  ("do", FOR_DO);
  ("break", BREAK);
  ("continue", CONTINUE);
  ("return", RETURN);
  ("true", BOOL(true));
  ("false", BOOL(false));
  ("null", NULL);
  ("undefined", UNDEFINED);
  ("==", OPBO_EQUAL);
  ("is", OPBO_EQUAL);
  ("!=", OPBO_NOT_EQUAL);
  ("isnt", OPBO_NOT_EQUAL);
  ("!", OPBO_NOT);
  ("not", OPBO_NOT);
  ("&&", OPBO_AND);
  ("and", OPBO_AND);
  ("||", OPBO_OR);
  ("or", OPBO_OR);
  ("->", OPF_THIN_ROCKET);
  ("=>", OPF_FAT_ROCKET);
  ("||=", OPAS_ASSIGN_OR);
  ("&&=", OPAS_ASSIGN_AND);
  ("?=", OPAS_ASSIGN_IF);
  ("+=", OPAS_ASSIGN_PLUS);
  ("-=", OPAS_ASSIGN_MINUS);
  ("*=", OPAS_ASSIGN_MULTIPLY);
  ("/=", OPAS_ASSIGN_DIVIDE);
  ("^=", OPAS_ASSIGN_OR_BIN);
  ("&=", OPAS_ASSIGN_AND_BIN);
  (">>=", OPAS_ASSIGN_SHIFTR);
  ("<<=", OPAS_ASSIGN_SHIFTL);
  ("%=", OPAS_ASSIGN_MODULO);
  ("**=", OPAS_ASSIGN_EXPONENT);
  ("%%=", OPAS_ASSIGN_MODULO_POSITIVE);
  ("//=", OPAS_ASSIGN_DIVIDE_INTEGER);
  (">=", OPBO_GREATER_EQUAL);
  (">>", OPBI_SHIFTR);
  (">", OPBO_GREATER);
  ("<=", OPBO_LESS_EQUAL);
  ("<<", OPBI_SHIFTL);
  ("<", OPBO_LESS);
  ("^", OPBI_OR);
  ("&", OPBI_AND);
  ("+",  OPAR_PLUS);
  ("-", OPAR_MINUS);
  ("**", OPAR_EXPONENT);
  ("*", OPAR_MULTIPLY);
  ("//", OPAR_DIVIDE_INTERGER);
  ("/", OPAR_SLASH );
  ("%%", OPAR_MODULO_POSITIVE);
  ("%", OPAR_MODULO);
  ("?", OP_EXISTS);
  ("\"\"\"", TRIPLE_DOUBLE_QUOTE );
  ("'''", TRIPLE_SIMPLE_QUOTE);
  ("###", TRIPLE_HASH);
  ("\"", DOUBLE_QUOTE);
  ("'", SIMPLE_QUOTE);
  ("#{", START_INTERPOLATE)
]

let keywords_suite =
  "Keywords Suite" >::: List.map (fun (input, expected) ->
    ("Test input: [" ^ input ^ "]") >:: (fun _ -> 
      assert_equal expected (Lexer.tokenize (Lexing.from_string input))
    )
  ) keywords

let suite = keywords_suite

