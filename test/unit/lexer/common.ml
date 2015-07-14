open Tokens

module EToken =
struct
  type t = Tokens.token
  let compare a b = if a = b then 0 else 1 
  let pp_printer formatter _ = Format.fprintf formatter "token"
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module ListTokens = OUnitDiff.ListSimpleMake(EToken)

let string_of_value_token = function
  | INT value -> "int(" ^ string_of_int(value) ^ ")"
  | FLOAT value -> "float(" ^ string_of_float(value) ^ ")"
  | BOOL value -> "bool(" ^ string_of_bool(value) ^ ")"
  | _ -> failwith "not a value"

let string_of_white_token = function
  | INDENT -> "indent"
  | DEDENT -> "dedent"
  | _ -> failwith "not a whitespace"

let tokenize input = Lexer.tokenize (Lexing.from_string input)

let assert_same_str expected got =
  OUnit2.assert_equal expected got ~msg:(
    "Expected [" ^ got ^ "] to be [" ^ expected ^"]")
