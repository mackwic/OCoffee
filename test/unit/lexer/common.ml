open Tokens

let string_of_value_token = function
  | INT value -> "int(" ^ string_of_int(value) ^ ")"
  | FLOAT value -> "float(" ^ string_of_float(value) ^ ")"
  | BOOL value -> "bool(" ^ string_of_bool(value) ^ ")"
  | _ -> failwith "not a value"

let tokenize input = Lexer.tokenize (Lexing.from_string input)

let assert_same_str expected got =
  OUnit2.assert_equal expected got ~msg:(
    "Expected [" ^ got ^ "] to be [" ^ expected ^"]")
