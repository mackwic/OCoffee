open Tokens

module EToken =
struct
  type t = Tokens.token
  let compare a b = if a = b then 0 else 1 
  let pp_printer = PrintTokens.pp_print_token
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module ListTokens = OUnitDiff.ListSimpleMake(EToken)

let tokenize input = Lexer.tokenize (Lexing.from_string input)

let assert_same_str expected got =
  OUnit2.assert_equal expected got ~msg:(
    "Expected [" ^ got ^ "] to be [" ^ expected ^"]")
