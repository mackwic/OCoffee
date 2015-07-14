
let tokenize input = Lexer.tokenize (Lexing.from_string input)

let assert_same_str expected got =
  OUnit2.assert_equal expected got ~msg:(
    "Expected [" ^ got ^ "] to be [" ^ expected ^"]")
