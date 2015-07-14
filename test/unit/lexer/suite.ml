
open OUnit2

let _ =
  let suites = "OCoffe Lexer Suite" >::: [
    Terminals.suite
  ] in
  run_test_tt_main suites
