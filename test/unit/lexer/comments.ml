open Common
open OUnit2
open Tokens

let rec contains_comment str_expected lexbuf =
  match Lexer.tokenize lexbuf with
  | COMMENT(value) -> assert_same_str str_expected value
  | EOF -> assert_failure "Not lexed as a comment !"
  | _ -> contains_comment str_expected lexbuf

let words = [
  ("#hey you", "hey you");
  ("## I just met you", "# I just met you");
  ("# But here's my number #", " But here's my number #");
  ("#  So call me maybe ##### yeah maybe    ",
    "  So call me maybe ##### yeah maybe    ");
  ("1+1 # comment 1+1", " comment 1+1");
  ("1+2\n#comment 1+2", "comment 1+2");
  ("# comment \n1+1", " comment ");
  ("1+1
  1+2
  1+3
  # coucou !
  1+4
  1+5", " coucou !");
  ("# coucou \r 1+1", " coucou ");
  ("# coucou \r\n 1+1", " coucou ");
  ("# coucou \n\r 1+1", " coucou ");
  ("#\n", "");
  ("     #\n", "");
  ("\t\t# \n", " ");
  ("#! /bin/bash", "! /bin/bash")
]

let count = ref 0

let suite = "Comment testing" >::: List.map (fun (input, str_expected) ->
  incr count;
  let count = string_of_int !count in
  "Comment test [" ^ input ^ "]" >:: fun _ ->
    contains_comment str_expected (Lexing.from_string input)
) words
