open CommonLexerTest
open OUnit2
open Tokens

type state = {
  sequence : Tokens.token list;
  base_indent : int;
  indent_width : int;
}

let l = new LogO.logger __MODULE__

let make_state ?(base_indent = 1) ?(indent_width = 2) sequence =
  {
    sequence = sequence;
    base_indent = base_indent;
    indent_width = indent_width
  }

let cmp_state expected got =
  assert_equal expected.indent_width got.indent_width ~msg:(
    "Indent Width differs: expected " ^ string_of_int expected.indent_width ^
    " but got " ^ string_of_int got.indent_width
  );
  assert_equal expected.base_indent got.base_indent ~msg:(
    "Base indent differs: expected " ^ string_of_int expected.base_indent ^
    " but got " ^ string_of_int got.base_indent);
  ListTokens.assert_equal expected.sequence got.sequence

let log pos tok = l#silly pos ("got [" ^ (PrintTokens.string_of_token tok) ^ "]")

let lex_input lexbuf =
  let rec inner_state_f aggr =
    match Lexer.tokenize lexbuf with
    | INDENT | DEDENT as value ->
        l#silly __POS__ "got [indent/dedent]";
        inner_state_f (value::aggr)
    | EOF ->
      l#silly __POS__ ("got eof");
      {
        sequence = List.rev aggr;
        base_indent = !LexerIndent.base_indent;
        indent_width = match !LexerIndent.indent_width with None -> 0 | Some(v) -> v
      }
    | _ as v ->
      log __POS__ v;
      inner_state_f aggr
  in
  inner_state_f []


let words = [
  ("base test 1", "  ", make_state [INDENT]);
  ("base test (purity) 1", "  ", make_state [INDENT]);
  ("base test (purity) 2", "  ", make_state [INDENT]);
  ("check other state variables 1", "  ", make_state ~base_indent:1 [INDENT]);
  ("check other state variables 2", "",
    make_state ~base_indent:0 ~indent_width:0 []);
  ("check other state variables 3", "  ", make_state ~indent_width:2 [INDENT]);
  ("check other state variables 4", "   ", make_state ~indent_width:3 [INDENT]);
  ("check other state variables 5", "    ", make_state ~indent_width:4 [INDENT]);
  ("only one indent 1", "  \n  \n", make_state [INDENT]);
  ("only one indent 2", "  \n  a\n  a", make_state [INDENT]);
  ("only one indent 3", "  \n  2\n  2", make_state [INDENT]);
  ("only one indent 4", "  1\n  2\n  3\n", make_state [INDENT]);
  ("indent then dedent 1", "  :\n0", make_state [INDENT; DEDENT]);
  ("increasing indentation 1", "  a\n  b\n   a", make_state [INDENT; INDENT]);
]

let suite = "Whitespaces testing" >::: List.map (fun (test_name, input, expected) ->
  "Whitespace test [" ^ test_name ^ "]" >:: fun ctxt ->
    l#notice __POS__ ("============== " ^ test_name ^ " ==============");
    Lexer.reset ();
    cmp_state expected (lex_input (Lexing.from_string input))
) words
