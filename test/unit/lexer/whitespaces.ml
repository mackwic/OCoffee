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
  let rec inner_state_f lexbuf aggr =
    match tokenize lexbuf with
    | INDENT | DEDENT as value ->
        l#silly __POS__ "got [indent/dedent]";
        inner_state_f lexbuf (value::aggr)
    | EOF ->
      l#silly __POS__ ("got eof");
      {
        sequence = List.rev aggr;
        base_indent = !Lexer.base_indent;
        indent_width = match !Lexer.indent_width with None -> 0 | Some(v) -> v
      }
    | _ as v ->
      log __POS__ v;
      inner_state_f lexbuf aggr
  in
  inner_state_f lexbuf []


let words = [
  ("  ", make_state [INDENT]);
  ("  ", make_state [INDENT]);
  ("  ", make_state [INDENT]);
  ("  ", make_state [INDENT]);
  ("  ", make_state [INDENT]);
  ("  ", make_state ~base_indent:1 [INDENT]);
  (*("  ", make_state ~indent_width:2 [INDENT]);*)
  ("  \na  \nb    \na  ", make_state [INDENT; INDENT; DEDENT]);
]

let i = ref 0

let suite = "Whitespaces testing" >::: List.map (fun (input, expected) ->
  "Whitespace test [" ^ (incr i; string_of_int !i) ^ "]" >:: fun ctxt ->
    Lexer.reset ();
    cmp_state expected (lex_input input)
) words
