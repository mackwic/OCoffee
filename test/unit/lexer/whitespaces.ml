open Common
open OUnit2
open Tokens

type state = {
  sequence : Tokens.token list;
  base_indent : int;
  indent_width : int;
}

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


let lex_input lexbuf =
  let rec inner_state_f lexbuf aggr =
    match tokenize lexbuf with
    | INDENT | DEDENT as value -> inner_state_f lexbuf (value::aggr)
    | EOF -> {
      sequence = List.rev aggr;
      base_indent = !Lexer.base_indent;
      indent_width = match !Lexer.indent_width with None -> 0 | Some(v) -> v
    }
    | _ -> inner_state_f lexbuf aggr
  in
  inner_state_f lexbuf []


let words = [
  ("  ", make_state [DEDENT]);
  ("  ", make_state ~base_indent:0 [INDENT]);
  ("  ", make_state ~indent_width:1 [INDENT]);
]

let suite = "Whitespaces testing" >::: List.map (fun (input, expected) ->
  "Whitespace test [" ^ input ^ "]" >:: fun ctxt ->
    Lexer.reset;
    cmp_state expected (lex_input input)
) words
