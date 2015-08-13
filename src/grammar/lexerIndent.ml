open Lexing

let log = new LogO.logger __MODULE__

let indent_width = ref None
let base_indent = ref 0

let reset () =
  indent_width := None;
  base_indent := 0;
  log#notice __POS__ "Lexer reset"


let emit_log ?(show_emit = true) pos str =
  if true
  then log#info pos ((if show_emit then "Emit " else "") ^ str)
let emit pos tok =
  emit_log pos (PrintTokens.string_of_token tok);
  tok

type whitespace_result = W_INDENT | W_DEDENT | W_SAMEDENT | W_NOTINDENT

let whitespace lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  (log#debug  __POS__ ("pos.pos_cnum=" ^ string_of_int
  pos.pos_cnum ^ ", pos.pos_bol=" ^ string_of_int pos.pos_bol));
  (* find the column of the start of lexeme. If 0, we got indentation, else 
    * we treat it just as a separator *)
  if (pos.pos_cnum - pos.pos_bol) <> 0
  then
    (emit_log ~show_emit:false __POS__ "cnum - bol <> 0 => not indent, continuing";
    W_NOTINDENT)
  else
    (emit_log ~show_emit:false __POS__ "viable for indent";
    let input_width =
      (Lexing.lexeme_end lexbuf) - (Lexing.lexeme_start lexbuf)
    in
    (log#debug  __POS__ ("input width = " ^ string_of_int input_width));
    let ref_width = match !indent_width with
      (* first indentation will set the indent width *)
      | None -> indent_width := Some(input_width); input_width
      | Some(width) -> width
    in
      (log#debug  __POS__ ("ref_width = " ^ string_of_int ref_width));
      (log#debug  __POS__ ("base_indent = " ^ string_of_int !base_indent));
      (*if input_width mod ref_width <> 0
      then (* FIXME error *) *)
      let diff_indent = (input_width / ref_width) - !base_indent in
      (log#debug  __POS__ ("diff_indent = " ^ string_of_int diff_indent));
      match diff_indent with
      (* no indentation difference, carry on *)
      | 0 -> W_SAMEDENT
      | 1 -> incr base_indent; W_INDENT
      | -1 -> decr base_indent; W_DEDENT (* FIXME assert base_indent > 0 *)
      | _ -> failwith "syntax error: indent error" (* FIXME better error *)
    )
