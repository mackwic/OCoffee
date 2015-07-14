open Common
open OUnit2
open Tokens

let ident_cmp str_expected token =
  match token with
  | ID(value) -> assert_same_str str_expected value
  | _ -> assert_failure "Not lexed as an ID token"

let idents = [
  "toto"; "plop42"; "_totoze"; "__builtin_totoze"; "__attribute__";
  "somestringverylonglonglonglonglonglonglonglonglonglonglong";
  "ocamlCase"; "PascalCase"; "snake_case";
  "int"; "t_uint"; "uint_t"; "string";
  "t_uint32"; "t_uin64"; "_4343434";
  "$"; "$a"; "$a$"; "$$$a";
  "£a"; "€a"; 
  "éternel"; "café"; "Ümlaüt_äre_über_cööl"; "cõmpilãziõne"
]

(* FIXME *)
let failing = [
  (* nothing fail... for now *)
]

let suite = "Idents suite" >::: List.map (fun str ->
    "Ident test: [" ^ str ^ "]" >:: (fun _ -> ident_cmp str (tokenize str))
  ) (idents @ failing)

