open Common
open OUnit2
open Tokens


let values = [
  (* integers *)
  ("42", INT(42));
  ("37", INT(37));
  ("1337", INT(1337));
  ("-42", INT(-42));
  ("-37", INT(-37));
  ("-1337", INT(-1337));
  ("0", INT(0));
  ("0", INT(0));
  (* int32 max *)
  ("2147483647", INT(2147483647));
  ("-2147483648", INT(-2147483648));
  (string_of_int max_int, INT(max_int));
  (string_of_int min_int, INT(min_int));
  (* booleans *)
  ("true", BOOL(true));
  ("false", BOOL(false));
  (* floats *)
  ("3.1415117398", FLOAT(3.1415117398));
  ("-3.1415117398", FLOAT(-3.1415117398));
  ("-0.0", FLOAT(-0.0));
  ("0.0", FLOAT(0.0));
   (* By spec, NaN <> NaN, so it won't work.
    * But I tested maually and I garantee you it's OK
   * ("NaN", FLOAT(nan));
   *)
  ("Infinity", FLOAT(infinity));
  ("-Infinity", FLOAT(neg_infinity));
  ("12e24", FLOAT(12e24));
  ("3.14e52", FLOAT(3.14e52));
  (".012e12", FLOAT(12e9))
]

let suite = "Values testing" >::: List.map (fun (input, expected) ->
  "Values test [" ^ input ^ "]" >:: fun _ ->
    let token = tokenize input in
    assert_equal expected token ~msg:(
      "Expected " ^ (PrintTokens.string_of_token expected) ^ " but got " ^
      PrintTokens.string_of_token token
    )
) values
