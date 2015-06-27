
(* 0. Context tokens *)
%token INDENT
%token DEDENT

(* 1. values and names *)
%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <string> ID
%token <string> AT_ID (* @prop *)
%token <string> REGEXP

(* 2. primitive values *)
%token TRUE
%token FALSE
%token NULL
%token UNDEFINED

(* 3. operators *)
(* 3.1 booleans *)
%token OPBO_NOT
%token OPBO_AND
%token OPBO_OR
%token OPBO_EQUAL
%token OPBO_NOT_EQUAL
%token OPBO_GREATER
%token OPBO_GREATER_EQUAL
%token OPBO_LESS
%token OPBO_LESS_EQUAL
(* 3.2 bitwise *)
%token OPBI_AND
%token OPBI_OR
%token OPBI_SHIFTL
%token OPBI_SHIFTR
(* 3.3 arithmetic *)
%token OPAR_PLUS
%token OPAR_MINUS  (* don't forget the unary minus !*)
%token OPAR_MULTIPLY
%token OPAR_DIVIDE
%token OPAR_DIVIDE_INTERGER (* 3 // 2 is 1 *)
%token OPAR_MODULO (* standard modulo *)
%token OPAR_EXPONENT (* 2 ** 3 is 2 ^ 3*)
%token OPAR_MODULO_POSITIVE (* -7 %% 5 is 3 *)
(* 3.4 function *)
%token OPF_THIN_ROCKET (* -> *)
%token OPF_FAT_ROCKET  (* => *)
(* 3.5 assignment *)
%token OPAS_ASSING     (* =  *)
%token OPAS_ASSING_OR  (* |= *)
%token OPAS_ASSING_AND (* &= *)
%token OPAS_ASSING_IF  (* ?= *)
%token OPAS_ASSING_PLUS (* += *)
%token OPAS_ASSING_MINUS (* -= *)
%token OPAS_ASSING_MULTIPLY
%token OPAS_ASSING_DIVIDE
%token OPAS_ASSING_DIVIDE_INTEGER
%token OPAS_ASSING_MODULO
%token OPAS_ASSING_MODULO_POSITIVE
%token OPAS_ASSING_EXPONENT
(* 3.6 exist  *)
%token OP_EXISTS (* ? *)
%token <string> OP_EXIST_FIELD (* a.b? *)
%token <string> OP_EXISTS_FUNC (* a.b?() *)

(* 4. conditionals *)
%token IF
%token UNLESS
%token THEN
%token ELSE
%token SWITCH
%token WHEN
(* 5. loop *)
%token WHILE
%token UNTIL
%token FOR
%token FOR_IN
%token FOR_OF
%token FOR_DO
%token BREAK
%token CONTINUE
%token RETURN

(* 5. quotes *)
%token START_STRING
%token END_STRING
%token START_INTERPOLATE
%token END_INTERPOLATE
%token START_EMBED_JS
%token END_EMBED_JS
%token START_BLOCK_STRING
%token END_BLOCK_STRING
%token START_BLOCK_COMMENT
%token END_BLOCK_COMMENT
%token START_REGEX
%token END_REGEXP
%token START_BLOCK_REGEXP
%token END_BLOCK_REGEXP

(* 6. classes *)
%token CLASS
%token EXTENDS
%token SUPER
%token DOUBLE_COMMA (* String::method *)

(* 7. exceptions *)
%token TRY
%token CATCH
%token FINALLY
%token THROW

(* 8. other tokens *)
%token THIS
%token NEW
%token DOT
%token DOUBLE_DOT (* .. as in [2..5] *)
%token TRIPLE_DOT (* ... as in [2...5] AND in args... *)
%token YIELD
%token L_BRACE
%token R_BRACE
%token L_BRACK
%token R_BRACK
%token SEMICOLON
%token COLON
%token COMMA
%token EOF

