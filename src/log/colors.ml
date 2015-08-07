
type color = int * int

let colors = [
  ("reset", (0, 0));

  ("bold", (1, 22));
  ("dim", (2, 22));
  ("italic", (3, 23));
  ("underline", (4, 24));
  ("inverse", (7, 27));
  ("hidden", (8, 28));
  ("strikethrough", (9, 29));

  ("black", (30, 39));
  ("red", (31, 39));
  ("green", (32, 39));
  ("yellow", (33, 39));
  ("blue", (34, 39));
  ("magenta", (35, 39));
  ("cyan", (36, 39));
  ("white", (37, 39));
  ("gray", (90, 39));
  ("grey", (90, 39));

  ("bgBlack", (40, 49));
  ("bgRed", (41, 49));
  ("bgGreen", (42, 49));
  ("bgYellow", (43, 49));
  ("bgBlue", (44, 49));
  ("bgMagenta", (45, 49));
  ("bgCyan", (46, 49));
  ("bgWhite", (47, 49));

  ("blackBG", (40, 49));
  ("redBG", (41, 49));
  ("greenBG", (42, 49));
  ("yellowBG", (43, 49));
  ("blueBG", (44, 49));
  ("magentaBG", (45, 49));
  ("cyanBG", (46, 49));
  ("whiteBG", (47, 4))
]

let color_code color_name = List.assoc color_name colors

let code_to_str code = "\x1b[" ^ string_of_int code ^ "m"
let col_to_str (code1,code2) = (code_to_str code1, code_to_str code2)

let colorize color_name str =
  let s_begin, s_end = col_to_str (color_code color_name) in
  s_begin ^ str ^ s_end

let bold = colorize "bold"
let black = colorize "black"
let red = colorize "red"
let green = colorize "green"
let yellow = colorize "yellow"
let blue = colorize "blue"
let magenta = colorize "magenta"
let cyan = colorize "cyan"
let white = colorize "white"
let gray = colorize "gray"
let grey = colorize "grey"


