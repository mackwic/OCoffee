type level =
  | Silly
  | Debug
  | Notice
  | Info
  | Warning
  | Error

and loggerName = string

and logger_t = {
  level : level;
  modul : string;
  out : out_channel;
}

let level_meta = [
  (Silly,   (" silly ", "magenta"));
  (Debug,   (" debug ", "cyan"));
  (Notice,  (" notice", "white"));
  (Info,    (" info  ", "green"));
  (Warning, ("warning", "yellow"));
  (Error,   (" error ", "red"))
]
let colorize level str =
  let color_name = (List.assoc level level_meta) |> snd in
  Colors.colorize color_name str
let str_of_level level = (List.assoc level level_meta) |> fst
let colorize_with_level level str =
  let (name, color) = List.assoc level level_meta in
  Colors.colorize color (name ^ ": " ^ str)

let make modul level channel_out =
  { level = level; modul = modul; out = channel_out }

let out_str level logger pos msg =
  if level < logger.level then ()
  else
    let file, line, unused1, unused2 = pos in
    let _ = unused1 + unused2 in (* type hint for the compiler *)
    let lvl, color = List.assoc level level_meta in 
      Printf.fprintf logger.out "[%s|%s:%s][%9s] %s\n"
        (Colors.yellow logger.modul)
        (Colors.magenta file)
        (Colors.cyan (string_of_int line))
        (Colors.colorize color lvl |> Colors.bold)
        (Colors.colorize color msg);
      ()

let silly = out_str Silly
let debug = out_str Debug
let notice = out_str Notice
let info = out_str Info
let warn = out_str Warning
let error = out_str Error

