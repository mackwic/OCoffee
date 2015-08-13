open Common_log

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

