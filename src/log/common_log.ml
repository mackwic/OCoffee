
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
  (Notice,  ("notice ", "yellow"));
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
