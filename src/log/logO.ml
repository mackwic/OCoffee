open Common_log

class logger ?(level = Silly) ?(channel_out = stderr) modul = 

  object(self)
    method modul = modul
    method level = level
    method channel_out = channel_out

    method log level (pos: string * int * int * int) msg =
      if level < self#level then ()
      else
        let (file, line, _, _) = pos in
        let lvl, color = List.assoc level level_meta in
          Printf.fprintf self#channel_out "[%s|%s:%s][%9s] %s\n"
            (Colors.yellow self#modul)
            (Colors.magenta file)
            (Colors.cyan (string_of_int line))
            (Colors.colorize color lvl |> Colors.bold)
            (Colors.colorize color msg);

    method silly = self#log Silly
    method debug = self#log Debug
    method notice = self#log Notice
    method info = self#log Info
    method warn = self#log Warning
    method error = self#log Error
  end
