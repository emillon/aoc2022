open Base
open Stdio

let emoji_for_day day =
  let path i = Printf.sprintf "day%02d/p%d.expected" day i in
  if Caml.Sys.file_exists (path 1) && Caml.Sys.file_exists (path 2) then
    ":star2:"
  else ":snowflake:"

let () =
  List.range 1 25 ~stop:`inclusive
  |> List.map ~f:emoji_for_day |> String.concat ~sep:" " |> print_endline
