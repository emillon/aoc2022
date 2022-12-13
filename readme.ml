open Base
open Stdio

let file_is_not_empty path =
  match In_channel.with_file path ~f:In_channel.input_all with
  | "" -> false
  | _ -> true
  | exception Sys_error _ -> false

let emoji_for_day day =
  let path i = Printf.sprintf "day%02d/p%d.expected" day i in
  if file_is_not_empty (path 1) && file_is_not_empty (path 2) then ":star2:"
  else ":snowflake:"

let () =
  List.range 1 25 ~stop:`inclusive
  |> List.map ~f:emoji_for_day |> String.concat ~sep:" " |> print_endline
