open Base
open Stdio

let groups ~chunk_len s =
  let n = String.length s - chunk_len in
  let starts = List.range 0 n ~stop:`inclusive in
  List.map starts ~f:(fun start -> String.sub ~pos:start ~len:chunk_len s)

let%expect_test "groups" =
  let test s = groups ~chunk_len:4 s |> [%sexp_of: string list] |> print_s in
  test "abcdefghi";
  [%expect {| (abcd bcde cdef defg efgh fghi) |}]

let group_ok s =
  let n = String.length s in
  String.to_list s |> Set.of_list (module Char) |> Set.length = n

let%expect_test "group_ok" =
  let test s = group_ok s |> [%sexp_of: bool] |> print_s in
  test "abcd";
  [%expect {| true |}];
  test "mjqj";
  [%expect {| false |}];
  test "jpqm";
  [%expect {| true |}]

let go ~chunk_len s =
  s |> groups ~chunk_len
  |> List.find_mapi_exn ~f:(fun i g ->
         if group_ok g then Some (i + chunk_len) else None)
  |> Int.to_string |> print_endline

let run s = go s ~chunk_len:4

let%expect_test "run" =
  run "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
  [%expect {| 7 |}]

let run2 s = go s ~chunk_len:14

let%expect_test "run2" =
  run2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
  [%expect{| 19 |}];
  run2 "bvwbjplbgvbhsrlpgdmjqwftvncz";
  [%expect{| 23 |}];
  run2 "nppdvjthqldpwncqszvftbrmjlhg";
  [%expect{| 23 |}];
  run2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
  [%expect{| 29 |}];
  run2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
  [%expect{| 26 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_all |> run
  | [| _; "--2"; p |] -> In_channel.with_file p ~f:In_channel.input_all |> run2
  | _ -> assert false
