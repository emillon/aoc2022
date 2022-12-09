open Base
open Stdio

let split s =
  let n = String.length s in
  assert (n % 2 = 0);
  let half = n / 2 in
  (String.subo ~len:half s, String.subo ~pos:half s)

let%expect_test "split" =
  split "abcDEF" |> [%sexp_of: string * string] |> print_s;
  [%expect {| (abc DEF) |}]

let to_set s = String.to_list s |> Set.of_list (module Char)
let common s1 s2 = Set.inter (to_set s1) (to_set s2) |> Set.choose_exn

let%expect_test "common" =
  common "abc" "dce" |> [%sexp_of: char] |> print_s;
  [%expect {| c |}]

let value = function
  | 'a' .. 'z' as c -> Char.to_int c - Char.to_int 'a' + 1
  | 'A' .. 'Z' as c -> Char.to_int c - Char.to_int 'A' + 27
  | _ -> assert false

let%expect_test "value" =
  let test c = value c |> [%sexp_of: int] |> print_s in
  test 'a';
  [%expect {| 1 |}];
  test 'z';
  [%expect {| 26 |}];
  test 'A';
  [%expect {| 27 |}];
  test 'Z';
  [%expect {| 52 |}]

let run lines =
  List.fold lines ~init:0 ~f:(fun sum s ->
      let s1, s2 = split s in
      let c = common s1 s2 in
      sum + value c)
  |> Int.to_string |> print_endline

let rec group3 = function
  | a :: b :: c :: r -> (a, b, c) :: group3 r
  | [] -> []
  | _ -> assert false

let common3 a b c =
  Set.inter (Set.inter (to_set a) (to_set b)) (to_set c) |> Set.choose_exn

let run2 lines =
  let groups = group3 lines in
  List.fold ~init:0 groups ~f:(fun sum (a, b, c) ->
      let c = common3 a b c in
      sum + value c)
  |> Int.to_string |> print_endline

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
