open Base
open Stdio

module Interval = struct
  type t = { low : int; high : int } [@@deriving sexp_of]

  let contains { low; high } ~sub:{ low = low_sub; high = high_sub } =
    low <= low_sub && high_sub <= high

  let%expect_test "contains" =
    let test a sub = contains a ~sub |> sexp_of_bool |> print_s in
    test { low = 1; high = 2 } { low = 1; high = 1 };
    [%expect {| true |}];
    test { low = 1; high = 2 } { low = 1; high = 3 };
    [%expect {| false |}]

  let overlap x y =
    let a, b = if x.low < y.low then (x, y) else (y, x) in
    a.high >= b.low

  let%expect_test "overlap" =
    let test a b = overlap a b |> sexp_of_bool |> print_s in
    test { low = 1; high = 2 } { low = 1; high = 1 };
    [%expect {| true |}];
    test { low = 1; high = 5 } { low = 4; high = 6 };
    [%expect {| true |}];
    test { low = 4; high = 6 } { low = 1; high = 5 };
    [%expect {| true |}];
    test { low = 4; high = 6 } { low = 1; high = 3 };
    [%expect {| false |}];
    test { low = 1; high = 3 } { low = 4; high = 6 };
    [%expect {| false |}]
end

let parse_line_re =
  let open Re in
  let num = group (rep1 digit) in
  let interval = seq [ num; char '-'; num ] in
  compile (seq [ bos; interval; char ','; interval; eos ])

let parse_line s =
  let open Interval in
  let g = Re.exec parse_line_re s in
  let int n = Re.Group.get g n |> Int.of_string in
  let a = { low = int 1; high = int 2 } in
  let b = { low = int 3; high = int 4 } in
  (a, b)

let%expect_test "parse_line" =
  parse_line "12-34,56-78" |> [%sexp_of: Interval.t * Interval.t] |> print_s;
  [%expect {| (((low 12) (high 34)) ((low 56) (high 78))) |}]

let run lines =
  lines |> List.map ~f:parse_line
  |> List.filter ~f:(fun (a, b) ->
         Interval.contains a ~sub:b || Interval.contains b ~sub:a)
  |> List.length |> Int.to_string |> print_endline

let%expect_test "run" =
  run [ "2-4,6-8"; "2-3,4-5"; "5-7,7-9"; "2-8,3-7"; "6-6,4-6"; "2-6,4-8" ];
  [%expect {| 2 |}]

let run2 lines =
  lines |> List.map ~f:parse_line
  |> List.filter ~f:(fun (a, b) -> Interval.overlap a b)
  |> List.length |> Int.to_string |> print_endline

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
