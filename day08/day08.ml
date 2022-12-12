open Base
open Stdio

type map = int list list [@@deriving equal, sexp]

let parse_char = function
  | '0' .. '9' as c -> Char.to_int c - Char.to_int '0'
  | _ -> assert false

let parse_line s = String.to_list s |> List.map ~f:parse_char
let parse lines = List.map lines ~f:parse_line
let sample = [ "30373"; "25512"; "65332"; "33549"; "35390" ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: map] |> print_s;
  [%expect {| ((3 0 3 7 3) (2 5 5 1 2) (6 5 3 3 2) (3 3 5 4 9) (3 5 3 9 0)) |}]

let visible_row l =
  List.fold_map l ~init:None ~f:(fun acc item ->
      let visible =
        match acc with None -> true | Some cur_max -> item >= cur_max
      in
      if visible then (Some item, true) else (acc, false))
  |> snd

let visible = List.map ~f:visible_row

type vis_map = bool list list

let sexp_of_vis_map l =
  List.map l ~f:(List.map ~f:(function true -> 'x' | false -> '.'))
  |> [%sexp_of: char list list]

let%expect_test "visible" =
  let test l = parse l |> visible |> [%sexp_of: vis_map] |> print_s in
  test sample;
  [%expect {| ((x . x x .) (x x x . .) (x . . . .) (x x x . x) (x x . x .)) |}]

let rotate l = List.rev l |> List.transpose_exn
let rotate' l = List.transpose_exn l |> List.rev

let%expect_test "rotate" =
  let r1 = parse sample |> rotate in
  r1 |> [%sexp_of: map] |> print_s;
  [%expect {| ((3 3 6 2 3) (5 3 5 5 0) (3 5 3 5 3) (9 4 3 1 7) (0 9 2 2 3)) |}];
  let r2 = rotate r1 in
  r2 |> [%sexp_of: map] |> print_s;
  [%expect {| ((0 9 3 5 3) (9 4 5 3 3) (2 3 3 5 6) (2 1 5 5 2) (3 7 3 0 3)) |}];
  let r3 = rotate r2 in
  r3 |> [%sexp_of: map] |> print_s;
  [%expect {| ((3 2 2 9 0) (7 1 3 4 9) (3 5 3 5 3) (0 5 5 3 5) (3 2 6 3 3)) |}];
  [%equal: map] r1 (rotate' (rotate r1)) |> [%sexp_of: bool] |> print_s;
  [%expect {| true |}]

let%expect_test "rotate + vis" =
  let m = parse sample in
  m |> rotate |> visible |> rotate' |> [%sexp_of: vis_map] |> print_s;
  [%expect {| ((. . . . .) (. x x . .) (x x . . .) (x . x . x) (x x x x x)) |}]

let apply_on_all_dirs f op m =
  let ( ||| ) = List.map2_exn ~f:(List.map2_exn ~f:op) in
  let v0 = f m in
  let r1 = rotate m in
  let v1 = r1 |> f |> rotate' in
  let r2 = rotate r1 in
  let v2 = r2 |> f |> rotate' |> rotate' in
  let r3 = rotate r2 in
  let v3 = r3 |> f |> rotate' |> rotate' |> rotate' in
  v0 ||| v1 ||| v2 ||| v3

let visible_any m = apply_on_all_dirs visible ( || ) m

let%expect_test "visible_any" =
  let m = parse sample in
  visible_any m |> [%sexp_of: vis_map] |> print_s;
  [%expect {| ((x x x x x) (x x x . x) (x x x x x) (x x x . x) (x x x x x)) |}]

let count m =
  List.fold m ~init:0 ~f:(fun acc ->
      List.fold ~init:acc ~f:(fun acc b -> if b then acc + 1 else acc))

let run lines =
  let m = parse lines in
  visible_any m |> count |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 23 |}]

let rec tails = function [] -> [] | h :: t -> (h, t) :: tails t

let%expect_test "tails" =
  tails [ 1; 2; 3; 4; 5 ] |> [%sexp_of: (int * int list) list] |> print_s;
  [%expect {| ((1 (2 3 4 5)) (2 (3 4 5)) (3 (4 5)) (4 (5)) (5 ())) |}]

let dist_row m =
  tails m
  |> List.map ~f:(fun (hd, tl) ->
         if List.is_empty tl then 0
         else
           let r, r' = List.split_while tl ~f:(fun x -> x < hd) in
           let lr = List.length r in
           if List.is_empty r' then lr else lr + 1)

let dist = List.map ~f:dist_row

let%expect_test "dist_row" =
  let test s = parse_line s |> dist_row |> [%sexp_of: int list] |> print_s in
  test "30373";
  [%expect {| (2 1 1 1 0) |}];
  test "25512";
  [%expect {| (1 1 2 1 0) |}];
  test "21552";
  [%expect {| (2 1 1 1 0) |}];
  test "35353";
  [%expect {| (1 2 1 1 0) |}];
  test "33549";
  [%expect {| (1 1 2 1 0) |}];
  test "71349";
  [%expect {| (4 1 1 1 0) |}];
  test "94317";
  [%expect {| (4 3 2 1 0) |}];
  test "94533";
  [%expect {| (4 1 2 1 0) |}]

let scenic_scores = apply_on_all_dirs (fun m -> dist m) ( * )

let%expect_test "scenic_scores" =
  let m = parse sample in
  scenic_scores m |> [%sexp_of: int list list] |> print_s;
  [%expect {| ((0 0 0 0 0) (0 1 4 1 0) (0 6 1 2 0) (0 1 8 3 0) (0 0 0 0 0)) |}]

let run2 lines =
  let m = parse lines in
  scenic_scores m |> List.concat
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn |> Int.to_string |> print_endline

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
