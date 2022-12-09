open Base
open Stdio

type pos = char list Map.M(Int).t [@@deriving sexp_of]

let width c =
  let c' = c + 1 in
  assert (c' % 4 = 0);
  c' / 4

let%expect_test "width" =
  let test n = width n |> Int.to_string |> print_endline in
  test 11;
  [%expect {| 3 |}];
  test 7;
  [%expect {| 2 |}];
  test 3;
  [%expect {| 1 |}]

let parse_line s =
  let n = String.length s |> width in
  List.range 0 n
  |> List.map ~f:(fun i ->
         match s.[(4 * i) + 1] with
         | ' ' -> None
         | 'A' .. 'Z' as c -> Some c
         | c -> raise_s [%message (c : char)])

let%expect_test "parse_line" =
  let test s = parse_line s |> [%sexp_of: char option list] |> print_s in
  test "    [D]    ";
  [%expect {| (() (D) ()) |}];
  test "[N] [C]    ";
  [%expect {| ((N) (C) ()) |}];
  test "[Z] [M] [P]";
  [%expect {| ((Z) (M) (P)) |}]

let assemble lines =
  List.transpose_exn lines
  |> List.mapi ~f:(fun i l ->
         let v =
           List.drop_while l ~f:Option.is_none
           |> List.map ~f:(fun o -> Option.value_exn o)
         in
         (i + 1, v))
  |> Map.of_alist_exn (module Int)

let parse_pos l = l |> List.drop_last_exn |> List.map ~f:parse_line |> assemble

let%expect_test "parse_pos" =
  let lines = [ "    [D]    "; "[N] [C]    "; "[Z] [M] [P]"; " 1   2   3 " ] in
  parse_pos lines |> [%sexp_of: pos] |> print_s;
  [%expect {| ((1 (N Z)) (2 (D C M)) (3 (P))) |}]

type move = { src : int; dest : int; count : int } [@@deriving sexp_of]

let parse_move_re =
  let open Re in
  let num = rep1 digit in
  compile
    (seq
       [
         bos;
         str "move ";
         group num;
         str " from ";
         group num;
         str " to ";
         group num;
         eos;
       ])

let parse_move ~single s =
  let g = Re.exec parse_move_re s in
  let count = Re.Group.get g 1 |> Int.of_string in
  let src = Re.Group.get g 2 |> Int.of_string in
  let dest = Re.Group.get g 3 |> Int.of_string in
  if single then List.init count ~f:(fun _ -> { src; dest; count = 1 })
  else [ { src; dest; count } ]

let%expect_test "parse_move" =
  parse_move ~single:true "move 2 from 3 to 4"
  |> [%sexp_of: move list] |> print_s;
  [%expect {| (((src 3) (dest 4) (count 1)) ((src 3) (dest 4) (count 1))) |}]

let parse ~single l =
  let pos_lines, r = List.split_while l ~f:(fun s -> not (String.is_empty s)) in
  let pos = parse_pos pos_lines in
  let move_lines = List.tl_exn r in
  let moves = List.concat_map ~f:(parse_move ~single) move_lines in
  (pos, moves)

let sample =
  [
    "    [D]    ";
    "[N] [C]    ";
    "[Z] [M] [P]";
    " 1   2   3 ";
    "";
    "move 1 from 2 to 1";
    "move 3 from 1 to 3";
    "move 2 from 2 to 1";
    "move 1 from 1 to 2";
  ]

let%expect_test "parse" =
  parse sample ~single:true |> [%sexp_of: pos * move list] |> print_s;
  [%expect
    {|
    (((1 (N Z)) (2 (D C M)) (3 (P)))
     (((src 2) (dest 1) (count 1)) ((src 1) (dest 3) (count 1))
      ((src 1) (dest 3) (count 1)) ((src 1) (dest 3) (count 1))
      ((src 2) (dest 1) (count 1)) ((src 2) (dest 1) (count 1))
      ((src 1) (dest 2) (count 1)))) |}]

let get pos key ~count =
  let v = Map.find_exn pos key in
  let h, t = List.split_n v count in
  (h, Map.set pos ~key ~data:t)

let put pos key data =
  Map.update pos key ~f:(fun lo ->
      let l = Option.value ~default:[] lo in
      data @ l)

let apply_move pos { src; dest; count } =
  let v, pos' = get pos ~count src in
  put pos' dest v

let go (pos, moves) =
  List.fold_left moves ~init:pos ~f:apply_move
  |> Map.to_alist
  |> List.map ~f:(fun (_, l) -> List.hd_exn l)
  |> String.of_char_list |> print_endline

let run lines = parse lines ~single:true |> go

let%expect_test "run" =
  run sample;
  [%expect {| CMZ |}]

let run2 lines = parse lines ~single:false |> go

let%expect_test "run2" =
  run2 sample;
  [%expect {| MCD |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
