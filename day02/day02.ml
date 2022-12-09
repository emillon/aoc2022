open Base
open Stdio

type rps = Rock | Paper | Scissors [@@deriving sexp_of]

let rps_of_char = function
  | 'A' -> Rock
  | 'B' -> Paper
  | 'C' -> Scissors
  | _ -> assert false

type encoded = X | Y | Z [@@deriving equal, sexp_of]

let encoded_of_char = function
  | 'X' -> X
  | 'Y' -> Y
  | 'Z' -> Z
  | _ -> assert false

type 'a line = { opponent : rps; player : 'a } [@@deriving sexp_of]

let parse_line s =
  assert (String.length s = 3);
  assert (Char.equal s.[1] ' ');
  { opponent = rps_of_char s.[0]; player = encoded_of_char s.[2] }

let%expect_test "parse_line" =
  let test s = parse_line s |> [%sexp_of: encoded line] |> print_s in
  test "A X";
  [%expect {| ((opponent Rock) (player X)) |}];
  test "B Y";
  [%expect {| ((opponent Paper) (player Y)) |}];
  test "C Z";
  [%expect {| ((opponent Scissors) (player Z)) |}]

let score_player = function Rock -> 1 | Paper -> 2 | Scissors -> 3

type outcome = Win | Lose | Draw

let score_outcome = function Lose -> 0 | Draw -> 3 | Win -> 6

let outcome = function
  | { player = Rock; opponent = Scissors } -> Win
  | { player = Paper; opponent = Rock } -> Win
  | { player = Scissors; opponent = Paper } -> Win
  | { player = Rock; opponent = Rock } -> Draw
  | { player = Scissors; opponent = Scissors } -> Draw
  | { player = Paper; opponent = Paper } -> Draw
  | { opponent = Rock; player = Scissors } -> Lose
  | { opponent = Paper; player = Rock } -> Lose
  | { opponent = Scissors; player = Paper } -> Lose

let score l = score_player l.player + score_outcome (outcome l)

let%expect_test "score" =
  let test l = score l |> Int.to_string |> print_endline in
  test { opponent = Rock; player = Paper };
  [%expect {| 8 |}];
  test { opponent = Paper; player = Rock };
  [%expect {| 1 |}];
  test { opponent = Scissors; player = Scissors };
  [%expect {| 6 |}]

type sub = { x : rps; y : rps; z : rps } [@@deriving sexp_of]

let winner_against = function
  | Rock -> Paper
  | Paper -> Scissors
  | Scissors -> Rock

let loser_against = function
  | Paper -> Rock
  | Scissors -> Paper
  | Rock -> Scissors

let last_encoded a b =
  match (a, b) with
  | X, Y -> Some Z
  | Y, X -> Some Z
  | X, Z -> Some Y
  | Z, X -> Some Y
  | Y, Z -> Some X
  | Z, Y -> Some X
  | X, X -> None
  | Y, Y -> None
  | Z, Z -> None

let last a b =
  match (a, b) with
  | Rock, Paper -> Some Scissors
  | Paper, Rock -> Some Scissors
  | Rock, Scissors -> Some Paper
  | Scissors, Rock -> Some Paper
  | Paper, Scissors -> Some Rock
  | Scissors, Paper -> Some Rock
  | Rock, Rock -> None
  | Paper, Paper -> None
  | Scissors, Scissors -> None

let complete_sub (e1, v1) (e2, v2) =
  let open Option.Let_syntax in
  let%bind e3 = last_encoded e1 e2 in
  let%bind v3 = last v1 v2 in
  let l = [ (e1, v1); (e2, v2); (e3, v3) ] in
  let find v = List.Assoc.find_exn l v ~equal:[%equal: encoded] in
  return { x = find X; y = find Y; z = find Z }

let find_sub lines =
  let l1 = List.hd_exn lines in
  let binding_1 = (l1.player, winner_against l1.opponent) in
  let ls = List.tl_exn lines in
  List.find_map_exn ls ~f:(fun l2 ->
      let binding_2 = (l2.player, loser_against l2.opponent) in
      complete_sub binding_1 binding_2)

let%expect_test "find_sub" =
  find_sub [ { opponent = Rock; player = Y }; { opponent = Paper; player = X } ]
  |> [%sexp_of: sub] |> print_s;
  [%expect {| ((x Rock) (y Paper) (z Scissors)) |}]

let apply_sub sub line =
  let player = match line.player with X -> sub.x | Y -> sub.y | Z -> sub.z in
  { opponent = line.opponent; player }

let run lines_s =
  let lines = List.map ~f:parse_line lines_s in
  let sub = find_sub lines in
  List.fold lines ~init:0 ~f:(fun acc line ->
      let sline = apply_sub sub line in
      acc + score sline)
  |> Int.to_string |> print_endline

let interpret2 l =
  let player =
    match l.player with
    | X -> loser_against l.opponent
    | Y -> l.opponent
    | Z -> winner_against l.opponent
  in
  { opponent = l.opponent; player }

let run2 lines_s =
  let lines = List.map ~f:parse_line lines_s in
  List.fold lines ~init:0 ~f:(fun acc line ->
      let line = interpret2 line in
      acc + score line)
  |> Int.to_string |> print_endline

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
