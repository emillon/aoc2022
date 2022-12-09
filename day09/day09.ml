open Base
open Stdio

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include Comparable.Make (T)
  include T
end

type pos = Pos.t [@@deriving sexp_of]
type move = int * int [@@deriving sexp]
type state_x = { hx_pos : pos; tx_pos : pos }

let move_pos (x, y) (dx, dy) = (x + dx, y + dy)

let move_of_string = function
  | "U" -> (0, 1)
  | "D" -> (0, -1)
  | "L" -> (-1, 0)
  | "R" -> (1, 0)
  | _ -> assert false

let parse_line_re =
  let open Re in
  compile (seq [ bos; group alpha; char ' '; group (rep1 digit); eos ])

let parse_line s =
  let g = Re.exec parse_line_re s in
  let move = Re.Group.get g 1 |> move_of_string in
  let count = Re.Group.get g 2 |> Int.of_string in
  (move, count)

let%expect_test "parse_line" =
  let test s = parse_line s |> [%sexp_of: move * int] |> print_s in
  test "R 4";
  [%expect {| ((1 0) 4) |}]

let diff (ax, ay) (bx, by) = (bx - ax, by - ay)

let move_to src dst =
  let dir =
    match diff src dst with
    | 0, 0 | -1, 0 | 0, 1 | 0, -1 | 1, 0 | 1, -1 | 1, 1 | -1, -1 | -1, 1 ->
        (0, 0)
    | 0, 2 -> (0, 1)
    | 0, -2 -> (0, -1)
    | -2, 0 -> (-1, 0)
    | 2, 0 -> (1, 0)
    | -1, 2 | -2, 1 | -2, 2 -> (-1, 1)
    | -2, -1 | -1, -2 | -2, -2 -> (-1, -1)
    | 1, -2 | 2, -1 | 2, -2 -> (1, -1)
    | 2, 1 | 1, 2 | 2, 2 -> (1, 1)
    | dx, dy -> raise_s [%message (dx : int) (dy : int)]
  in
  move_pos src dir

let apply_movex { hx_pos; tx_pos } move =
  let hx_pos' = move_pos hx_pos move in
  let tx_pos' = move_to tx_pos hx_pos' in
  { hx_pos = hx_pos'; tx_pos = tx_pos' }

let run lines_s =
  let lines =
    List.concat_map lines_s ~f:(fun s ->
        let move, count = parse_line s in
        List.init count ~f:(fun _ -> move))
  in
  let origin = (0, 0) in
  let init = ({ hx_pos = origin; tx_pos = origin }, Set.empty (module Pos)) in
  List.fold lines ~init ~f:(fun (state, pos_set) move ->
      let next_state = apply_movex state move in
      (next_state, Set.add pos_set next_state.tx_pos))
  |> snd |> Set.length |> Int.to_string |> print_endline

let view sgs =
  let board = Array.make_matrix ~dimx:60 ~dimy:60 ' ' in
  printf "---\n";
  List.iteri sgs ~f:(fun i (x, y) ->
      board.(30 - y).(30 + x) <- Char.of_int_exn (Char.to_int '0' + i));
  Array.iter board ~f:(fun line ->
      Array.iter line ~f:(fun c -> printf "%c" c);
      printf "\n")

let move_points l m =
  match l with
  | [] -> assert false
  | first :: rest ->
      let first' = move_pos first m in
      let follow = ref first' in
      let rest' =
        List.map rest ~f:(fun p ->
            let p' = move_to p !follow in
            follow := p';
            p')
      in
      first' :: rest'

let tail_pos2 sgs = List.last_exn sgs

let run2 lines_s =
  let lines =
    List.concat_map lines_s ~f:(fun s ->
        let move, count = parse_line s in
        List.init count ~f:(fun _ -> move))
  in
  let origin = (0, 0) in
  let init = (List.init 10 ~f:(fun _ -> origin), Set.empty (module Pos)) in
  List.fold lines ~init ~f:(fun (points, pos_set) move ->
      let points' = move_points points move in
      (points', Set.add pos_set (tail_pos2 points')))
  |> snd |> Set.length |> Int.to_string |> print_endline

let%expect_test "run2" =
  run2 [ "R 5"; "U 8"; "L 8"; "D 3"; "R 17"; "D 10"; "L 25"; "U 20" ];
  [%expect {| 36 |}];
  run2 [ "R 4"; "U 4"; "L 3"; "D 1"; "R 4"; "D 1"; "L 5"; "R 2" ];
  [%expect {| 1 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
