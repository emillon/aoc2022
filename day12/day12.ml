open Base
open Stdio

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, sexp, hash]
  end

  include T
  include Comparator.Make (T)
end

type map = { heights : int Map.M(Pos).t; start : Pos.t; end_ : Pos.t }
[@@deriving sexp]

let parse_char = function
  | 'a' .. 'z' as c -> Char.to_int c - Char.to_int 'a' + 1
  | 'S' -> 1
  | 'E' -> 26
  | c -> raise_s [%message "parse_char" (c : char)]

let parse_line j s =
  String.to_list s |> List.mapi ~f:(fun i c -> ((i, j), parse_char c))

let find_char c l =
  List.find_mapi_exn l ~f:(fun j s ->
      String.find_mapi s ~f:(fun i ch ->
          if Char.equal ch c then Some (i, j) else None))

let all_moves (x, y) = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

let is_valid_move heights pos ~src_height =
  match Map.find heights pos with
  | None -> false
  | Some pos_height -> pos_height <= src_height + 1

let parse lines =
  let heights =
    List.concat_mapi lines ~f:parse_line |> Map.of_alist_exn (module Pos)
  in
  let start = find_char 'S' lines in
  let end_ = find_char 'E' lines in
  { heights; start; end_ }

let sample = [ "Sabqponm"; "abcryxxl"; "accszExk"; "acctuvwj"; "abdefghi" ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: map] |> print_s;
  [%expect
    {|
    ((heights
      (((0 0) 1) ((0 1) 1) ((0 2) 1) ((0 3) 1) ((0 4) 1) ((1 0) 1) ((1 1) 2)
       ((1 2) 3) ((1 3) 3) ((1 4) 2) ((2 0) 2) ((2 1) 3) ((2 2) 3) ((2 3) 3)
       ((2 4) 4) ((3 0) 17) ((3 1) 18) ((3 2) 19) ((3 3) 20) ((3 4) 5) ((4 0) 16)
       ((4 1) 25) ((4 2) 26) ((4 3) 21) ((4 4) 6) ((5 0) 15) ((5 1) 24)
       ((5 2) 26) ((5 3) 22) ((5 4) 7) ((6 0) 14) ((6 1) 24) ((6 2) 24)
       ((6 3) 23) ((6 4) 8) ((7 0) 13) ((7 1) 12) ((7 2) 11) ((7 3) 10)
       ((7 4) 9)))
     (start (0 0)) (end_ (5 2))) |}]

module G = struct
  type t = map

  module V = Pos

  module E = struct
    type t = Pos.t * Pos.t
    type label = unit

    let label _ = ()
    let src = fst
    let dst = snd
    let create src () dst = (src, dst)
  end

  let iter_vertex _ = raise_s [%message "iter_vertex"]
  let fold_vertex _ = raise_s [%message "fold_vertex"]
  let iter_succ _ = raise_s [%message "iter_succ"]

  let iter_succ_e f map src =
    let src_height = Map.find_exn map.heights src in
    all_moves src
    |> List.filter ~f:(is_valid_move ~src_height map.heights)
    |> List.iter ~f:(fun dst -> f (src, dst))

  let fold_edges_e _ = raise_s [%message "fold_edges_e"]
  let nb_vertex _ = raise_s [%message "nb_vertex"]
end

module D =
  Graph.Path.Dijkstra
    (G)
    (struct
      type t = int [@@deriving compare]
      type edge = G.E.t

      let weight _ = 1
      let zero = 0
      let add = ( + )
    end)

let run lines =
  let map = parse lines in
  D.shortest_path map map.start map.end_
  |> snd |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 31 |}]

let run2 lines =
  let map = parse lines in
  Map.fold map.heights ~init:[] ~f:(fun ~key ~data acc ->
      if data = 1 then key :: acc else acc)
  |> List.map ~f:(fun start ->
         match D.shortest_path map start map.end_ with
         | _, n -> Some n
         | exception Caml.Not_found -> None)
  |> List.filter_opt
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn |> Int.to_string |> print_endline

let%expect_test "run" =
  run2 sample;
  [%expect {| 29 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
