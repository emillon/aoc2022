open Base
open Stdio

let sample =
  [
    "        ...#";
    "        .#..";
    "        #...";
    "        ....";
    "...#.......#";
    "........#...";
    "..#....#....";
    "..........#.";
    "        ...#....";
    "        .....#..";
    "        .#......";
    "        ......#.";
    "";
    "10R5L5R10L4R5L5";
  ]

module Dir = struct
  type t = R | D | L | U [@@deriving sexp]

  let char = function R -> '>' | D -> 'v' | L -> '<' | U -> '^'
  let turn_left = function R -> U | D -> R | L -> D | U -> L
  let turn_right = function R -> D | D -> L | L -> U | U -> R
  let value = function R -> 0 | D -> 1 | L -> 2 | U -> 3
end

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, sexp]
  end

  include T
  include Comparator.Make (T)

  let shift (row, col) (dir : Dir.t) =
    match dir with
    | R -> (row, col + 1)
    | L -> (row, col - 1)
    | U -> (row - 1, col)
    | D -> (row + 1, col)

  let compare_col (_, a) (_, b) = Int.compare a b
  let compare_row (a, _) (b, _) = Int.compare a b
  let same_row a b = compare_row a b = 0
  let same_col a b = compare_col a b = 0
end

type tile = Floor | Wall [@@deriving sexp]
type map = tile Map.M(Pos).t [@@deriving sexp]
type move = L | R | Forward of int [@@deriving sexp]

let forward n = Forward n

type state = { map : map; moves : move list; pos : Pos.t; dir : Dir.t }
[@@deriving sexp]

let parse_map lines =
  let ( let* ) l k = List.concat_mapi l ~f:(fun i x -> k (i + 1, x)) in
  (let* row_num, line = lines in
   let* col_num, c = String.to_list line in
   let pos = (row_num, col_num) in
   match c with
   | ' ' -> []
   | '.' -> [ (pos, Floor) ]
   | '#' -> [ (pos, Wall) ]
   | _ -> assert false)
  |> Map.of_alist_exn (module Pos)

let parse_moves s =
  let open Angstrom in
  let number = Int.of_string <$> take_while1 Char.is_digit in
  let moves =
    many1
      (choice
         [ forward <$> number; char 'L' *> return L; char 'R' *> return R ])
  in
  parse_string ~consume:All moves s |> Result.ok_or_failwith

let initial_pos map = Map.min_elt_exn map |> fst

let parse lines =
  let map_lines, rest = List.split_while ~f:(Fn.non String.is_empty) lines in
  let map = parse_map map_lines in
  let moves = parse_moves (List.nth_exn rest 1) in
  let pos = initial_pos map in
  { map; moves; pos; dir = R }

let view state =
  let xmin, xmax, ymin, ymax =
    Map.fold state.map
      ~init:(Int.max_value, Int.min_value, Int.max_value, Int.min_value)
      ~f:(fun ~key:(x, y) ~data:_ (xmin, xmax, ymin, ymax) ->
        (Int.min xmin x, Int.max xmax x, Int.min ymin y, Int.max ymax y))
  in
  for x = xmin to xmax do
    for y = ymin to ymax do
      if Pos.equal (x, y) state.pos then printf "%c" (Dir.char state.dir)
      else
        match Map.find state.map (x, y) with
        | None -> printf " "
        | Some Floor -> printf "."
        | Some Wall -> printf "#"
    done;
    printf "\n"
  done

let%expect_test "parse" =
  parse sample |> view;
  [%expect
    {|
            >..#
            .#..
            #...
            ....
    ...#.......#
    ........#...
    ..#....#....
    ..........#.
            ...#....
            .....#..
            .#......
            ......#. |}]

let find_target state =
  let target = Pos.shift state.pos state.dir in
  if Map.mem state.map target then target
  else
    let f, compare =
      match state.dir with
      | R | L -> (Pos.same_row state.pos, Pos.compare_col)
      | D | U -> (Pos.same_col state.pos, Pos.compare_row)
    in
    let get =
      match state.dir with R | D -> List.min_elt | U | L -> List.max_elt
    in
    Map.keys state.map |> List.filter ~f |> get ~compare |> Option.value_exn

type face = F1 | F2 | F3 | F4 | F5 | F6

let find_face (row, col) =
  let row_n = (row - 1) / 50 in
  let col_n = (col - 1) / 50 in
  match (row_n, col_n) with
  | 0, 0 -> None
  | 0, 1 -> Some F1
  | 0, 2 -> Some F2
  | 3, 0 -> Some F6
  | 2, 1 -> Some F5
  | 2, 0 -> Some F4
  | 1, 1 -> Some F3
  | _ -> raise_s [%message "face" (row_n : int) (col_n : int)]

let find_target_cube state =
  let target = Pos.shift state.pos state.dir in
  if Map.mem state.map target then (target, state.dir)
  else
    let face = find_face state.pos in
    match face with
    | None -> (
        (* sample *)
        match (state.pos, state.dir) with
        | (6, 12), R -> ((6 + 3, 12 + 3), D)
        | (12, 11), D -> ((8, 2), U)
        | (5, 7), U -> ((3, 9), R)
        | _ -> assert false)
    | Some f -> (
        (* input *)
        let row, col = state.pos in
        match (f, state.dir) with
        (* A *)
        | F1, L -> ((151 - row, 1), R)
        (* B *)
        | F2, U -> ((200, col - 100), U)
        (* C *)
        | F6, R -> ((150, row - 100), U)
        (* D *)
        | F5, R -> ((151 - row, 150), L)
        (* E *)
        | F6, L -> ((1, row - 100), D)
        (* F *)
        | F2, D -> ((col - 50, 100), L)
        (* G *)
        | F4, U -> ((col + 50, 51), R)
        (* rev A *)
        | F4, L -> ((151 - row, 51), R)
        (* rev B *)
        | F6, D -> ((1, col + 100), D)
        (* rev C *)
        | F5, D -> ((100 + col, 50), L)
        (* rev D *)
        | F2, R -> ((151 - row, 100), L)
        (* rev E *)
        | F1, U -> ((col + 100, 1), R)
        (* rev F *)
        | F3, R -> ((50, row + 50), U)
        (* rev G *)
        | F3, L -> ((101, row - 50), D)
        | F1, (R | D) -> assert false
        | F2, L -> assert false
        | F3, (U | D) -> assert false
        | F4, (R | D) -> assert false
        | F5, (L | U) -> assert false
        | F6, U -> assert false)

let move_forward ~cube state =
  let target, dir =
    if cube then find_target_cube state else (find_target state, state.dir)
  in
  match Map.find_exn state.map target with
  | Floor -> { state with pos = target; dir }
  | Wall -> state

let next ~cube state =
  match state.moves with
  | [] -> None
  | Forward n :: moves ->
      Some (Fn.apply_n_times ~n (move_forward ~cube) { state with moves })
  | L :: moves -> Some { state with dir = Dir.turn_left state.dir; moves }
  | R :: moves -> Some { state with dir = Dir.turn_right state.dir; moves }

let%expect_test "next" =
  let s = ref (parse sample) in
  for _ = 1 to 13 do
    s := Option.value_exn (next ~cube:false !s);
    view !s;
    printf "\n"
  done;
  [%expect
    {|
            ..>#
            .#..
            #...
            ....
    ...#.......#
    ........#...
    ..#....#....
    ..........#.
            ...#....
            .....#..
            .#......
            ......#.

            ..v#
            .#..
            #...
            ....
    ...#.......#
    ........#...
    ..#....#....
    ..........#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    ........#.v.
    ..#....#....
    ..........#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    ........#.>.
    ..#....#....
    ..........#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    ...>....#...
    ..#....#....
    ..........#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    ...v....#...
    ..#....#....
    ..........#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    ........#...
    ..#....#....
    ...v......#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    ........#...
    ..#....#....
    ...>......#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    ........#...
    ..#....#....
    .......>..#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    ........#...
    ..#....#....
    .......v..#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    .......v#...
    ..#....#....
    ..........#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    .......>#...
    ..#....#....
    ..........#.
            ...#....
            .....#..
            .#......
            ......#.

            ...#
            .#..
            #...
            ....
    ...#.......#
    .......>#...
    ..#....#....
    ..........#.
            ...#....
            .....#..
            .#......
            ......#. |}]

let password { pos = row, col; dir; _ } =
  (1000 * row) + (4 * col) + Dir.value dir

let go ~cube lines =
  let state = ref (parse lines) in
  let continue = ref true in
  while !continue do
    match next ~cube !state with
    | None -> continue := false
    | Some s -> state := s
  done;
  password !state |> Int.to_string |> print_endline

let run lines = go ~cube:false lines

let%expect_test "run" =
  run sample;
  [%expect {| 6032 |}]

let run2 lines = go ~cube:true lines

let%expect_test "run2" =
  run2 sample;
  [%expect {| 5031 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
