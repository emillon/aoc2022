open Base
open Stdio

type dir = L | R [@@deriving sexp]

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Block = struct
  type t = B of int

  let index (B i) = i
  let first = B 0
  let next (B i) = B (i + 1)

  let all =
    [
      [ (0, 0); (1, 0); (2, 0); (3, 0) ];
      [ (1, 0); (1, 1); (1, 2); (0, 1); (2, 1) ];
      [ (0, 0); (1, 0); (2, 0); (2, 1); (2, 2) ];
      [ (0, 0); (0, 1); (0, 2); (0, 3) ];
      [ (0, 0); (0, 1); (1, 0); (1, 1) ];
    ]
    |> List.map ~f:(Set.of_list (module Pos))

  let get (B i) = List.nth_exn all (i % 5)
end

let move_pos dir (x, y) = match dir with L -> (x - 1, y) | R -> (x + 1, y)
let parse_dir = function '<' -> L | '>' -> R | _ -> assert false
let parse s = String.strip s |> String.to_list |> List.map ~f:parse_dir
let sample = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

let%expect_test "parse" =
  parse sample |> [%sexp_of: dir list] |> print_s;
  [%expect
    {|
    (R R R L L R L R R L L L R R L R R R L L L R R R L L L R L L L R R L R R L L
     R R) |}]

let move_block m block ~f =
  let block' = Set.map (module Pos) block ~f in
  if
    Set.exists block' ~f:(fun pos ->
        let x, y = pos in
        Set.mem m pos || x < 0 || x >= 7 || y < 0)
  then None
  else Some block'

let move_block_dir m block dir =
  match move_block m block ~f:(move_pos dir) with
  | None -> (block, true)
  | Some x -> (x, false)

let move_down (x, y) = (x, y - 1)
let move_block_down m block = move_block m block ~f:move_down
let move_at moves i = List.nth_exn moves (i % List.length moves)
let commit_block m block = Set.union m block
let ymax m = Set.fold m ~init:(-1) ~f:(fun n (_, y) -> Int.max n y)

let init_pos m =
  let ymax = ymax m in
  (2, ymax + 4)

let shift_block m (dx, dy) =
  Set.map (module Pos) m ~f:(fun (x, y) -> (x + dx, y + dy))

type block = Set.M(Pos).t [@@deriving sexp]

let view m =
  printf "\n";
  let ymax = ymax m in
  for y = ymax downto 0 do
    for x = 0 to 7 do
      printf "%c" (if Set.mem m (x, y) then '#' else '.')
    done;
    printf "\n"
  done

let block_pos =
  Set.fold ~init:(Int.max_value, Int.max_value) ~f:(fun (xm, ym) (x, y) ->
      (Int.min xm x, Int.min ym y))

let add_block m moves i_move block =
  let rec go m block i_move =
    let move = move_at moves i_move in
    let block, _bumped = move_block_dir m block move in
    let down = move_block_down m block in
    match down with
    | Some block -> go m block (i_move + 1)
    | None -> (commit_block m block, i_move + 1, block_pos block)
  in
  let init_block = shift_block (Block.get block) (init_pos m) in
  go m init_block i_move

let%expect_test "add_block" =
  let moves = parse sample in
  let m, i, _ = add_block (Set.empty (module Pos)) moves 0 Block.first in
  printf "%d\n" i;
  view m;
  [%expect {|
    4

    ..####.. |}];
  let m, i, _ = add_block m moves i (Block.next Block.first) in
  printf "%d\n" i;
  view m;
  [%expect {|
    8

    ...#....
    ..###...
    ...#....
    ..####.. |}]

module Trace = struct
  module T = struct
    type t = int * (int * int) * (int * int) * (int * int) * (int * int) * int
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let trace (x1, y1) (x2, y2) (x3, y3) (x4, y4) (x5, y5) n =
  (x1, (x2, y2 - y1), (x3, y3 - y2), (x4, y4 - y3), (x5, y5 - y4), n)

let sim lines target =
  let moves = parse lines in
  let rec go m0 i0 block0 seen_map height_map =
    let bi0 = Block.index block0 in
    let m1, i1, p1 = add_block m0 moves i0 block0 in
    let ym1 = ymax m1 in
    let hm1 = Map.add_exn height_map ~key:bi0 ~data:ym1 in
    let block1 = Block.next block0 in
    let m2, i2, p2 = add_block m1 moves i1 block1 in
    let bi1 = Block.index block1 in
    let hm2 = Map.add_exn hm1 ~key:bi1 ~data:(ymax m2) in
    let block2 = Block.next block1 in
    let m3, i3, p3 = add_block m2 moves i2 block2 in
    let bi2 = Block.index block2 in
    let hm3 = Map.add_exn hm2 ~key:bi2 ~data:(ymax m3) in
    let block3 = Block.next block2 in
    let m4, i4, p4 = add_block m3 moves i3 block3 in
    let bi3 = Block.index block3 in
    let hm4 = Map.add_exn hm3 ~key:bi3 ~data:(ymax m4) in
    let block4 = Block.next block3 in
    let m5, i5, p5 = add_block m4 moves i4 block4 in
    let bi4 = Block.index block4 in
    let hm5 = Map.add_exn hm4 ~key:bi4 ~data:(ymax m5) in
    let block5 = Block.next block4 in
    let tr = trace p1 p2 p3 p4 p5 (i0 % List.length moves) in
    let ymax = ymax m5 in
    match Map.add seen_map ~key:tr ~data:(bi0, ymax) with
    | `Ok seen_map' -> go m5 i5 block5 seen_map' hm5
    | `Duplicate ->
        let cycle_end_height = ymax in
        let cycle_end_i = bi0 in
        let cycle_start_i, cycle_start_height = Map.find_exn seen_map tr in
        let total_from_cycle = target - cycle_start_i in
        let cycle_length = cycle_end_i - cycle_start_i in
        let cycle_count = total_from_cycle / cycle_length in
        let after_cycle = Int.rem total_from_cycle cycle_length in
        let one_cycle_height = cycle_end_height - cycle_start_height in
        let cycle_height = one_cycle_height * cycle_count in
        let after_cycle_height =
          Map.find_exn height_map (cycle_start_i + after_cycle - 1)
          - cycle_start_height
        in
        let total_height =
          cycle_start_height + cycle_height + after_cycle_height + 1
        in
        total_height |> Int.to_string |> print_endline
  in
  go
    (Set.empty (module Pos))
    0 Block.first
    (Map.empty (module Trace))
    (Map.empty (module Int))

let run lines = sim lines 2022

let%expect_test "run" =
  run sample;
  [%expect {|
    3068 |}]

let run2 lines = sim lines 1_000_000_000_000

let%expect_test "run2" =
  run2 sample;
  [%expect {| 1514285714288 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_all |> run
  | [| _; "--2"; p |] -> In_channel.with_file p ~f:In_channel.input_all |> run2
  | _ -> assert false
