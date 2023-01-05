open Base
open Stdio

let sample =
  [
    "....#.."; "..###.#"; "#...#.#"; ".#...##"; "#.###.."; "##.#.##"; ".#..#..";
  ]

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, sexp]
  end

  include T
  include Comparator.Make (T)
end

module Dir = struct
  type t = N | NE | NW | S | SE | SW | W | E

  let all = [ N; NE; NW; S; SE; SW; W; E ]

  let shift (row, col) = function
    | N -> (row - 1, col)
    | NE -> (row - 1, col + 1)
    | NW -> (row - 1, col - 1)
    | S -> (row + 1, col)
    | SE -> (row + 1, col + 1)
    | SW -> (row + 1, col - 1)
    | W -> (row, col - 1)
    | E -> (row, col + 1)
end

let no_elf_in dirs map pos =
  not (List.exists dirs ~f:(fun dir -> Set.mem map (Dir.shift pos dir)))

module Check = struct
  type t = Check_N | Check_S | Check_W | Check_E [@@deriving sexp]

  let neighborhood : _ -> Dir.t list = function
    | Check_N -> [ N; NE; NW ]
    | Check_S -> [ S; SE; SW ]
    | Check_W -> [ W; NW; SW ]
    | Check_E -> [ E; NE; SE ]

  let target : _ -> Dir.t = function
    | Check_N -> N
    | Check_S -> S
    | Check_W -> W
    | Check_E -> E

  let eval check map pos =
    Option.some_if (no_elf_in (neighborhood check) map pos) (target check)

  let rec eval_first checks map pos =
    match checks with
    | [] -> None
    | check :: checks -> (
        match eval check map pos with
        | Some _ as r -> r
        | None -> eval_first checks map pos)

  let initial = [ Check_N; Check_S; Check_W; Check_E ]
  let next = function h :: t -> t @ [ h ] | [] -> assert false
end

type map = Set.M(Pos).t [@@deriving sexp]
type state = { map : map; checks : Check.t list } [@@deriving sexp]

let parse lines =
  let ( let* ) l k = List.concat_mapi l ~f:(fun i x -> k (i + 1, x)) in
  let map =
    (let* row_num, line = lines in
     let* col_num, c = String.to_list line in
     let pos = (row_num, col_num) in
     match c with '.' -> [] | '#' -> [ pos ] | _ -> assert false)
    |> Set.of_list (module Pos)
  in
  { map; checks = Check.initial }

let bounding_box map =
  Set.fold map
    ~init:(Int.max_value, Int.min_value, Int.max_value, Int.min_value)
    ~f:(fun (xmin, xmax, ymin, ymax) (x, y) ->
      (Int.min xmin x, Int.max xmax x, Int.min ymin y, Int.max ymax y))

let view { map; _ } =
  let xmin, xmax, ymin, ymax = bounding_box map in
  for x = xmin to xmax do
    for y = ymin to ymax do
      if Set.mem map (x, y) then printf "#" else printf "."
    done;
    printf "\n"
  done

let%expect_test "parse" =
  parse sample |> view;
  [%expect
    {|
    ....#..
    ..###.#
    #...#.#
    .#...##
    #.###..
    ##.#.##
    .#..#.. |}]

let proposed_target map pos checks =
  if no_elf_in Dir.all map pos then None
  else
    let open Option.Let_syntax in
    let%map dir = Check.eval_first checks map pos in
    Dir.shift pos dir

let next { map; checks } =
  let candidate_map =
    Set.fold map
      ~init:(Map.empty (module Pos))
      ~f:(fun m pos ->
        match proposed_target map pos checks with
        | Some target -> Map.add_multi m ~key:target ~data:pos
        | None -> m)
  in
  let moves =
    candidate_map |> Map.to_alist
    |> List.filter_map ~f:(fun (target, sources) ->
           match sources with
           | [ source ] -> Some (source, target)
           | _ :: _ :: _ -> None
           | [] -> assert false)
    |> Map.of_alist_exn (module Pos)
  in
  let map' =
    Set.map
      (module Pos)
      map
      ~f:(fun pos ->
        match Map.find moves pos with Some target -> target | None -> pos)
  in
  { map = map'; checks = Check.next checks }

let%expect_test "next" =
  parse sample |> next |> view;
  [%expect
    {|
    .....#...
    ...#...#.
    .#..#.#..
    .....#..#
    ..#.#.##.
    #..#.#...
    #.#.#.##.
    .........
    ..#..#... |}];
  parse sample |> Fn.apply_n_times ~n:10 next |> view;
  [%expect
    {|
    ......#.....
    ..........#.
    .#.#..#.....
    .....#......
    ..#.....#..#
    #......##...
    ....##......
    .#........#.
    ...#.#..#...
    ............
    ...#..#..#.. |}]

let ground_tiles { map; _ } =
  let xmin, xmax, ymin, ymax = bounding_box map in
  ((xmax - xmin + 1) * (ymax - ymin + 1)) - Set.length map

let run lines =
  parse lines
  |> Fn.apply_n_times ~n:10 next
  |> ground_tiles |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 110 |}]

let run2 lines =
  let rec go s i =
    let s' = next s in
    if Set.equal s.map s'.map then i else go s' (i + 1)
  in
  let n = go (parse lines) 1 in
  printf "%d\n" n

let%expect_test "run2" =
  run2 sample;
  [%expect {| 20 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
