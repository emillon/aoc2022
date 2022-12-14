open Base
open Stdio

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let all_values_between a b =
    let stride = if a <= b then 1 else -1 in
    List.range a b ~stride ~start:`inclusive ~stop:`inclusive

  let interpolate ((ax, ay), (bx, by)) =
    if ax = bx then
      let ys = all_values_between ay by in
      List.map ys ~f:(fun y -> (ax, y))
    else (
      assert (ay = by);
      let xs = all_values_between ax bx in
      List.map xs ~f:(fun x -> (x, ay)))
end

type map = { points : Set.M(Pos).t; ymax : int } [@@deriving sexp_of]

let parse_line s =
  let open Angstrom in
  let number = Int.of_string <$> take_while1 Char.is_digit in
  let pos =
    let* x = number in
    let+ y = string "," *> number in
    (x, y)
  in
  let line = sep_by1 (string " -> ") pos in
  parse_string ~consume:All line s |> Result.ok_or_failwith

let parse l = List.map l ~f:parse_line
let sample = [ "498,4 -> 498,6 -> 496,6"; "503,4 -> 502,4 -> 502,9 -> 494,9" ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: Pos.t list list] |> print_s;
  [%expect {| (((498 4) (498 6) (496 6)) ((503 4) (502 4) (502 9) (494 9))) |}]

let segments l = List.zip_exn (List.drop_last_exn l) (List.tl_exn l)

let bounding_box s =
  let open Int in
  Set.fold s ~init:(max_value, min_value, max_value, min_value)
    ~f:(fun (xmin, xmax, ymin, ymax) (x, y) ->
      (min xmin x, max xmax x, min ymin y, max ymax y))

let expand l =
  let points =
    List.concat_map l ~f:(fun l ->
        segments l |> List.concat_map ~f:Pos.interpolate)
    |> Set.of_list (module Pos)
  in
  let _, _, _, ymax = bounding_box points in
  { points; ymax }

let%expect_test "expand" =
  parse sample |> expand |> [%sexp_of: map] |> print_s;
  [%expect
    {|
    ((points
      ((494 9) (495 9) (496 6) (496 9) (497 6) (497 9) (498 4) (498 5) (498 6)
       (498 9) (499 9) (500 9) (501 9) (502 4) (502 5) (502 6) (502 7) (502 8)
       (502 9) (503 4)))
     (ymax 9)) |}]

let view { points = s; _ } =
  let xmin, xmax, ymin, ymax = bounding_box s in
  for y = ymin to ymax do
    for x = xmin to xmax do
      let c = if Set.mem s (x, y) then '#' else '.' in
      printf "%c" c
    done;
    printf "\n"
  done

let%expect_test "view" =
  parse sample |> expand |> view;
  [%expect
    {|
    ....#...##
    ....#...#.
    ..###...#.
    ........#.
    ........#.
    #########. |}]

let add_point m p =
  { points = Set.add m.points p; ymax = Int.max m.ymax (snd p) }

let add_sand m =
  let origin = (500, 0) in
  let out_of_bounds (_, y) = y >= m.ymax + 2 in
  let is_empty p = not (Set.mem m.points p) in
  let below (x, y) = (x, y + 1) in
  let below_left (x, y) = (x - 1, y + 1) in
  let below_right (x, y) = (x + 1, y + 1) in
  let rec go p =
    let b = below p in
    let bl = below_left p in
    let br = below_right p in
    if out_of_bounds p then None
    else if is_empty b then go b
    else if is_empty bl then go bl
    else if is_empty br then go br
    else Some p
  in
  if is_empty origin then
    match go origin with Some p -> Some (add_point m p) | None -> None
  else None

let%expect_test "add_sand" =
  parse sample |> expand |> add_sand |> Option.value_exn |> view;
  [%expect
    {|
    ....#...##
    ....#...#.
    ..###...#.
    ........#.
    ......#.#.
    #########. |}]

let fill m =
  let rec go m i =
    match add_sand m with Some m' -> go m' (i + 1) | None -> i
  in
  go m 0

let run lines = parse lines |> expand |> fill |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 24 |}]

let union a b =
  { points = Set.union a.points b.points; ymax = Int.max a.ymax b.ymax }

let add_ground m =
  let y = m.ymax + 2 in
  union m (expand [ [ (500 - y, y); (500 + y, y) ] ])

let%expect_test "add_ground" =
  parse sample |> expand |> add_ground |> view;
  [%expect
    {|
    .........#...##........
    .........#...#.........
    .......###...#.........
    .............#.........
    .............#.........
    .....#########.........
    .......................
    ####################### |}]

let run2 lines =
  parse lines |> expand |> add_ground |> fill |> Int.to_string |> print_endline

let%expect_test "run2" =
  run2 sample;
  [%expect {| 93 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
