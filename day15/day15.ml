open Base
open Stdio

type pos = int * int [@@deriving sexp_of]
type data = { sensor : pos; closest_beacon : pos } [@@deriving sexp_of]

module Parser = struct
  let parse_using p s =
    Angstrom.parse_string ~consume:All p s |> Result.ok_or_failwith

  let number =
    let open Angstrom in
    let* n_s = take_while1 (fun c -> Char.equal '-' c || Char.is_digit c) in
    return (Int.of_string n_s)

  let position =
    let open Angstrom in
    let* _ = string "x=" in
    let* x = number in
    let* _ = string ", y=" in
    let* y = number in
    return (x, y)

  let%expect_test "position" =
    let test s = parse_using position s |> [%sexp_of: pos] |> print_s in
    test "x=2, y=18";
    [%expect {| (2 18) |}];
    test "x=-2, y=15";
    [%expect {| (-2 15) |}]

  let parse_line =
    let open Angstrom in
    let line =
      let* _ = string "Sensor at " in
      let* sensor = position in
      let* _ = string ": closest beacon is at " in
      let* closest_beacon = position in
      return { sensor; closest_beacon }
    in
    parse_using line

  let lines l = List.map ~f:parse_line l
end

let parse = Parser.lines

let sample =
  [
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15";
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16";
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3";
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16";
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16";
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16";
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10";
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10";
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10";
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17";
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22";
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3";
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3";
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3";
  ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: data list] |> print_s;
  [%expect
    {|
    (((sensor (2 18)) (closest_beacon (-2 15)))
     ((sensor (9 16)) (closest_beacon (10 16)))
     ((sensor (13 2)) (closest_beacon (15 3)))
     ((sensor (12 14)) (closest_beacon (10 16)))
     ((sensor (10 20)) (closest_beacon (10 16)))
     ((sensor (14 17)) (closest_beacon (10 16)))
     ((sensor (8 7)) (closest_beacon (2 10)))
     ((sensor (2 0)) (closest_beacon (2 10)))
     ((sensor (0 11)) (closest_beacon (2 10)))
     ((sensor (20 14)) (closest_beacon (25 17)))
     ((sensor (17 20)) (closest_beacon (21 22)))
     ((sensor (16 7)) (closest_beacon (15 3)))
     ((sensor (14 3)) (closest_beacon (15 3)))
     ((sensor (20 1)) (closest_beacon (15 3)))) |}]

let dist (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)
let radius { sensor; closest_beacon } = dist sensor closest_beacon

let%expect_test "radius" =
  parse sample
  |> List.map ~f:(fun x -> (x, radius x))
  |> [%sexp_of: (data * int) list] |> print_s;
  [%expect
    {|
    ((((sensor (2 18)) (closest_beacon (-2 15))) 7)
     (((sensor (9 16)) (closest_beacon (10 16))) 1)
     (((sensor (13 2)) (closest_beacon (15 3))) 3)
     (((sensor (12 14)) (closest_beacon (10 16))) 4)
     (((sensor (10 20)) (closest_beacon (10 16))) 4)
     (((sensor (14 17)) (closest_beacon (10 16))) 5)
     (((sensor (8 7)) (closest_beacon (2 10))) 9)
     (((sensor (2 0)) (closest_beacon (2 10))) 10)
     (((sensor (0 11)) (closest_beacon (2 10))) 3)
     (((sensor (20 14)) (closest_beacon (25 17))) 8)
     (((sensor (17 20)) (closest_beacon (21 22))) 6)
     (((sensor (16 7)) (closest_beacon (15 3))) 5)
     (((sensor (14 3)) (closest_beacon (15 3))) 1)
     (((sensor (20 1)) (closest_beacon (15 3))) 7)) |}]

type interval = Diet.Int.interval

let sexp_of_interval i =
  (Diet.Int.Interval.x i, Diet.Int.Interval.y i) |> [%sexp_of: int * int]

type intersection = interval option [@@deriving sexp_of]

let intersect (cx, cy) r y =
  let dy = abs (cy - y) in
  let dx = r - dy in
  if dx < 0 then None else Some (Diet.Int.Interval.make (cx - dx) (cx + dx))

let%expect_test "intersect" =
  parse sample
  |> List.map ~f:(fun d ->
         let r = radius d in
         let i = intersect d.sensor r 10 in
         (d.sensor, i))
  |> [%sexp_of: (pos * intersection) list] |> print_s;
  [%expect
    {|
    (((2 18) ()) ((9 16) ()) ((13 2) ()) ((12 14) ((12 12))) ((10 20) ())
     ((14 17) ()) ((8 7) ((2 14))) ((2 0) ((2 2))) ((0 11) ((-2 2)))
     ((20 14) ((16 24))) ((17 20) ()) ((16 7) ((14 18))) ((14 3) ()) ((20 1) ())) |}]

let singleton i = Diet.Int.add i Diet.Int.empty
let singleton_int n = singleton (Diet.Int.Interval.make n n)

let diet_of_list = function
  | [] -> Diet.Int.empty
  | h :: t -> List.fold ~init:h t ~f:Diet.Int.union

let get_line m y =
  List.fold m ~init:Diet.Int.empty ~f:(fun acc d ->
      let r = radius d in
      match intersect d.sensor r y with
      | Some s -> Diet.Int.add s acc
      | None -> acc)

let go lines y =
  let m = parse lines in
  let inner = get_line m y in
  let beacons =
    List.filter_map m ~f:(fun { closest_beacon = bx, by; _ } ->
        if y = by then Some (singleton_int bx) else None)
    |> diet_of_list
  in
  Diet.Int.diff inner beacons
  |> Diet.Int.cardinal |> Int.to_string |> print_endline

let%expect_test "go" =
  go sample 10;
  [%expect {| 26 |}]

let freq (x, y) = (x * 4_000_000) + y

let find m maxi =
  let exception Found of pos in
  try
    for y = 0 to maxi do
      let line = get_line m y in
      let x = Diet.Int.find_next_gap 0 line in
      if x < maxi then raise (Found (x, y))
    done;
    assert false
  with Found p -> p

let%expect_test "find" =
  let m = parse sample in
  find m 20 |> freq |> Int.to_string |> print_endline;
  [%expect {| 56000011 |}]

let run lines = go lines 2_000_000

let run2 lines =
  let m = parse lines in
  find m 4_000_000 |> freq |> Int.to_string |> print_endline

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
