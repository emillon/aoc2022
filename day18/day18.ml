open Base
open Stdio

module Point = struct
  module T = struct
    type t = int * int * int [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
end

let parse_line s =
  let open Angstrom in
  let num = Int.of_string <$> take_while1 Char.is_digit in
  let sep = string "," in
  let line =
    let+ x = num <* sep and+ y = num <* sep and+ z = num in
    (x, y, z)
  in
  parse_string ~consume:All line s |> Result.ok_or_failwith

let parse = List.map ~f:parse_line

let sample =
  [
    "2,2,2";
    "1,2,2";
    "3,2,2";
    "2,1,2";
    "2,3,2";
    "2,2,1";
    "2,2,3";
    "2,2,4";
    "2,2,6";
    "1,2,5";
    "3,2,5";
    "2,1,5";
    "2,3,5";
  ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: Point.t list] |> print_s;
  [%expect
    {|
    ((2 2 2) (1 2 2) (3 2 2) (2 1 2) (2 3 2) (2 2 1) (2 2 3) (2 2 4) (2 2 6)
     (1 2 5) (3 2 5) (2 1 5) (2 3 5)) |}]

let shift (x, y, z) (dx, dy, dz) = (x + dx, y + dy, z + dz)

let neighbors p =
  List.map ~f:(shift p)
    [ (0, 0, 1); (0, 0, -1); (0, 1, 0); (0, -1, 0); (1, 0, 0); (-1, 0, 0) ]

let find l x = if List.mem l x ~equal:[%equal: Point.t] then 1 else 0

let run lines =
  let ps = parse lines in
  List.fold ps ~init:0 ~f:(fun acc p ->
      acc + 6
      - List.fold (neighbors p) ~init:0 ~f:(fun acc n -> acc + find ps n))
  |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 64 |}]

let run2 lines =
  let ps = parse lines in
  let xmin, xmax, ymin, ymax, zmin, zmax =
    List.fold ps
      ~init:
        ( Int.max_value,
          Int.min_value,
          Int.max_value,
          Int.min_value,
          Int.max_value,
          Int.min_value )
      ~f:(fun (xmin, xmax, ymin, ymax, zmin, zmax) (x, y, z) ->
        ( Int.min xmin x,
          Int.max xmax x,
          Int.min ymin y,
          Int.max ymax y,
          Int.min zmin z,
          Int.max zmax z ))
  in
  let water = ref (Set.empty (module Point)) in
  let rec go p =
    water := Set.add !water p;
    neighbors p
    |> List.filter ~f:(fun pn -> not (Set.mem !water pn))
    |> List.filter ~f:(fun pn -> not (List.mem ps pn ~equal:[%equal: Point.t]))
    |> List.filter ~f:(fun (x, y, z) ->
           xmin - 1 <= x
           && x <= xmax + 1
           && ymin - 1 <= y
           && y <= ymax + 1
           && zmin - 1 <= z
           && z <= zmax + 1)
    |> List.iter ~f:go
  in
  go (0, 0, 0);
  let water = !water in
  List.fold ps ~init:0 ~f:(fun acc p ->
      acc + List.count (neighbors p) ~f:(fun n -> Set.mem water n))
  |> Int.to_string |> print_endline

let%expect_test "run2" =
  run2 sample;
  [%expect {| 58 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
