open Base
open Stdio

type expr = List of expr list | Int of int [@@deriving equal]

let rec expr_of_sexp = function
  | Sexp.Atom _ as s -> Int ([%of_sexp: int] s)
  | List _ as s -> List ([%of_sexp: expr list] s)

let rec sexp_of_expr = function
  | List l -> [%sexp_of: expr list] l
  | Int n -> [%sexp_of: int] n

let parse_expr s =
  String.map s ~f:(function
    | ',' -> ' '
    | '[' -> '('
    | ']' -> ')'
    | '0' .. '9' as c -> c
    | c -> raise_s [%message "parse" (c : char)])
  |> Parsexp.Single.parse_string_exn |> [%of_sexp: expr]

let%expect_test "parse_expr" =
  let test s = parse_expr s |> [%sexp_of: expr] |> print_s in
  test "[]";
  [%expect {| () |}];
  test "[[4,4],4,4]";
  [%expect {| ((4 4) 4 4) |}];
  test "[[[]]]";
  [%expect {| ((())) |}]

let rec parse = function
  | a :: b :: "" :: r -> (parse_expr a, parse_expr b) :: parse r
  | [ a; b ] -> [ (parse_expr a, parse_expr b) ]
  | l -> raise_s [%message "parse" (l : string list)]

let sample =
  [
    "[1,1,3,1,1]";
    "[1,1,5,1,1]";
    "";
    "[[1],[2,3,4]]";
    "[[1],4]";
    "";
    "[9]";
    "[[8,7,6]]";
    "";
    "[[4,4],4,4]";
    "[[4,4],4,4,4]";
    "";
    "[7,7,7,7]";
    "[7,7,7]";
    "";
    "[]";
    "[3]";
    "";
    "[[[]]]";
    "[[]]";
    "";
    "[1,[2,[3,[4,[5,6,7]]]],8,9]";
    "[1,[2,[3,[4,[5,6,0]]]],8,9]";
  ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: (expr * expr) list] |> print_s;
  [%expect
    {|
    (((1 1 3 1 1) (1 1 5 1 1)) (((1) (2 3 4)) ((1) 4)) ((9) ((8 7 6)))
     (((4 4) 4 4) ((4 4) 4 4 4)) ((7 7 7 7) (7 7 7)) (() (3)) (((())) (()))
     ((1 (2 (3 (4 (5 6 7)))) 8 9) (1 (2 (3 (4 (5 6 0)))) 8 9))) |}]

let rec compare_expr a b =
  match (a, b) with
  | Int na, Int nb -> [%compare: int] na nb
  | List la, List lb -> [%compare: expr list] la lb
  | Int _, List _ -> [%compare: expr] (List [ a ]) b
  | List _, Int _ -> [%compare: expr] a (List [ b ])

let ordered a b = [%compare: expr] a b < 1

let%expect_test "ordered" =
  parse sample
  |> List.map ~f:(fun (a, b) -> (a, b, ordered a b))
  |> [%sexp_of: (expr * expr * bool) list] |> print_s;
  [%expect
    {|
    (((1 1 3 1 1) (1 1 5 1 1) true) (((1) (2 3 4)) ((1) 4) true)
     ((9) ((8 7 6)) false) (((4 4) 4 4) ((4 4) 4 4 4) true)
     ((7 7 7 7) (7 7 7) false) (() (3) true) (((())) (()) false)
     ((1 (2 (3 (4 (5 6 7)))) 8 9) (1 (2 (3 (4 (5 6 0)))) 8 9) false)) |}]

let run lines =
  parse lines
  |> List.mapi ~f:(fun im1 (a, b) ->
         let i = im1 + 1 in
         if ordered a b then i else 0)
  |> List.fold ~init:0 ~f:( + ) |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 13 |}]

let find l target =
  List.findi_exn l ~f:(fun _ e -> [%equal: expr] e target) |> fst |> Int.succ

let collect l = List.concat_map l ~f:(fun (a, b) -> [ a; b ])

let run2 lines =
  let pkt1 = parse_expr "[[2]]" in
  let pkt2 = parse_expr "[[6]]" in
  let l =
    parse lines |> collect
    |> List.append [ pkt1; pkt2 ]
    |> List.sort ~compare:[%compare: expr]
  in
  let i1 = find l pkt1 in
  let i2 = find l pkt2 in
  i1 * i2 |> Int.to_string |> print_endline

let%expect_test "run2" =
  run2 sample;
  [%expect {| 140 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
