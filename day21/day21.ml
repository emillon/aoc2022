open Base
open Stdio

type op = Plus | Minus | Div | Mult [@@deriving equal, sexp]

type expr = Int of int | Var of string | Op of op * expr * expr
[@@deriving sexp]

type exprs = expr Map.M(String).t [@@deriving sexp]

let parse_line s =
  let open Angstrom in
  let id = take_while1 Char.is_alpha in
  let number = Int.of_string <$> take_while1 Char.is_digit in
  let op =
    choice
      [
        string "+" *> return Plus;
        string "-" *> return Minus;
        string "/" *> return Div;
        string "*" *> return Mult;
      ]
  in
  let expr =
    choice
      [
        (let+ i1 = id <* string " " and+ op = op <* string " " and+ i2 = id in
         Op (op, Var i1, Var i2));
        (let+ number in
         Int number);
        (let+ id in
         Var id);
      ]
  in
  let line = both (id <* string ": ") expr in
  parse_string ~consume:All line s |> Result.ok_or_failwith

let parse lines =
  List.map ~f:parse_line lines |> Map.of_alist_exn (module String)

let sample =
  [
    "root: pppw + sjmn";
    "dbpl: 5";
    "cczh: sllz + lgvd";
    "zczc: 2";
    "ptdq: humn - dvpt";
    "dvpt: 3";
    "lfqf: 4";
    "humn: 5";
    "ljgn: 2";
    "sjmn: drzm * dbpl";
    "sllz: 4";
    "pppw: cczh / lfqf";
    "lgvd: ljgn * ptdq";
    "drzm: hmdt - zczc";
    "hmdt: 32";
  ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: exprs] |> print_s;
  [%expect
    {|
    ((cczh (Op Plus (Var sllz) (Var lgvd))) (dbpl (Int 5))
     (drzm (Op Minus (Var hmdt) (Var zczc))) (dvpt (Int 3)) (hmdt (Int 32))
     (humn (Int 5)) (lfqf (Int 4)) (lgvd (Op Mult (Var ljgn) (Var ptdq)))
     (ljgn (Int 2)) (pppw (Op Div (Var cczh) (Var lfqf)))
     (ptdq (Op Minus (Var humn) (Var dvpt)))
     (root (Op Plus (Var pppw) (Var sjmn)))
     (sjmn (Op Mult (Var drzm) (Var dbpl))) (sllz (Int 4)) (zczc (Int 2))) |}]

let eval_op = function
  | Plus -> ( + )
  | Minus -> ( - )
  | Div -> ( / )
  | Mult -> ( * )

let rec eval expr m =
  match expr with
  | Var n -> eval (Map.find_exn m n) m
  | Int n -> n
  | Op (op, e1, e2) ->
      let n1 = eval e1 m in
      let n2 = eval e2 m in
      let f = eval_op op in
      f n1 n2

let run lines =
  parse lines |> eval (Var "root") |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 152 |}]

type xexpr = X | Int of int | Op of op * xexpr * xexpr
[@@deriving equal, sexp]

let rec expand expr m =
  match expr with
  | Var "humn" -> X
  | Var v -> expand (Map.find_exn m v) m
  | Int n -> Int n
  | Op (op, e1, e2) ->
      let x1 = expand e1 m in
      let x2 = expand e2 m in
      Op (op, x1, x2)

let rec xeval_step = function
  | Op (op, Int n1, Int n2) -> Int (eval_op op n1 n2)
  | Op (op, x1, x2) -> Op (op, xeval_step x1, xeval_step x2)
  | X -> X
  | Int n -> Int n

let simplify e =
  let rec go e =
    let e' = xeval_step e in
    if [%equal: xexpr] e e' then e else go e'
  in
  go e

let rec solve target = function
  | Op (Minus, e, Int n) -> solve (target + n) e
  | Op (Minus, Int n, e) -> solve (n - target) e
  | Op (Div, e, Int n) -> solve (target * n) e
  | Op (Plus, Int n, e) | Op (Plus, e, Int n) -> solve (target - n) e
  | Op (Mult, Int n, e) | Op (Mult, e, Int n) -> solve (target / n) e
  | X -> target
  | x -> raise_s [%message "solve" (x : xexpr) (target : int)]

let run2 lines =
  let m = parse lines in
  let e1, e2 =
    match Map.find_exn m "root" with
    | Op (_, e1, e2) -> (e1, e2)
    | _ -> assert false
  in
  expand (Op (Minus, e1, e2)) m
  |> simplify |> solve 0 |> Int.to_string |> print_endline

let%expect_test "run2" =
  run2 sample;
  [%expect {| 301 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
