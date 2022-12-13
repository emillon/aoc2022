open Base
open Stdio

type op = Add of int | Mul of int | Square [@@deriving sexp]

let eval_op op a =
  match op with Add b -> a + b | Mul b -> a * b | Square -> a * a

type monkey = {
  items : int list;
  operation : op;
  test_divisible_by : int;
  if_true : int;
  if_false : int;
  activity : int;
}
[@@deriving sexp]

let eval_test { test_divisible_by; if_true; if_false; _ } n =
  if n % test_divisible_by = 0 then if_true else if_false

let num_re =
  let open Re in
  compile (seq [ bos; str "Monkey "; group (rep1 digit); str ":"; eos ])

let items_re =
  let open Re in
  compile
    (seq
       [
         bos;
         str "  Starting items: ";
         group (rep1 (alt [ digit; space; char ',' ]));
         eos;
       ])

let operation_re =
  let open Re in
  compile
    (seq
       [
         bos;
         str "  Operation: new = old ";
         group (alt [ str "+"; str "*" ]);
         str " ";
         group (alt [ str "old"; rep1 digit ]);
         eos;
       ])

let parse_operation s =
  let fail () = raise_s [%message "parse_operation" (s : string)] in
  match Re.exec_opt operation_re s with
  | Some g -> (
      let op = Re.Group.get g 1 in
      let no =
        match Re.Group.get g 2 with
        | "old" -> None
        | s -> Some (Int.of_string s)
      in
      match (op, no) with
      | "+", Some n -> Add n
      | "*", Some n -> Mul n
      | "*", None -> Square
      | _ -> fail ())
  | None -> fail ()

let extract_only_num_re =
  let open Re in
  compile
    (seq
       [ bos; rep1 (alt [ alpha; space; char ':' ]); group (rep1 digit); eos ])

let only_num_in s =
  let g = Re.exec extract_only_num_re s in
  Re.Group.get g 1 |> Int.of_string

let parse_monkey lines =
  match lines with
  | [
   num_line; items_line; operation_line; test_line; if_true_line; if_false_line;
  ] ->
      let num =
        let g = Re.exec num_re num_line in
        Re.Group.get g 1 |> Int.of_string
      in
      let items =
        let g = Re.exec items_re items_line in
        Re.Group.get g 1 |> String.split ~on:',' |> List.map ~f:String.strip
        |> List.map ~f:Int.of_string
      in
      let operation = parse_operation operation_line in
      let test_divisible_by = only_num_in test_line in
      let if_true = only_num_in if_true_line in
      let if_false = only_num_in if_false_line in
      ( num,
        { items; operation; test_divisible_by; if_true; if_false; activity = 0 }
      )
  | _ -> assert false

type game = monkey Map.M(Int).t [@@deriving sexp]

let update_monkey t num ~f =
  Map.update t num ~f:(function None -> assert false | Some m -> f m)

let update_items t num ~f =
  update_monkey t num ~f:(fun m -> { m with items = f m.items })

let send_item t item ~target =
  update_items t target ~f:(fun items -> items @ [ item ])

let remove_items n t = update_items t n ~f:(fun _ -> [])

let record_activity n t =
  update_monkey t n ~f:(fun m -> { m with activity = m.activity + 1 })

let parse lines =
  let rec go lines =
    let m, r = List.split_while lines ~f:(Fn.non String.is_empty) in
    let r' = match List.tl r with Some r -> go r | None -> [] in
    parse_monkey m :: r'
  in
  go lines |> Map.of_alist_exn (module Int)

let sample =
  [
    "Monkey 0:";
    "  Starting items: 79, 98";
    "  Operation: new = old * 19";
    "  Test: divisible by 23";
    "    If true: throw to monkey 2";
    "    If false: throw to monkey 3";
    "";
    "Monkey 1:";
    "  Starting items: 54, 65, 75, 74";
    "  Operation: new = old + 6";
    "  Test: divisible by 19";
    "    If true: throw to monkey 2";
    "    If false: throw to monkey 0";
    "";
    "Monkey 2:";
    "  Starting items: 79, 60, 97";
    "  Operation: new = old * old";
    "  Test: divisible by 13";
    "    If true: throw to monkey 1";
    "    If false: throw to monkey 3";
    "";
    "Monkey 3:";
    "  Starting items: 74";
    "  Operation: new = old + 3";
    "  Test: divisible by 17";
    "    If true: throw to monkey 0";
    "    If false: throw to monkey 1";
  ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: game] |> print_s;
  [%expect
    {|
    ((0
      ((items (79 98)) (operation (Mul 19)) (test_divisible_by 23) (if_true 2)
       (if_false 3) (activity 0)))
     (1
      ((items (54 65 75 74)) (operation (Add 6)) (test_divisible_by 19)
       (if_true 2) (if_false 0) (activity 0)))
     (2
      ((items (79 60 97)) (operation Square) (test_divisible_by 13) (if_true 1)
       (if_false 3) (activity 0)))
     (3
      ((items (74)) (operation (Add 3)) (test_divisible_by 17) (if_true 0)
       (if_false 1) (activity 0)))) |}]

let apply_boredom n = n / 3

let round_g ~bored_f game =
  let monkey_nums = Map.keys game in
  List.fold_left monkey_nums ~init:game ~f:(fun acc monkey_num ->
      let monkey = Map.find_exn acc monkey_num in
      List.fold_left monkey.items ~init:acc ~f:(fun acc item ->
          let item' = eval_op monkey.operation item |> bored_f in
          let target = eval_test monkey item' in
          send_item acc item' ~target |> record_activity monkey_num)
      |> remove_items monkey_num)

let round = round_g ~bored_f:apply_boredom

let%expect_test "round" =
  parse sample |> round |> [%sexp_of: game] |> print_s;
  [%expect
    {|
    ((0
      ((items (20 23 27 26)) (operation (Mul 19)) (test_divisible_by 23)
       (if_true 2) (if_false 3) (activity 2)))
     (1
      ((items (2080 25 167 207 401 1046)) (operation (Add 6))
       (test_divisible_by 19) (if_true 2) (if_false 0) (activity 4)))
     (2
      ((items ()) (operation Square) (test_divisible_by 13) (if_true 1)
       (if_false 3) (activity 3)))
     (3
      ((items ()) (operation (Add 3)) (test_divisible_by 17) (if_true 0)
       (if_false 1) (activity 5)))) |}]

let round20 = Fn.apply_n_times ~n:20 round

let%expect_test "round20" =
  parse sample |> round20 |> [%sexp_of: game] |> print_s;
  [%expect
    {|
    ((0
      ((items (10 12 14 26 34)) (operation (Mul 19)) (test_divisible_by 23)
       (if_true 2) (if_false 3) (activity 101)))
     (1
      ((items (245 93 53 199 115)) (operation (Add 6)) (test_divisible_by 19)
       (if_true 2) (if_false 0) (activity 95)))
     (2
      ((items ()) (operation Square) (test_divisible_by 13) (if_true 1)
       (if_false 3) (activity 7)))
     (3
      ((items ()) (operation (Add 3)) (test_divisible_by 17) (if_true 0)
       (if_false 1) (activity 105)))) |}]

let take2 l = List.take l 2

let monkey_business game =
  Map.to_alist game |> List.map ~f:snd
  |> List.map ~f:(fun m -> m.activity)
  |> List.sort ~compare:Int.compare
  |> List.rev |> take2 |> List.fold ~f:( * ) ~init:1

let run lines =
  parse lines |> round20 |> monkey_business |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 10605 |}]

let run2 lines =
  let game = parse lines in
  let prod =
    Map.fold ~init:1
      ~f:(fun ~key:_ ~data:monkey acc -> acc * monkey.test_divisible_by)
      game
  in
  let round10k =
    Fn.apply_n_times ~n:10_000 (round_g ~bored_f:(fun n -> n % prod))
  in
  game |> round10k |> monkey_business |> Int.to_string |> print_endline

let%expect_test "run2" =
  run2 sample;
  [%expect {| 2713310158 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
