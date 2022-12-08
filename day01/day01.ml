open Base
open Stdio

type state = { cur : int option; complete : int list } [@@deriving sexp_of]
type line = Blank | Num of int [@@deriving sexp_of]

let parse s = if String.is_empty s then Blank else Num (Int.of_string s)

let%expect_test "parse" =
  let test s = parse s |> sexp_of_line |> print_s in
  test "";
  [%expect {| Blank |}];
  test "12345";
  [%expect {| (Num 12345) |}]

let flush = function
  | { cur = None; complete } -> complete
  | { cur = Some c; complete } -> c :: complete

let to_list all =
  let init = { cur = None; complete = [] } in
  String.split_lines all |> List.map ~f:parse
  |> List.fold_left ~init ~f:(fun state input ->
         match (state, input) with
         | { complete; cur = Some a }, Num b -> { complete; cur = Some (a + b) }
         | { complete; cur = Some cur }, Blank ->
             { complete = cur :: complete; cur = None }
         | { complete; cur = None }, Num a -> { complete; cur = Some a }
         | { complete = _; cur = None }, Blank -> state)
  |> flush

let run all =
  to_list all
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn |> Int.to_string |> print_endline

let top l = List.take l 3

let run2 all =
  to_list all
  |> List.sort ~compare:Int.compare
  |> List.rev |> top |> List.fold ~f:( + ) ~init:0 |> Int.to_string
  |> print_endline

let sample = {|
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
|}

let%expect_test "run" =
  run sample;
  [%expect {| 24000 |}]

let%expect_test "run2" =
  run2 sample;
  [%expect {| 45000 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_all |> run
  | [| _; "--2"; p |] -> In_channel.with_file p ~f:In_channel.input_all |> run2
  | _ -> assert false
