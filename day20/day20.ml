open Base
open Stdio

module File = struct
  type 'a cell = { value : 'a; original_pos : int }

  let sexp_of_cell sexp_of_a { value; _ } = sexp_of_a value

  type t = int cell array [@@deriving sexp_of]

  let map a ~f = Array.map a ~f:(fun c -> { c with value = f c.value })

  let of_list l =
    Array.of_list_mapi l ~f:(fun original_pos value -> { original_pos; value })

  let with_original_pos a ~pos =
    Array.find_mapi_exn a ~f:(fun i { original_pos; _ } ->
        Option.some_if (original_pos = pos) i)

  let rotate_left src ~start ~end_ =
    assert (start < end_);
    let dst = Array.copy src in
    Array.blit ~src ~dst ~src_pos:(start + 1) ~dst_pos:start ~len:(end_ - start);
    Array.set dst end_ (Array.get src start);
    dst

  let rotate_right src ~start ~end_ =
    let n = Array.length src in
    assert (start < end_);
    assert (start >= 0);
    assert (start < n - 1);
    assert (end_ >= 1);
    assert (end_ < n);
    let dst = Array.copy src in
    Array.blit ~src ~dst ~src_pos:start ~dst_pos:(start + 1) ~len:(end_ - start);
    Array.set dst start (Array.get src end_);
    dst

  let rotate_right' src ~start ~end_ = rotate_right src ~end_:start ~start:end_

  let rotate a ~start ~end_ =
    if start = end_ then a
    else if end_ > start then rotate_left a ~start ~end_
    else rotate_right' a ~start ~end_

  let get a i =
    let c = Array.get a i in
    c.value

  let length = Array.length

  let swap_at a ~src =
    let v = get a src in
    let n = length a in
    let end_ = (src + v) % (n - 1) in
    rotate a ~start:src ~end_

  let at a d =
    let zero =
      Array.find_mapi_exn a ~f:(fun i { value; _ } ->
          Option.some_if (value = 0) i)
    in
    let n = length a in
    get a ((zero + d) % n)
end

let parse l = List.map ~f:Int.of_string l |> File.of_list
let sample = [ "1"; "2"; "-3"; "3"; "-2"; "0"; "4" ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: File.t] |> print_s;
  [%expect {| (1 2 -3 3 -2 0 4) |}]

let swap_nth f ~n =
  let src = File.with_original_pos f ~pos:n in
  File.swap_at f ~src

let%expect_test "swap_nth" =
  let f = parse sample in
  f |> [%sexp_of: File.t] |> print_s;
  [%expect {| (1 2 -3 3 -2 0 4) |}];
  let f = f |> swap_nth ~n:0 in
  f |> [%sexp_of: File.t] |> print_s;
  [%expect {| (2 1 -3 3 -2 0 4) |}];
  let f = f |> swap_nth ~n:1 in
  f |> [%sexp_of: File.t] |> print_s;
  [%expect {| (1 -3 2 3 -2 0 4) |}];
  let f = f |> swap_nth ~n:2 in
  f |> [%sexp_of: File.t] |> print_s;
  [%expect {| (1 2 3 -2 -3 0 4) |}];
  let f = f |> swap_nth ~n:3 in
  f |> [%sexp_of: File.t] |> print_s;
  [%expect {| (1 2 -2 -3 0 3 4) |}];
  let f = f |> swap_nth ~n:4 in
  f |> [%sexp_of: File.t] |> print_s;
  [%expect {| (-2 1 2 -3 0 3 4) |}];
  let f = f |> swap_nth ~n:5 in
  f |> [%sexp_of: File.t] |> print_s;
  [%expect {| (-2 1 2 -3 0 3 4) |}];
  let f = f |> swap_nth ~n:6 in
  f |> [%sexp_of: File.t] |> print_s;
  [%expect {| (-2 1 2 -3 4 0 3) |}]

let mix a =
  let r = ref a in
  for n = 0 to File.length a - 1 do
    r := swap_nth !r ~n
  done;
  !r

let%expect_test "mix" =
  parse sample |> mix |> [%sexp_of: File.t] |> print_s;
  [%expect {| (-2 1 2 -3 4 0 3) |}]

let coords a = File.at a 1000 + File.at a 2000 + File.at a 3000
let run lines = parse lines |> mix |> coords |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 3 |}]

let run2 lines =
  parse lines
  |> File.map ~f:(fun n -> n * 811589153)
  |> Fn.apply_n_times ~n:10 mix |> coords |> Int.to_string |> print_endline

let%expect_test "run2" =
  run2 sample;
  [%expect {| 1623178306 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
