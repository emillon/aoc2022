open Base
open Stdio

let sample =
  [ "#.######"; "#>>.<^<#"; "#.<..<<#"; "#>v.><>#"; "#<^v^^>#"; "######.#" ]

module Pos = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
end

type dir = N | S | E | W [@@deriving sexp]

let dir_char = function N -> '^' | S -> 'V' | E -> '>' | W -> '<'

let dir_add (x, y) = function
  | N -> (x, y - 1)
  | S -> (x, y + 1)
  | E -> (x + 1, y)
  | W -> (x - 1, y)

type t = { dimx : int; dimy : int; blizzards : dir list Map.M(Pos).t }
[@@deriving sexp]

let is_in_bounds { dimx; dimy; _ } (x, y) =
  0 <= x && x < dimx && 0 <= y && y < dimy

let is_pos_free t pos =
  (is_in_bounds t pos
  || Pos.equal pos (0, -1)
  || Pos.equal pos (t.dimx, t.dimy - 1))
  && not (Map.mem t.blizzards pos)

let parse lines =
  let dimx = String.length (List.hd_exn lines) - 2 in
  let dimy = List.length lines - 2 in
  let blizzards = ref (Map.empty (module Pos)) in
  let add_blizzard pos dir =
    blizzards := Map.add_exn !blizzards ~key:pos ~data:[ dir ]
  in
  List.iteri lines ~f:(fun j line ->
      String.iteri line ~f:(fun i c ->
          let pos = (i - 1, j - 1) in
          match c with
          | '#' -> ()
          | '.' -> ()
          | '>' -> add_blizzard pos E
          | '<' -> add_blizzard pos W
          | '^' -> add_blizzard pos N
          | 'v' -> add_blizzard pos S
          | _ -> raise_s [%message "parse" (c : char) (i : int) (j : int)]));
  { dimx; dimy; blizzards = !blizzards }

let%expect_test "parse" =
  let test x = parse x |> [%sexp_of: t] |> print_s in
  test sample;
  [%expect
    {|
    ((dimx 6) (dimy 4)
     (blizzards
      (((0 0) (E)) ((0 2) (E)) ((0 3) (W)) ((1 0) (E)) ((1 1) (W)) ((1 2) (S))
       ((1 3) (N)) ((2 3) (S)) ((3 0) (W)) ((3 2) (E)) ((3 3) (N)) ((4 0) (N))
       ((4 1) (W)) ((4 2) (W)) ((4 3) (N)) ((5 0) (W)) ((5 1) (W)) ((5 2) (E))
       ((5 3) (E))))) |}];
  test [ "#.#####"; "#^...v#"; "#<...>#"; "#####.#" ];
  [%expect
    {|
    ((dimx 5) (dimy 2)
     (blizzards (((0 0) (N)) ((0 1) (W)) ((4 0) (S)) ((4 1) (E))))) |}]

type square = Empty | Single of dir | Multi2 | Multi3 | Multi4

let add_dir s d =
  match s with
  | Empty -> Single d
  | Single _ -> Multi2
  | Multi2 -> Multi3
  | Multi3 -> Multi4
  | Multi4 -> assert false

let add_dirs s ds = List.fold ds ~init:s ~f:add_dir

let render t =
  let a = Array.make_matrix ~dimx:t.dimy ~dimy:t.dimx Empty in
  Map.iteri t.blizzards ~f:(fun ~key:(x, y) ~data:dirs ->
      a.(y).(x) <- add_dirs a.(y).(x) dirs);
  Array.iter a ~f:(fun line ->
      Array.iter line ~f:(fun s ->
          let c =
            match s with
            | Empty -> '.'
            | Single d -> dir_char d
            | Multi2 -> '2'
            | Multi3 -> '3'
            | Multi4 -> '4'
          in
          Stdlib.print_char c);
      Stdlib.print_newline ())

let%expect_test "render" =
  parse sample |> render;
  [%expect {|
    >>.<^<
    .<..<<
    >V.><>
    <^V^^> |}]

let next t =
  let cycle (x, y) = (x % t.dimx, y % t.dimy) in
  let blizzards =
    Map.fold t.blizzards
      ~init:(Map.empty (module Pos))
      ~f:(fun ~key:pos ~data:dirs acc ->
        List.fold dirs ~init:acc ~f:(fun acc dir ->
            let new_pos = cycle (dir_add pos dir) in
            Map.add_multi acc ~key:new_pos ~data:dir))
  in
  { t with blizzards }

let%expect_test "render" =
  let r = ref (parse sample) in
  let i = ref 0 in
  let step () =
    Int.incr i;
    r := next !r;
    printf "minute %d\n" !i;
    render !r
  in
  step ();
  [%expect {|
    minute 1
    .>3.<.
    <..<<.
    >2.22.
    >V..^< |}];
  step ();
  [%expect {|
    minute 2
    .2>2..
    .^22^<
    .>2.^>
    .>..<. |}];
  step ();
  [%expect {|
    minute 3
    <^<22.
    .2<.2.
    ><2>..
    ..><.. |}];
  step ();
  [%expect {|
    minute 4
    .<..22
    <<.<..
    <2.>>.
    .^22^. |}];
  step ();
  [%expect {|
    minute 5
    2.V.<>
    <.<..<
    .^>^22
    .2..2. |}];
  step ();
  [%expect {|
    minute 6
    >2.<.<
    .2V^2<
    >..>2>
    <....> |}];
  step ();
  [%expect {|
    minute 7
    .22^2.
    <V.<2.
    >>V<>.
    >....< |}];
  step ();
  [%expect {|
    minute 8
    .<>2^.
    ..<<.<
    .22..>
    .2V^2. |}];
  step ();
  [%expect {|
    minute 9
    <.2>>.
    .<<.<.
    >2>2^.
    .V><^. |}];
  step ();
  [%expect {|
    minute 10
    .2..>2
    <2V2^.
    <>.>2.
    ..<>.. |}];
  step ();
  [%expect {|
    minute 11
    2^.^2>
    <V<.^<
    ..2.>2
    .<..>. |}];
  step ();
  [%expect {|
    minute 12
    >>.<^<
    .<..<<
    >V.><>
    <^V^^> |}];
  step ();
  [%expect {|
    minute 13
    .>3.<.
    <..<<.
    >2.22.
    >V..^< |}];
  step ();
  [%expect {|
    minute 14
    .2>2..
    .^22^<
    .>2.^>
    .>..<. |}];
  step ();
  [%expect {|
    minute 15
    <^<22.
    .2<.2.
    ><2>..
    ..><.. |}];
  step ();
  [%expect {|
    minute 16
    .<..22
    <<.<..
    <2.>>.
    .^22^. |}];
  step ();
  [%expect {|
    minute 17
    2.V.<>
    <.<..<
    .^>^22
    .2..2. |}];
  step ();
  [%expect {|
    minute 18
    >2.<.<
    .2V^2<
    >..>2>
    <....> |}]

module State = struct
  module T = struct
    type t = {
      pos : Pos.t;
      time : int;
      target : Pos.t;
      next_targets : Pos.t list;
    }
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let shortest t ~back_and_forth =
  let exception Found of int in
  let q = Queue.create () in
  let next_targets =
    if back_and_forth then [ (0, -1); (t.dimx, t.dimy - 1) ] else []
  in
  let root : State.t =
    { pos = (0, -1); time = 0; target = (t.dimx, t.dimy - 1); next_targets }
  in
  let visited = ref (Set.singleton (module State) root) in
  let t_cache = ref (Map.singleton (module Int) 0 t) in
  let at_time n =
    match Map.find !t_cache n with
    | Some v -> v
    | None ->
        let prev = Map.find_exn !t_cache (n - 1) in
        let v = next prev in
        t_cache := Map.add_exn !t_cache ~key:n ~data:v;
        v
  in
  Queue.enqueue q root;
  try
    while not (Queue.is_empty q) do
      let v = Queue.dequeue_exn q in
      let v =
        if Pos.equal v.pos v.target then
          match v.next_targets with
          | [] -> raise (Found v.time)
          | target :: next_targets -> { v with target; next_targets }
        else v
      in
      let next_positions =
        v.pos :: List.map ~f:(dir_add v.pos) [ N; S; E; W ]
      in
      let next_time = v.time + 1 in
      let next_t = at_time next_time in
      let next_states =
        List.filter_map next_positions ~f:(fun pos ->
            if is_pos_free next_t pos then
              Some { v with State.pos; time = next_time }
            else None)
      in
      List.iter next_states ~f:(fun s ->
          if not (Set.mem !visited s) then (
            visited := Set.add !visited s;
            Queue.enqueue q s))
    done;
    print_endline "not found"
  with Found n -> printf "%d\n" n

let run lines = parse lines |> shortest ~back_and_forth:false

let%expect_test "run" =
  run sample;
  [%expect {| 18 |}]

let run2 lines = parse lines |> shortest ~back_and_forth:true

let%expect_test "run2" =
  run2 sample;
  [%expect {| 54 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
