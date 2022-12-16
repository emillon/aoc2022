open Base
open Stdio

type node = { src : string; flow : int; dests : string list } [@@deriving sexp]
type graph = node Map.M(String).t [@@deriving sexp]

let parse_line s =
  let open Angstrom in
  let node_name = take_while1 Char.is_uppercase in
  let number = Int.of_string <$> take_while1 Char.is_digit in
  let line =
    let+ src = string "Valve " *> node_name
    and+ flow = string " has flow rate=" *> number
    and+ dests =
      string "; "
      *> (string "tunnel leads to valve " <|> string "tunnels lead to valves ")
      *> sep_by1 (string ", ") node_name
    in
    { src; flow; dests }
  in
  parse_string ~consume:All line s |> Result.ok_or_failwith

let parse l =
  List.map ~f:parse_line l
  |> List.map ~f:(fun n -> (n.src, n))
  |> Map.of_alist_exn (module String)

let sample =
  [
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB";
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA";
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB";
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE";
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD";
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG";
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH";
    "Valve HH has flow rate=22; tunnel leads to valve GG";
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ";
    "Valve JJ has flow rate=21; tunnel leads to valve II";
  ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: graph] |> print_s;
  [%expect
    {|
    ((AA ((src AA) (flow 0) (dests (DD II BB))))
     (BB ((src BB) (flow 13) (dests (CC AA))))
     (CC ((src CC) (flow 2) (dests (DD BB))))
     (DD ((src DD) (flow 20) (dests (CC AA EE))))
     (EE ((src EE) (flow 3) (dests (FF DD))))
     (FF ((src FF) (flow 0) (dests (EE GG))))
     (GG ((src GG) (flow 0) (dests (FF HH))))
     (HH ((src HH) (flow 22) (dests (GG))))
     (II ((src II) (flow 0) (dests (AA JJ))))
     (JJ ((src JJ) (flow 21) (dests (II))))) |}]

type 'a rev_list = 'a list

let sexp_of_rev_list sexp_of_a l = [%sexp_of: a list] (List.rev l)

type state = {
  pos : string;
  closed_valves : Set.M(String).t;
  remaining : int;
  score : int;
  trace : Sexp.t rev_list;
}
[@@deriving sexp_of]

module G = struct
  type t = graph

  module V = struct
    type t = string [@@deriving compare, equal, hash]
  end

  module E = struct
    type t = V.t * V.t
    type label = unit

    let label _ = ()
    let src = fst
    let dst = snd
    let create src () dst = (src, dst)
  end

  let iter_succ_e f g v =
    let node = Map.find_exn g v in
    List.iter node.dests ~f:(fun dest -> f (E.create node.src () dest))

  let iter_vertex _ = failwith "iter_vertex"
  let fold_vertex _ = failwith "fold_vertex"
  let iter_succ _ = failwith "iter_succ"
  let fold_edges_e _ = failwith "fold_edges_e"
  let nb_vertex _ = failwith "nb_vertex"
end

module W = struct
  type edge = G.E.t
  type t = int [@@deriving compare]

  let weight _ = 1
  let add = ( + )
  let zero = 0
end

module D = Graph.Path.Dijkstra (G) (W)

let open_valve (g : graph) s =
  let node = Map.find_exn g s.pos in
  {
    s with
    score = s.score + (node.flow * (s.remaining - 1));
    closed_valves = Set.remove s.closed_valves s.pos;
    remaining = s.remaining - 1;
  }

module Cache_key = struct
  module T = struct
    type t = string * string [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let shortest_path g =
  let cache = ref (Map.empty (module Cache_key)) in
  Staged.stage (fun src dst ->
      let key = (src, dst) in
      match Map.find !cache key with
      | Some hit -> hit
      | None ->
          let r = D.shortest_path g src dst in
          cache := Map.add_exn !cache ~key ~data:r;
          r)

let go_to_valve g shortest_path s dest =
  let _, n = shortest_path s.pos dest in
  open_valve g { s with pos = dest; remaining = s.remaining - n }

module Valve_set = struct
  module T = struct
    type t = Set.M(String).t [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make (T)
end

let all_closed_valves g =
  Map.to_alist g
  |> List.filter_map ~f:(fun (n, { flow; _ }) -> Option.some_if (flow > 0) n)
  |> Set.of_list (module String)

let paths (g : graph) ~len =
  let init =
    {
      pos = "AA";
      remaining = len;
      score = 0;
      closed_valves = all_closed_valves g;
      trace = [];
    }
  in
  let shortest_path = Staged.unstage (shortest_path g) in
  let best = ref init in
  let ok = ref [] in
  let rec go s =
    if s.remaining > 0 then (
      ok := s :: !ok;
      best := if s.score > !best.score then s else !best;
      Set.iter s.closed_valves ~f:(fun dest ->
          go (go_to_valve g shortest_path s dest)))
  in
  go init;
  !ok

let best_path =
  List.fold ~init:Int.min_value ~f:(fun max s -> Int.max max s.score)

let run lines =
  parse lines |> paths ~len:30 |> best_path |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {|
    1651 |}]

let run2 lines =
  let g = parse lines in
  let ok = paths ~len:26 g in
  let max_card = Set.length (all_closed_valves g) in
  let per_set =
    List.fold ok
      ~init:(Map.empty (module Valve_set))
      ~f:(fun m s ->
        let set = s.closed_valves in
        Map.update m set ~f:(fun vo ->
            match vo with None -> s.score | Some ps -> Int.max ps s.score))
  in
  Map.fold per_set ~init:Int.min_value ~f:(fun ~key:ka ~data:da max ->
      Map.fold per_set ~init:max ~f:(fun ~key:kb ~data:db max ->
          if Set.length (Set.union ka kb) = max_card then Int.max max (da + db)
          else max))
  |> Int.to_string |> print_endline

let%expect_test "run2" =
  run2 sample;
  [%expect {|
    1707 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
