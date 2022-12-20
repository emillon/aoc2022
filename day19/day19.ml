open Base
open Stdio

type blueprint = {
  id : int;
  cost_ore_robot : int;
  cost_clay_robot : int;
  cost_obsidian_robot_ore : int;
  cost_obsidian_robot_clay : int;
  cost_geode_robot_ore : int;
  cost_geode_robot_obsidian : int;
}
[@@deriving sexp]

let parse_line s =
  let open Angstrom in
  let number = Int.of_string <$> take_while1 Char.is_digit in
  let line =
    let+ id = string "Blueprint " *> number <* string ": Each ore robot costs "
    and+ cost_ore_robot = number <* string " ore. Each clay robot costs "
    and+ cost_clay_robot = number <* string " ore. Each obsidian robot costs "
    and+ cost_obsidian_robot_ore = number <* string " ore and "
    and+ cost_obsidian_robot_clay =
      number <* string " clay. Each geode robot costs "
    and+ cost_geode_robot_ore = number <* string " ore and "
    and+ cost_geode_robot_obsidian = number <* string " obsidian." in
    {
      id;
      cost_ore_robot;
      cost_clay_robot;
      cost_obsidian_robot_ore;
      cost_obsidian_robot_clay;
      cost_geode_robot_ore;
      cost_geode_robot_obsidian;
    }
  in
  parse_string ~consume:All line s |> Result.ok_or_failwith

let parse l = List.map ~f:parse_line l

let sample =
  [
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. \
     Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore \
     and 7 obsidian.";
    "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. \
     Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore \
     and 12 obsidian.";
  ]

let%expect_test "parse" =
  parse sample |> [%sexp_of: blueprint list] |> print_s;
  [%expect
    {|
    (((id 1) (cost_ore_robot 4) (cost_clay_robot 2) (cost_obsidian_robot_ore 3)
      (cost_obsidian_robot_clay 14) (cost_geode_robot_ore 2)
      (cost_geode_robot_obsidian 7))
     ((id 2) (cost_ore_robot 2) (cost_clay_robot 3) (cost_obsidian_robot_ore 3)
      (cost_obsidian_robot_clay 8) (cost_geode_robot_ore 3)
      (cost_geode_robot_obsidian 12))) |}]

type state = {
  ore : int;
  ore_robots : int;
  clay : int;
  clay_robots : int;
  obsidian : int;
  obsidian_robots : int;
  geode : int;
  geode_robots : int;
  remaining : int;
}
[@@deriving sexp]

let tick state =
  let remaining = state.remaining - 1 in
  if remaining >= 0 then
    Some
      {
        state with
        ore = state.ore + state.ore_robots;
        clay = state.clay + state.clay_robots;
        obsidian = state.obsidian + state.obsidian_robots;
        geode = state.geode + state.geode_robots;
        remaining;
      }
  else None

let action_buy_ore_robot blueprint state =
  let ore = state.ore - blueprint.cost_ore_robot in
  if ore >= 0 then Some { state with ore } else None

let action_get_ore_robot state =
  { state with ore_robots = state.ore_robots + 1 }

let action_make_ore_robot blueprint state =
  let open Option.Let_syntax in
  let%bind state = action_buy_ore_robot blueprint state in
  let%map state = tick state in
  action_get_ore_robot state

let action_buy_clay_robot blueprint state =
  let ore = state.ore - blueprint.cost_clay_robot in
  if ore >= 0 then Some { state with ore } else None

let action_get_clay_robot state =
  { state with clay_robots = state.clay_robots + 1 }

let action_make_clay_robot blueprint state =
  let open Option.Let_syntax in
  let%bind state = action_buy_clay_robot blueprint state in
  let%map state = tick state in
  action_get_clay_robot state

let action_buy_obsidian_robot blueprint state =
  let ore = state.ore - blueprint.cost_obsidian_robot_ore in
  let clay = state.clay - blueprint.cost_obsidian_robot_clay in
  if ore >= 0 && clay >= 0 then Some { state with ore; clay } else None

let action_get_obsidian_robot state =
  { state with obsidian_robots = state.obsidian_robots + 1 }

let action_make_obsidian_robot blueprint state =
  let open Option.Let_syntax in
  let%bind state = action_buy_obsidian_robot blueprint state in
  let%map state = tick state in
  action_get_obsidian_robot state

let action_buy_geode_robot blueprint state =
  let ore = state.ore - blueprint.cost_geode_robot_ore in
  let obsidian = state.obsidian - blueprint.cost_geode_robot_obsidian in
  if ore >= 0 && obsidian >= 0 then Some { state with ore; obsidian } else None

let action_get_geode_robot state =
  { state with geode_robots = state.geode_robots + 1 }

let action_make_geode_robot blueprint state =
  let open Option.Let_syntax in
  let%bind state = action_buy_geode_robot blueprint state in
  let%map state = tick state in
  action_get_geode_robot state

let goal_make_robot blueprint state action =
  let rec go = function
    | None -> None
    | Some state -> (
        match action blueprint state with
        | Some _ as r -> r
        | None -> go (tick state))
  in
  go (Some state)

let init_state ~n =
  {
    ore = 0;
    ore_robots = 1;
    clay = 0;
    clay_robots = 0;
    obsidian = 0;
    obsidian_robots = 0;
    geode = 0;
    geode_robots = 0;
    remaining = n;
  }

let action_wait _blueprint state = tick state

let next blueprint state =
  let max_ore_cost =
    let ( || ) = Int.max in
    blueprint.cost_ore_robot || blueprint.cost_clay_robot
    || blueprint.cost_obsidian_robot_ore || blueprint.cost_geode_robot_ore
  in
  let keep state =
    if state.ore_robots > max_ore_cost then false
    else if state.clay_robots > blueprint.cost_obsidian_robot_clay then false
    else if state.obsidian_robots > blueprint.cost_geode_robot_obsidian then
      false
    else true
  in
  let actions =
    [
      action_make_ore_robot;
      action_make_clay_robot;
      action_make_obsidian_robot;
      action_make_geode_robot;
    ]
  in
  let r =
    List.filter_map actions ~f:(fun action ->
        Option.bind (goal_make_robot blueprint state action) ~f:(fun s ->
            Option.some_if (keep s) s))
  in
  if List.is_empty r then
    goal_make_robot blueprint state action_wait |> Option.to_list
  else r

let score ~n blueprint =
  let best_score = ref Int.min_value in
  let rec go state =
    best_score := Int.max !best_score state.geode;
    next blueprint state |> List.iter ~f:go
  in
  go (init_state ~n);
  !best_score

let run lines =
  parse lines
  |> List.foldi ~init:0 ~f:(fun i sum blueprint ->
         let score = score blueprint ~n:24 in
         sum + ((i + 1) * score))
  |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 33 |}]

let run2 lines =
  List.take (parse lines) 3
  |> List.map ~f:(score ~n:32)
  |> List.fold ~init:1 ~f:( * ) |> Int.to_string |> print_endline

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
