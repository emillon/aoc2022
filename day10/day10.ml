open Base
open Stdio

type inst = Noop | Addx of int [@@deriving sexp]
type uop = U_noop | U_addx of int [@@deriving sexp]

let addx_re =
  let open Re in
  compile
    (seq [ bos; str "addx "; group (seq [ opt (str "-"); rep1 digit ]); eos ])

let parse_inst = function
  | "noop" -> Noop
  | s ->
      let g = Re.exec addx_re s in
      let n = Re.Group.get g 1 |> Int.of_string in
      Addx n

let%expect_test "parse_inst" =
  let test s = parse_inst s |> [%sexp_of: inst] |> print_s in
  test "noop";
  [%expect {| Noop |}];
  test "addx 3";
  [%expect {| (Addx 3) |}];
  test "addx -5";
  [%expect {| (Addx -5) |}]

let to_uops = function Noop -> [ U_noop ] | Addx n -> [ U_noop; U_addx n ]
let to_cycles l = List.concat_map ~f:to_uops l

let exec l =
  let cycles = to_cycles l in
  (0, 1)
  :: List.folding_mapi ~init:1
       ~f:(fun i x uop ->
         let x' = match uop with U_noop -> x | U_addx y -> x + y in
         (x', (i + 1, x')))
       cycles

let sample =
  [
    Addx 15;
    Addx ~-11;
    Addx 6;
    Addx ~-3;
    Addx 5;
    Addx ~-1;
    Addx ~-8;
    Addx 13;
    Addx 4;
    Noop;
    Addx ~-1;
    Addx 5;
    Addx ~-1;
    Addx 5;
    Addx ~-1;
    Addx 5;
    Addx ~-1;
    Addx 5;
    Addx ~-1;
    Addx ~-35;
    Addx 1;
    Addx 24;
    Addx ~-19;
    Addx 1;
    Addx 16;
    Addx ~-11;
    Noop;
    Noop;
    Addx 21;
    Addx ~-15;
    Noop;
    Noop;
    Addx ~-3;
    Addx 9;
    Addx 1;
    Addx ~-3;
    Addx 8;
    Addx 1;
    Addx 5;
    Noop;
    Noop;
    Noop;
    Noop;
    Noop;
    Addx ~-36;
    Noop;
    Addx 1;
    Addx 7;
    Noop;
    Noop;
    Noop;
    Addx 2;
    Addx 6;
    Noop;
    Noop;
    Noop;
    Noop;
    Noop;
    Addx 1;
    Noop;
    Noop;
    Addx 7;
    Addx 1;
    Noop;
    Addx ~-13;
    Addx 13;
    Addx 7;
    Noop;
    Addx 1;
    Addx ~-33;
    Noop;
    Noop;
    Noop;
    Addx 2;
    Noop;
    Noop;
    Noop;
    Addx 8;
    Noop;
    Addx ~-1;
    Addx 2;
    Addx 1;
    Noop;
    Addx 17;
    Addx ~-9;
    Addx 1;
    Addx 1;
    Addx ~-3;
    Addx 11;
    Noop;
    Noop;
    Addx 1;
    Noop;
    Addx 1;
    Noop;
    Noop;
    Addx ~-13;
    Addx ~-19;
    Addx 1;
    Addx 3;
    Addx 26;
    Addx ~-30;
    Addx 12;
    Addx ~-1;
    Addx 3;
    Addx 1;
    Noop;
    Noop;
    Noop;
    Addx ~-9;
    Addx 18;
    Addx 1;
    Addx 2;
    Noop;
    Noop;
    Addx 9;
    Noop;
    Noop;
    Noop;
    Addx ~-1;
    Addx 2;
    Addx ~-37;
    Addx 1;
    Addx 3;
    Noop;
    Addx 15;
    Addx ~-21;
    Addx 22;
    Addx ~-6;
    Addx 1;
    Noop;
    Addx 2;
    Addx 1;
    Noop;
    Addx ~-10;
    Noop;
    Noop;
    Addx 20;
    Addx 1;
    Addx 2;
    Addx 2;
    Addx ~-6;
    Addx ~-11;
    Noop;
    Noop;
    Noop;
  ]

let%expect_test "exec" =
  let test l = exec l |> [%sexp_of: (int * int) list] |> print_s in
  test sample;
  [%expect
    {|
    ((0 1) (1 1) (2 16) (3 16) (4 5) (5 5) (6 11) (7 11) (8 8) (9 8) (10 13)
     (11 13) (12 12) (13 12) (14 4) (15 4) (16 17) (17 17) (18 21) (19 21)
     (20 21) (21 20) (22 20) (23 25) (24 25) (25 24) (26 24) (27 29) (28 29)
     (29 28) (30 28) (31 33) (32 33) (33 32) (34 32) (35 37) (36 37) (37 36)
     (38 36) (39 1) (40 1) (41 2) (42 2) (43 26) (44 26) (45 7) (46 7) (47 8)
     (48 8) (49 24) (50 24) (51 13) (52 13) (53 13) (54 13) (55 34) (56 34)
     (57 19) (58 19) (59 19) (60 19) (61 16) (62 16) (63 25) (64 25) (65 26)
     (66 26) (67 23) (68 23) (69 31) (70 31) (71 32) (72 32) (73 37) (74 37)
     (75 37) (76 37) (77 37) (78 37) (79 37) (80 1) (81 1) (82 1) (83 2)
     (84 2) (85 9) (86 9) (87 9) (88 9) (89 9) (90 11) (91 11) (92 17) (93 17)
     (94 17) (95 17) (96 17) (97 17) (98 17) (99 18) (100 18) (101 18) (102 18)
     (103 25) (104 25) (105 26) (106 26) (107 26) (108 13) (109 13) (110 26)
     (111 26) (112 33) (113 33) (114 33) (115 34) (116 34) (117 1) (118 1)
     (119 1) (120 1) (121 1) (122 3) (123 3) (124 3) (125 3) (126 3) (127 11)
     (128 11) (129 11) (130 10) (131 10) (132 12) (133 12) (134 13) (135 13)
     (136 13) (137 30) (138 30) (139 21) (140 21) (141 22) (142 22) (143 23)
     (144 23) (145 20) (146 20) (147 31) (148 31) (149 31) (150 31) (151 32)
     (152 32) (153 32) (154 33) (155 33) (156 33) (157 33) (158 20) (159 20)
     (160 1) (161 1) (162 2) (163 2) (164 5) (165 5) (166 31) (167 31) (168 1)
     (169 1) (170 13) (171 13) (172 12) (173 12) (174 15) (175 15) (176 16)
     (177 16) (178 16) (179 16) (180 16) (181 7) (182 7) (183 25) (184 25)
     (185 26) (186 26) (187 28) (188 28) (189 28) (190 28) (191 37) (192 37)
     (193 37) (194 37) (195 37) (196 36) (197 36) (198 38) (199 38) (200 1)
     (201 1) (202 2) (203 2) (204 5) (205 5) (206 5) (207 20) (208 20) (209 -1)
     (210 -1) (211 21) (212 21) (213 15) (214 15) (215 16) (216 16) (217 16)
     (218 18) (219 18) (220 19) (221 19) (222 19) (223 9) (224 9) (225 9)
     (226 9) (227 29) (228 29) (229 30) (230 30) (231 32) (232 32) (233 34)
     (234 34) (235 28) (236 28) (237 17) (238 17) (239 17) (240 17)) |}]

let ( += ) r d = r := !r + d

let score trace =
  let m = trace |> Map.of_alist_exn (module Int) in
  let i = ref 20 in
  let total = ref 0 in
  let continue = ref true in
  while !continue do
    match Map.find m (!i - 1) with
    | None -> continue := false
    | Some s ->
        total += (!i * s);
        i += 40
  done;
  !total

let go instrs = exec instrs |> score |> Int.to_string |> print_endline
let run lines = List.map lines ~f:parse_inst |> go

let%expect_test "go" =
  go sample;
  [%expect {|
    13140 |}]

let crt_lines trace =
  let lines = List.chunks_of ~length:40 trace in
  List.map lines ~f:(fun subtrace ->
      List.map subtrace ~f:(fun (cycle_num, x) ->
          let crt_x = cycle_num % 40 in
          let visible = x = crt_x || x - 1 = crt_x || x + 1 = crt_x in
          if visible then '#' else '.')
      |> String.of_char_list)

let go2 instrs = exec instrs |> crt_lines

let%expect_test "go2" =
  go2 sample |> [%sexp_of: string list] |> print_s;
  [%expect
    {|
    (##..##..##..##..##..##..##..##..##..##..
     ###...###...###...###...###...###...###.
     ####....####....####....####....####....
     #####.....#####.....#####.....#####.....
     ######......######......######......####
     #######.......#######.......#######..... .) |}]

let run2 lines =
  List.map lines ~f:parse_inst |> go2 |> List.iter ~f:print_endline

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
