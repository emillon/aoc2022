open Base
open Stdio

type entry = Dir of string | File of { name : string; size : int }
[@@deriving sexp]

type cmd = Cd of string | Ls of entry list [@@deriving sexp]

let entry_dir_re =
  let open Re in
  compile (seq [ bos; str "dir "; group (rep1 any); eos ])

let entry_file_re =
  let open Re in
  compile (seq [ bos; group (rep1 digit); str " "; group (rep1 any); eos ])

let parse_entry s =
  match Re.exec_opt entry_dir_re s with
  | Some g ->
      let dir = Re.Group.get g 1 in
      Dir dir
  | None ->
      let g = Re.exec entry_file_re s in
      let size = Re.Group.get g 1 |> Int.of_string in
      let name = Re.Group.get g 2 in
      File { size; name }

let cd_re =
  let open Re in
  compile (seq [ bos; str "$ cd "; group (rep1 any); eos ])

let rec parse = function
  | "$ ls" :: ls ->
      let content_lines, rest =
        List.split_while ls ~f:(fun s -> not (String.is_prefix ~prefix:"$" s))
      in
      let contents = List.map ~f:parse_entry content_lines in
      Ls contents :: parse rest
  | cd_s :: ls ->
      let g = Re.exec cd_re cd_s in
      let dir = Re.Group.get g 1 in
      Cd dir :: parse ls
  | [] -> []

let sample =
  [
    "$ cd /";
    "$ ls";
    "dir a";
    "14848514 b.txt";
    "8504156 c.dat";
    "dir d";
    "$ cd a";
    "$ ls";
    "dir e";
    "29116 f";
    "2557 g";
    "62596 h.lst";
    "$ cd e";
    "$ ls";
    "584 i";
    "$ cd ..";
    "$ cd ..";
    "$ cd d";
    "$ ls";
    "4060174 j";
    "8033020 d.log";
    "5626152 d.ext";
    "7214296 k";
  ]

let%expect_test "parse" =
  let test l = parse l |> [%sexp_of: cmd list] |> print_s in
  test [ "$ cd /" ];
  [%expect {| ((Cd /)) |}];
  test [ "$ ls" ];
  [%expect {| ((Ls ())) |}];
  test [ "$ ls"; "12 d"; "34 e" ];
  [%expect {| ((Ls ((File (name d) (size 12)) (File (name e) (size 34))))) |}];
  test sample;
  [%expect
    {|
    ((Cd /)
     (Ls
      ((Dir a) (File (name b.txt) (size 14848514))
       (File (name c.dat) (size 8504156)) (Dir d)))
     (Cd a)
     (Ls
      ((Dir e) (File (name f) (size 29116)) (File (name g) (size 2557))
       (File (name h.lst) (size 62596))))
     (Cd e) (Ls ((File (name i) (size 584)))) (Cd ..) (Cd ..) (Cd d)
     (Ls
      ((File (name j) (size 4060174)) (File (name d.log) (size 8033020))
       (File (name d.ext) (size 5626152)) (File (name k) (size 7214296))))) |}]

module Fs = struct
  type cmd_entry = entry

  type 'a t = { entries : 'a entry Map.M(String).t; extra : 'a }
  and 'a entry = Dir of 'a t | File of { size : int } [@@deriving sexp]

  let empty = { entries = Map.empty (module String); extra = () }

  let from_cmd_entry : cmd_entry -> string * unit entry = function
    | Dir name -> (name, Dir empty)
    | File { name; size } -> (name, File { size })

  let update_subdir t dir ~f =
    {
      t with
      entries =
        Map.update t.entries dir ~f:(fun vo ->
            match vo with Some (Dir d) -> Dir (f d) | _ -> assert false);
    }

  let rec set_entries t ~cwd entries =
    match cwd with
    | [] ->
        {
          entries =
            List.map entries ~f:from_cmd_entry
            |> Map.of_alist_exn (module String);
          extra = ();
        }
    | dir :: rest ->
        update_subdir t dir ~f:(fun fs -> set_entries fs ~cwd:rest entries)

  let rec add_usage t =
    let entries' =
      Map.map
        ~f:(fun entry ->
          match entry with
          | Dir contents -> Dir (add_usage contents)
          | File f -> File f)
        t.entries
    in
    let total_size =
      Map.fold entries' ~init:0 ~f:(fun ~key:_ ~data acc ->
          match data with
          | Dir { extra; _ } -> acc + extra
          | File { size } -> acc + size)
    in
    { entries = entries'; extra = total_size }

  let rec fold_dirs t ~init ~f =
    f ~extra:t.extra
      (Map.fold t.entries ~init ~f:(fun ~key:_ ~data acc ->
           match data with
           | File _ -> acc
           | Dir contents -> fold_dirs contents ~init:acc ~f))
end

module Fs_fold_state = struct
  type t = { cwd : string list; fs : unit Fs.t }

  let empty = { cwd = []; fs = Fs.empty }

  let cd t = function
    | "/" -> { t with cwd = [] }
    | ".." -> { t with cwd = List.drop_last_exn t.cwd }
    | s -> { t with cwd = t.cwd @ [ s ] }

  let ls t entries = { t with fs = Fs.set_entries t.fs ~cwd:t.cwd entries }
end

let to_fs cmds =
  let s =
    List.fold_left ~init:Fs_fold_state.empty
      ~f:(fun state cmd ->
        match cmd with
        | Cd s -> Fs_fold_state.cd state s
        | Ls l -> Fs_fold_state.ls state l)
      cmds
  in
  s.fs

let%expect_test "to_fs" =
  parse sample |> to_fs |> [%sexp_of: unit Fs.t] |> print_s;
  [%expect
    {|
    ((entries
      ((a
        (Dir
         ((entries
           ((e (Dir ((entries ((i (File (size 584))))) (extra ()))))
            (f (File (size 29116))) (g (File (size 2557)))
            (h.lst (File (size 62596)))))
          (extra ()))))
       (b.txt (File (size 14848514))) (c.dat (File (size 8504156)))
       (d
        (Dir
         ((entries
           ((d.ext (File (size 5626152))) (d.log (File (size 8033020)))
            (j (File (size 4060174))) (k (File (size 7214296)))))
          (extra ()))))))
     (extra ())) |}]

let%expect_test "usage" =
  parse sample |> to_fs |> Fs.add_usage |> [%sexp_of: int Fs.t] |> print_s;
  [%expect
    {|
    ((entries
      ((a
        (Dir
         ((entries
           ((e (Dir ((entries ((i (File (size 584))))) (extra 584))))
            (f (File (size 29116))) (g (File (size 2557)))
            (h.lst (File (size 62596)))))
          (extra 94853))))
       (b.txt (File (size 14848514))) (c.dat (File (size 8504156)))
       (d
        (Dir
         ((entries
           ((d.ext (File (size 5626152))) (d.log (File (size 8033020)))
            (j (File (size 4060174))) (k (File (size 7214296)))))
          (extra 24933642))))))
     (extra 48381165)) |}]

let run lines =
  parse lines |> to_fs |> Fs.add_usage
  |> Fs.fold_dirs ~init:0 ~f:(fun ~extra:size acc ->
         if size <= 100000 then acc + size else acc)
  |> Int.to_string |> print_endline

let%expect_test "run" =
  run sample;
  [%expect {| 95437 |}]

let run2 lines =
  let fs = parse lines |> to_fs |> Fs.add_usage in
  let total_used = fs.extra in
  let required = 30_000_000 in
  let total_disk = 70_000_000 in
  let enough size = total_used - size + required < total_disk in
  Fs.fold_dirs fs ~init:[] ~f:(fun ~extra:size acc -> size :: acc)
  |> List.filter ~f:enough
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn |> Int.to_string |> print_endline

let%expect_test "run2" =
  run2 sample;
  [%expect {| 24933642 |}]

let main () =
  match Sys.get_argv () with
  | [| _; p |] -> In_channel.with_file p ~f:In_channel.input_lines |> run
  | [| _; "--2"; p |] ->
      In_channel.with_file p ~f:In_channel.input_lines |> run2
  | _ -> assert false
