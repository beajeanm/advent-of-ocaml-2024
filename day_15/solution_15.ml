open ContainersLabels

let small_sample =
  {|
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
|}
  |> String.trim

let sample =
  {|
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
|}
  |> String.trim

let parse input =
  let parse_move = function
    | '<' -> Grid.W
    | '^' -> Grid.N
    | '>' -> Grid.E
    | 'v' -> Grid.S
    | _ -> failwith "Invalid direction"
  in
  let parse_map lines =
    let height = List.length lines - 2 in
    let width = String.length (List.hd lines) - 2 in
    let valid_lines = lines |> List.map ~f:String.to_array |> Array.of_list in
    let map =
      Grid.init height width (fun (x, y) -> valid_lines.(x + 1).(y + 1))
    in
    map
  in
  let lines = String.lines input in
  let map_lines, remaining =
    List.take_drop_while
      ~f:(fun line -> (not (String.is_empty line)) && Char.(line.[0] = '#'))
      lines
  in
  let moves =
    List.tl remaining
    |> List.flat_map ~f:(fun line ->
           String.to_list line |> List.map ~f:parse_move)
  in
  let map = parse_map map_lines in
  (map, moves)

let opposite = function
  | Grid.E -> Grid.W
  | Grid.W -> Grid.E
  | Grid.N -> Grid.S
  | Grid.S -> Grid.N
  | _ -> failwith "wrong opposite"

let show_direction = function
  | Grid.E -> "East"
  | Grid.W -> "West"
  | Grid.N -> "North"
  | Grid.S -> "South"
  | _ -> failwith "wrong opposite"

let move grid direction =
  let robot_position = Grid.find (fun _ c -> Char.(c = '@')) grid in
  let rec offset pos =
    let prev_pos = Grid.move (opposite direction) pos in
    let prev = Grid.get grid prev_pos in
    Grid.set grid pos prev;
    if Char.(prev <> '@') then offset prev_pos else Grid.set grid prev_pos '.'
  in
  let rec next_position pos =
    (* let () = Format.printf "Next position of %a@." Util.Point.pp pos in *)
    if (not (Grid.inside grid pos)) || Char.(Grid.get grid pos = '#') then None
    else if Char.(Grid.get grid pos = '.') then Some pos
    else next_position (Grid.move direction pos)
  in
  match next_position robot_position with Some pos -> offset pos | None -> ()

let gps pos char sum =
  if Char.(char = 'O') then (100 * (fst pos + 1)) + snd pos + 1 + sum else sum

module Part_1 = struct
  let solve input =
    let map, moves = parse input in
    List.iter ~f:(fun direction -> move map direction) moves;
    Grid.fold gps map 0

  let%test "small sample data" =
    Test.(run int (solve small_sample) ~expect:2028)

  let%test "sample data" = Test.(run int (solve sample) ~expect:10092)
end

module Part_2 = struct
  let solve input = 0
  let%test "sample data" = Test.(run int (solve sample) ~expect:0)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  (* Run.solve_int (module Part_2); *)
  ()
