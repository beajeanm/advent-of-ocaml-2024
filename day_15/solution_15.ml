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
    let height = List.length lines in
    let width = String.length (List.hd lines) in
    let valid_lines = lines |> List.map ~f:String.to_array |> Array.of_list in
    let map = Grid.init height width (fun (x, y) -> valid_lines.(x).(y)) in
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

let robot_position = Grid.find (fun _ c -> Char.(c = '@'))

let move grid direction =
  let robot_position = robot_position grid in
  let rec offset pos =
    let prev_pos = Grid.move (opposite direction) pos in
    let prev = Grid.get grid prev_pos in
    Grid.set grid pos prev;
    if Char.(prev <> '@') then offset prev_pos else Grid.set grid prev_pos '.'
  in
  let rec next_position pos =
    if (not (Grid.inside grid pos)) || Char.(Grid.get grid pos = '#') then None
    else if Char.(Grid.get grid pos = '.') then Some pos
    else next_position (Grid.move direction pos)
  in
  match next_position robot_position with Some pos -> offset pos | None -> ()

module Part_1 = struct
  let gps pos char sum =
    if Char.(char = 'O') then (100 * fst pos) + snd pos + sum else sum

  let solve input =
    let map, moves = parse input in
    List.iter ~f:(fun direction -> move map direction) moves;
    Grid.fold gps map 0

  let%test "small sample data" =
    Test.(run int (solve small_sample) ~expect:2028)

  let%test "sample data" = Test.(run int (solve sample) ~expect:10092)
end

module Part_2 = struct
  let widen_map map =
    let chat_at pos =
      let x, y = pos in
      let original = Grid.get map (x, y / 2) in
      let ( %= ) = Char.equal in
      match (original, y mod 2 = 0) with
      | c, _ when c %= '#' -> '#'
      | c, _ when c %= '.' -> '.'
      | c, true when c %= '@' -> '@'
      | c, false when c %= '@' -> '.'
      | c, true when c %= 'O' -> '['
      | c, false when c %= 'O' -> ']'
      | _ ->
          failwith
            (Format.sprintf "Wrong input %c %a" original Util.Point.pp pos)
    in
    let width = Grid.width map in
    let height = Grid.height map in
    Grid.init height (2 * width) chat_at

  let next_positions grid direction current =
    let next = Grid.move direction current in
    let ( == ) = Stdlib.( == ) in
    if Grid.inside grid next && (direction == Grid.N || direction == Grid.S)
    then
      let next_char = Grid.get grid next in
      if Char.(next_char = '[') then [ next; Grid.(move E next) ]
      else if Char.(next_char = ']') then [ Grid.(move W next); next ]
      else if Char.(next_char = '.') then []
      else [ next ]
    else [ next ]

  let rec trace_path grid direction path candidates =
    let is_outside =
      List.find_opt ~f:(fun pos -> not (Grid.inside grid pos)) candidates
      |> Option.is_some
    in
    let has_block () =
      List.find_opt ~f:(fun pos -> Char.(Grid.get grid pos = '#')) candidates
      |> Option.is_some
    in
    let is_all_free () =
      List.length candidates
      = List.count ~f:(fun pos -> Char.(Grid.get grid pos = '.')) candidates
    in
    if is_outside || has_block () then None
    else if is_all_free () then Some path
    else
      trace_path grid direction (candidates :: path)
        (List.flat_map ~f:(next_positions grid direction) candidates
        |> List.uniq ~eq:Util.Point.equal)

  let move grid direction =
    let move_one pos =
      let next_pos = Grid.move direction pos in
      let content = Grid.get grid pos in
      Grid.set grid next_pos content;
      Grid.set grid pos '.'
    in
    let rec offset = function
      | [] -> ()
      | positions :: tail ->
          List.iter ~f:(fun pos -> move_one pos) positions;
          offset tail
    in
    let robot_position = robot_position grid in
    let path =
      trace_path grid direction [ [ robot_position ] ]
        (next_positions grid direction robot_position)
    in
    match path with None -> () | Some path -> offset path

  let gps pos char sum =
    if Char.(char = '[') then (100 * fst pos) + snd pos + sum else sum

  let solve input =
    let map, moves = parse input in
    let wide_map = widen_map map in
    moves |> List.iter ~f:(fun direction -> move wide_map direction);
    Grid.fold gps wide_map 0

  let%test "sample data" = Test.(run int (solve sample) ~expect:9021)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
