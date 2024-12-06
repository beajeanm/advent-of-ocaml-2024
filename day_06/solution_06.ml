open ContainersLabels

let sample =
  {|
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
|}
  |> String.trim

module Point = Util.Point

module PointDirection = struct
  type t = Point.t * Grid.direction

  let directions = Grid.[ N; E; S; W ]

  let direction_rank a =
    let ( == ) = Stdlib.( == ) in
    List.find_index ~f:(fun d -> d == a) directions
    |> Option.get_exn_or "no direction"

  let compare_direction a b = Int.compare (direction_rank a) (direction_rank b)
  let compare = Pair.compare Point.compare compare_direction
end

module PointDirectionSet = Set.Make (PointDirection)

let next_direction = function
  | Grid.N -> Grid.E
  | Grid.E -> Grid.S
  | Grid.S -> Grid.W
  | Grid.W -> Grid.N
  | _ -> failwith "Impossible"

let has_obstacle grid position = Char.equal '#' (Grid.get grid position)

let rec turn grid position direction =
  let direction = next_direction direction in
  let next_position = Grid.move direction position in
  if has_obstacle grid next_position then turn grid position direction
  else (direction, next_position)

let find_guard =
  let ( = ) = Char.equal in
  Grid.find (fun _ c -> c = '<' || c = '^' || c = '>' || c = 'v')

let guard_direction = function
  | '^' -> Grid.N
  | '>' -> Grid.E
  | 'v' -> Grid.S
  | '<' -> Grid.W
  | _ -> failwith "Wrong direction"

module PointSet = Set.Make (Point)

let rec record_and_move grid position direction locations =
  let locations = PointSet.add position locations in
  let next_position = Grid.move direction position in
  if Grid.inside grid next_position then
    let has_obstacle = Char.equal '#' (Grid.get grid next_position) in
    if has_obstacle then
      let direction, position = turn grid position direction in
      record_and_move grid position direction locations
    else record_and_move grid next_position direction locations
  else locations

module Part_1 = struct
  let solve input =
    let grid = Util.parse_as_grid input in
    let guard_position = find_guard grid in
    let locations =
      record_and_move grid guard_position
        (guard_direction (Grid.get grid guard_position))
        PointSet.empty
    in
    PointSet.to_seq locations |> Seq.length

  let%test "sample data" = Test.(run int (solve sample) ~expect:41)
end

module Part_2 = struct
  let rec record_and_move_with_loops grid position direction locations =
    if has_obstacle grid position then failwith "Walking on obstacle"
    else if PointDirectionSet.mem (position, direction) locations then true
    else
      let locations = PointDirectionSet.add (position, direction) locations in
      let next_position = Grid.move direction position in
      if Grid.inside grid next_position then
        if has_obstacle grid next_position then
          let direction, position = turn grid position direction in
          record_and_move_with_loops grid position direction locations
        else record_and_move_with_loops grid next_position direction locations
      else false

  let add_obstacle grid position =
    let new_grid = Grid.copy grid in
    Grid.set new_grid position '#';
    new_grid

  let solve input =
    let grid = Util.parse_as_grid input in
    let guard_position = find_guard grid in
    let guard_direction = guard_direction (Grid.get grid guard_position) in
    let locations =
      record_and_move grid guard_position guard_direction PointSet.empty
    in
    let locations = PointSet.remove guard_position locations in
    PointSet.to_seq locations
    |> Seq.filter (fun position ->
           record_and_move_with_loops
             (add_obstacle grid position)
             guard_position guard_direction PointDirectionSet.empty)
    |> Seq.length

  let%test "sample data" = Test.(run int (solve sample) ~expect:6)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
