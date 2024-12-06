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
module PointSet = Set.Make (Point)

module Part_1 = struct
  let next_direction = function
    | Grid.N -> Grid.E
    | Grid.E -> Grid.S
    | Grid.S -> Grid.W
    | Grid.W -> Grid.N
    | _ -> failwith "Impossible"

  let turn grid position direction =
    let direction = next_direction direction in
    let next_position = Grid.move direction position in
    (direction, next_position)

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

  let find_guard =
    let ( = ) = Char.equal in
    Grid.find (fun _ c -> c = '<' || c = '^' || c = '>' || c = 'v')

  let guard_direction = function
    | '^' -> Grid.N
    | '>' -> Grid.E
    | 'v' -> Grid.S
    | '<' -> Grid.W
    | _ -> failwith "Wrong direction"

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
  let solve input = 0
  let%test "sample data" = Test.(run int (solve sample) ~expect:0)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  (* Run.solve_int (module Part_2); *)
  ()
