open ContainersLabels

let sample =
  {|
...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9
|} |> String.trim

let sample2 =
  {|
 89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|}
  |> String.trim

let sample3 =
  {|
.....0.
..4321.
..5..2.
..6543.
..7..4.
..8765.
..9....
|} |> String.trim

module Point = Util.Point
module PointSet = Set.Make (Point)

module Part_1 = struct
  let count_trails grid starting_position =
    let rec loop current_position current_value =
      if current_value = 9 then
        (* let () = *)
        (*   Format.printf "Found trail leading to 9 at %a@." Util.Point.pp *)
        (*     current_position *)
        (* in *)
        PointSet.singleton current_position
      else
        let next_positions =
          Grid.fold4
            (fun pos n acc -> if n - 1 = current_value then pos :: acc else acc)
            grid current_position []
        in
        List.map ~f:(fun pos -> loop pos (current_value + 1)) next_positions
        |> List.fold_left ~f:PointSet.union ~init:PointSet.empty
    in
    loop starting_position 0 |> PointSet.cardinal

  let solve input =
    let grid =
      Util.parse_as_grid input
      |> Grid.map (fun _ c -> Char.code c - Char.code '0')
    in
    Grid.fold (fun pos n acc -> if n = 0 then pos :: acc else acc) grid []
    |> List.map ~f:(count_trails grid)
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:2)
  let%test "sample data 2" = Test.(run int (solve sample2) ~expect:36)
end

module Part_2 = struct
  let count_trails grid starting_position =
    let rec loop current_position current_value =
      if current_value = 9 then
        (* let () = *)
        (*   Format.printf "Found trail leading to 9 at %a@." Util.Point.pp *)
        (*     current_position *)
        (* in *)
        1
      else
        let next_positions =
          Grid.fold4
            (fun pos n acc -> if n - 1 = current_value then pos :: acc else acc)
            grid current_position []
        in
        List.map ~f:(fun pos -> loop pos (current_value + 1)) next_positions
        |> Util.sum
    in
    loop starting_position 0

  let solve input =
    let grid =
      Util.parse_as_grid input
      |> Grid.map (fun _ c -> Char.code c - Char.code '0')
    in
    Grid.fold (fun pos n acc -> if n = 0 then pos :: acc else acc) grid []
    |> List.map ~f:(count_trails grid)
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample3) ~expect:3)
  let%test "sample data" = Test.(run int (solve sample2) ~expect:81)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
