open ContainersLabels

let sample = {|
AAAA
BBCD
BBCC
EEEC
|} |> String.trim

let sample2 =
  {|
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
|}
  |> String.trim

let sample3 = {|
EEEEE
EXXXX
EEEEE
EXXXX
EEEEE
|}

module Point = Util.Point
module PointSet = Set.Make (Point)

let pp_set = PointSet.pp Point.pp

let find_regions grid =
  let rec explore_neighbours explored current_region position plant =
    Grid.fold4
      (fun next_pos next_plant (explored, current_region) ->
        if PointSet.mem next_pos explored || not (Char.equal plant next_plant)
        then (explored, current_region)
        else
          let explored = PointSet.add next_pos explored in
          let current_region = PointSet.add next_pos current_region in
          explore_neighbours explored current_region next_pos next_plant)
      grid position (explored, current_region)
  in
  Grid.fold
    (fun pos plan (explored, regions) ->
      if PointSet.mem pos explored then (explored, regions)
      else
        let explored, new_region =
          explore_neighbours explored (PointSet.singleton pos) pos plan
        in
        (explored, new_region :: regions))
    grid (PointSet.empty, [])
  |> snd

let fence_price region =
  let region_area = PointSet.cardinal region in
  let inside_borders =
    PointSet.fold
      (fun pos sum ->
        let sum =
          if PointSet.mem (Grid.east pos) region then sum + 1 else sum
        in
        let sum =
          if PointSet.mem (Grid.south pos) region then sum + 1 else sum
        in
        sum)
      region 0
  in
  let perimeter = (region_area * 4) - (inside_borders * 2) in
  region_area * perimeter

module Part_1 = struct
  let solve input =
    let regions = find_regions (Util.parse_as_grid input) in
    List.map ~f:fence_price regions |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:140)
  let%test "sample data 2" = Test.(run int (solve sample2) ~expect:1930)
end

module Part_2 = struct
  let solve input = 0
  let%test "sample data" = Test.(run int (solve sample) ~expect:0)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
