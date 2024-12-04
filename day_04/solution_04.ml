open ContainersLabels

let sample =
  {|
....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX
|}
  |> String.trim

let sample2 =
  {|
.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........
|}
  |> String.trim

let to_grid line =
  let chars =
    String.lines_seq line |> Seq.map String.to_seq |> Seq.map Seq.to_array
    |> Seq.to_array
  in
  let height = Array.length chars and width = Array.length chars.(0) in
  Grid.init height width (fun (i, j) -> chars.(i).(j))

module Part_1 = struct
  let count_xmas grid =
    let rec find_word word position direction =
      if Grid.inside grid position then
        let current = Grid.get grid position in
        match word with
        | [] -> failwith "Impossible"
        | [ c ] when Char.equal c current -> 1
        | c :: cs when Char.equal c (Grid.get grid position) ->
            find_word cs (Grid.move direction position) direction
        | _ -> 0
      else 0
    in
    let find_xmas position current acc =
      if Char.equal current 'X' then
        List.map
          ~f:(fun direction -> (Grid.move direction position, direction))
          Grid.[ E; NE; N; NW; W; SW; S; SE ]
        |> List.map ~f:(fun (position, direction) ->
               find_word [ 'M'; 'A'; 'S' ] position direction)
        |> List.fold_left ~f:Int.add ~init:acc
      else acc
    in
    Grid.fold find_xmas grid 0

  let solve input =
    let grid = to_grid (String.trim input) in
    count_xmas grid

  let%test "sample data" = Test.(run int (solve sample) ~expect:18)
end

module Part_2 = struct
  let count_two_mas grid =
    let find_mases position current acc =
      if Char.equal current 'A' then
        let nw = Grid.north_west position in
        let ne = Grid.north_east position in
        let se = Grid.south_east position in
        let sw = Grid.south_west position in
        let is_m = Char.equal 'M' in
        let is_s = Char.equal 'S' in
        let is_whithin_bounds =
          Grid.inside grid nw && Grid.inside grid ne && Grid.inside grid se
          && Grid.inside grid sw
        in
        let down_right_mas () =
          (is_m (Grid.get grid nw) && is_s (Grid.get grid se))
          || (is_m (Grid.get grid se) && is_s (Grid.get grid nw))
        in
        let up_left_mas () =
          (is_m (Grid.get grid sw) && is_s (Grid.get grid ne))
          || (is_m (Grid.get grid ne) && is_s (Grid.get grid sw))
        in
        if is_whithin_bounds && down_right_mas () && up_left_mas () then acc + 1
        else acc
      else acc
    in
    Grid.fold find_mases grid 0

  let solve input = count_two_mas (to_grid input)
  let%test "sample data" = Test.(run int (solve sample2) ~expect:9)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
