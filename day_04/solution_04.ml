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

module Part_1 = struct
  let to_grid line =
    let chars =
      String.lines_seq line |> Seq.map String.to_seq |> Seq.map Seq.to_array
      |> Seq.to_array
    in
    let height = Array.length chars and width = Array.length chars.(0) in
    Grid.init height width (fun (i, j) -> chars.(i).(j))

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
  let solve input = 0
  let%test "sample data" = Test.(run int (solve sample) ~expect:0)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  (* Run.solve_int (module Part_2); *)
  ()
