open ContainersLabels

(*
let sample =
  {|
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
|}
  |> String.trim
  *)

let sample =
  {|
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
|}
  |> String.trim

module Point = Util.Point
module PointSet = Set.Make (Point)
module CharMap = Hashtbl.Make (Char)

let find_antennas grid =
  let add_antenna map pos = function
    | '.' -> ()
    | c ->
        let positions = CharMap.find_opt map c |> Option.value ~default:[] in
        CharMap.replace map c (pos :: positions)
  in
  let antennas = CharMap.create (Grid.height grid) in
  Grid.iter (add_antenna antennas) grid;
  antennas

module Part_1 = struct
  let find_antinodes grid antennas =
    let aux (x1, y1) (x2, y2) =
      let x_diff = x1 - x2 in
      let y_diff = y1 - y2 in
      [
        (x1 - x_diff, y1 - y_diff);
        (x1 + x_diff, y1 + y_diff);
        (x2 - x_diff, y2 - y_diff);
        (x2 + x_diff, y2 + y_diff);
      ]
      |> List.filter ~f:(Grid.inside grid)
      |> List.filter ~f:(fun p ->
             Point.(not (equal p (x1, y1) || equal p (x2, y2))))
    in
    let rec loop acc = function
      | [] | [ _ ] -> acc
      | p :: ps ->
          let acc =
            PointSet.add_list acc (List.map ~f:(aux p) ps |> List.concat)
          in
          loop acc ps
    in
    let results = loop PointSet.empty antennas in
    let display_grid = Grid.copy grid in
    PointSet.iter (fun p -> Grid.set display_grid p '#') results;
    results

  let solve input =
    let grid = Util.parse_as_grid input in
    let antennas = find_antennas grid in
    let antennas_list = CharMap.to_seq_values antennas in
    Seq.map (find_antinodes grid) antennas_list
    |> Seq.fold_left PointSet.union PointSet.empty
    |> PointSet.to_seq |> Seq.length

  let%test "sample data" = Test.(run int (solve sample) ~expect:14)
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
