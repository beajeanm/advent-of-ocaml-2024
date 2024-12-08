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

let find_antinodes grid steps exclude_antennas antennas =
  let aux (x1, y1) (x2, y2) =
    let x_diff = x1 - x2 in
    let y_diff = y1 - y2 in
    List.(1 -- steps)
    |> List.flat_map ~f:(fun step ->
           [
             (x1 - (x_diff * step), y1 - (y_diff * step));
             (x1 + (x_diff * step), y1 + (y_diff * step));
             (x2 - (x_diff * step), y2 - (y_diff * step));
             (x2 + (x_diff * step), y2 + (y_diff * step));
           ])
    |> List.filter ~f:(Grid.inside grid)
    |> List.filter ~f:(fun p ->
           Point.(
             (not exclude_antennas) || not (equal p (x1, y1) || equal p (x2, y2))))
  in
  let rec loop acc = function
    | [] | [ _ ] -> acc
    | p :: ps ->
        let acc =
          PointSet.add_list acc (List.map ~f:(aux p) ps |> List.concat)
        in
        loop acc ps
  in
  loop PointSet.empty antennas

module Part_1 = struct
  let solve input =
    let grid = Util.parse_as_grid input in
    let antennas = find_antennas grid in
    let antennas_list = CharMap.to_seq_values antennas in
    Seq.map (find_antinodes grid 1 true) antennas_list
    |> Seq.fold_left PointSet.union PointSet.empty
    |> PointSet.to_seq |> Seq.length

  let%test "sample data" = Test.(run int (solve sample) ~expect:14)
end

module Part_2 = struct
  let solve input =
    let grid = Util.parse_as_grid input in
    let antennas = find_antennas grid in
    let antennas_list = CharMap.to_seq_values antennas in
    let antinodes =
      Seq.map (find_antinodes grid 50 false) antennas_list
      |> Seq.fold_left PointSet.union PointSet.empty
      |> PointSet.to_seq |> Seq.length
    in
    antinodes

  let%test "sample data" = Test.(run int (solve sample) ~expect:34)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
