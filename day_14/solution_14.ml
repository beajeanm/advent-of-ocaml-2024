open ContainersLabels

let sample =
  {|
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
|}
  |> String.trim

type robot = { pos : int * int; vel : int * int }

module PointSet = Set.Make (Util.Point)

let parse input =
  let parse_robot line =
    Scanf.sscanf line "p=%i,%i v=%i,%i" (fun a b c d ->
        { pos = (a, b); vel = (c, d) })
  in
  String.lines input |> List.map ~f:parse_robot

let ( %% ) a b =
  if a > 0 then a mod b
  else
    let rem, op = float_of_int a /. float_of_int b |> Stdlib.modf in
    let div =
      if Float.Infix.(rem < 0.0) then int_of_float op - 1 else int_of_float op
    in
    a - (b * div)

let move seconds width height robot =
  let x, y = robot.pos in
  let dx, dy = robot.vel in
  ((x + (seconds * dx)) %% width, (y + (seconds * dy)) %% height)

type group = TOP_LEFT | TOP_RIGHT | BOTTOM_LEFT | BOTTOM_RIGHT | MID
[@@deriving show { with_path = false }]

let safety_factor width height positions =
  let mid_width = width / 2 in
  let mid_height = height / 2 in
  let classify (x, y) =
    if x = mid_width || y = mid_height then MID
    else
      match (x < mid_width, y < mid_height) with
      | true, true -> TOP_LEFT
      | true, false -> BOTTOM_LEFT
      | false, true -> TOP_RIGHT
      | false, false -> BOTTOM_RIGHT
  in
  let classify pos = classify pos in
  let aux (tl, tr, bl, br) pos =
    match classify pos with
    | TOP_LEFT -> (tl + 1, tr, bl, br)
    | TOP_RIGHT -> (tl, tr + 1, bl, br)
    | BOTTOM_LEFT -> (tl, tr, bl + 1, br)
    | BOTTOM_RIGHT -> (tl, tr, bl, br + 1)
    | MID -> (tl, tr, bl, br)
  in
  let a, b, c, d = List.fold_left ~init:(0, 0, 0, 0) ~f:aux positions in
  a * b * c * d

module Part_1 = struct
  let solve_aux width height input =
    let robots = parse input in
    List.map ~f:(fun robot -> move 100 width height robot) robots
    |> safety_factor width height

  let solve = solve_aux 101 103
  let%test "sample data" = Test.(run int (solve_aux 11 7 sample) ~expect:12)
end

module Part_2 = struct
  let solve_unique width height robots =
    let rec loop count =
      let update_pos = List.map ~f:(move count width height) robots in
      let unique_positions = PointSet.cardinal (PointSet.of_list update_pos) in
      if unique_positions = List.length robots then count else loop (count + 1)
    in
    loop 1

  let solve input = parse input |> solve_unique 101 103
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
