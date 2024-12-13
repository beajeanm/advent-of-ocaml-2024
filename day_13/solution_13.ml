open ContainersLabels

let sample =
  {|
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279

|}
  |> String.trim

type machine = { a : int * int; b : int * int; prize : int * int }
[@@deriving show]

let parse correction_factor input =
  let parse_machine remaining =
    let[@warning "-8"] [ a_str; b_str; prize_str ], remaining =
      List.take_drop 3 remaining
    in
    let a = Scanf.sscanf a_str "Button A: X+%i, Y+%i" Pair.make in
    let b = Scanf.sscanf b_str "Button B: X+%i, Y+%i" Pair.make in
    let prize =
      Scanf.sscanf prize_str "Prize: X=%i, Y=%i" Pair.make
      |> Pair.map_same (Int.add correction_factor)
    in
    ({ a; b; prize }, List.tail_opt remaining |> Option.value ~default:[])
  in
  let rec loop (machines, remaining) =
    if List.is_empty remaining then machines
    else
      let machine, remaining = parse_machine remaining in
      loop (machine :: machines, remaining)
  in
  loop ([], String.lines input)

let find_prize upper_bond machine =
  let ax, ay = machine.a in
  let bx, by = machine.b in
  let px, py = machine.prize in
  let det = (ax * by) - (bx * ay) in
  let a = ((px * by) - (py * bx)) / det in
  let b = ((py * ax) - (px * ay)) / det in
  let check_x = (a * ax) + (b * bx) in
  let check_y = (a * ay) + (b * by) in
  if
    check_x = px && check_y = py && a >= 0 && a <= upper_bond && b >= 0
    && b <= upper_bond
  then (3 * a) + b
  else 0

module Part_1 = struct
  let solve input = List.map ~f:(find_prize 100) (parse 1 input) |> Util.sum
  let%test "sample data" = Test.(run int (solve sample) ~expect:480)
end

module Part_2 = struct
  let solve input =
    List.map ~f:(find_prize Int.max_int) (parse 10000000000000 input)
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:0)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
