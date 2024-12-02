open ContainersLabels

let sample =
  {|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|}
  |> String.trim

let parse input =
  String.lines input
  |> List.map ~f:(String.split_on_char ~by:' ')
  |> List.map ~f:(List.map ~f:Int.of_string_exn)

module Part_1 = struct
  let is_gradual a b =
    let diff = abs (a - b) in
    diff >= 1 && diff <= 3

  let is_safe_aux (state : int * [ `Decreasing | `Increasing ] * bool)
      (current : int) =
    match state with
    | previous, `Increasing, _ ->
        if is_gradual previous current && previous < current then
          ((current, `Increasing, true), `Continue)
        else ((current, `Increasing, false), `Stop)
    | previous, `Decreasing, _ ->
        if is_gradual previous current && previous > current then
          ((current, `Decreasing, true), `Continue)
        else ((current, `Decreasing, false), `Stop)

  let is_safe = function
    | [] | [ _ ] -> true
    | first :: second :: tl ->
        if is_gradual first second then
          let init =
            if first > second then (second, `Decreasing, true)
            else (second, `Increasing, true)
          in
          let _, _, safeness = List.fold_while ~f:is_safe_aux ~init tl in
          safeness
        else false

  let solve input =
    let parsed = parse input in
    List.filter ~f:is_safe parsed |> List.length

  let%test "sample data" = Test.(run int (solve sample) ~expect:2)
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
