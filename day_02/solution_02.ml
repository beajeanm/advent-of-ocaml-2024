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

let is_gradual a b =
  let diff = abs (a - b) in
  diff >= 1 && diff <= 3

let is_safe_aux (previous, cmp, _) (current : int) =
  if is_gradual previous current && cmp previous current then
    ((current, cmp, true), `Continue)
  else ((current, cmp, false), `Stop)

let is_safe = function
  | [] | [ _ ] -> true
  | first :: second :: tl ->
      if is_gradual first second then
        let init =
          if first > second then (second, (fun a b -> a > b), true)
          else (second, (fun a b -> a < b), true)
        in
        let _, _, safeness = List.fold_while ~f:is_safe_aux ~init tl in
        safeness
      else false

module Part_1 = struct
  let solve input =
    let parsed = parse input in
    List.filter ~f:is_safe parsed |> List.length

  let%test "sample data" = Test.(run int (solve sample) ~expect:2)
end

module Part_2 = struct
  let ints_pp =
    List.pp
      ~pp_start:(fun fmt () -> Format.fprintf fmt "[")
      ~pp_stop:(fun fmt () -> Format.fprintf fmt "]")
      Int.pp

  let remove_level levels index =
    let first, second =
      List.foldi ~init:([], [])
        ~f:(fun (first, second) i a ->
          if i = index - 1 then (first, a :: second)
          else if i = index then (a :: first, second)
          else (a :: first, a :: second))
        levels
    in
    (List.rev first, List.rev second)

  let is_safe_aux (previous, cmp, index, _) (current : int) =
    if is_gradual previous current && cmp previous current then
      ((current, cmp, index + 1, true), `Continue)
    else ((current, cmp, index, false), `Stop)

  (* Brute forcing my way to success... *)
  (* With the dampener, a report is safe if any of it's sub list of levels is a safe report. *)
  (* Rather than trying to be cleaver, just generate all the sub lists and check them. *)
  (* We don't need to check the initial report since all sub lists of a valid report will be safe. *)
  let is_safe_with_dampener levels =
    let remove_last = function
      | [] -> []
      | [ a ] -> []
      | xs -> List.(rev (tl (rev xs)))
    in

    let remove_i i xs =
      let front, back = List.take_drop i xs in
      List.concat [ remove_last front; back ]
    in

    let sub_lists =
      List.map
        ~f:(fun i -> remove_i i levels)
        (List.range 0 (List.length levels))
    in
    List.fold_left ~init:false
      ~f:(fun state xs -> state || is_safe xs)
      sub_lists

  let solve input =
    let parsed = parse input in
    List.filter ~f:is_safe_with_dampener parsed |> List.length

  let%test "sample data" = Test.(run int (solve sample) ~expect:4)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
