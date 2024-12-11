open ContainersLabels

let sample = {|
125 17
|} |> String.trim

let even_digit stone =
  Int.to_string stone |> String.length |> fun x -> x mod 2 = 0

let split stone =
  let number = Int.to_string stone in
  let len = String.length number in
  if len mod 2 = 0 then
    let left = String.sub ~pos:0 ~len:(len / 2) number |> Int.of_string_exn in
    let right =
      String.sub ~pos:(len / 2) ~len:(len / 2) number |> Int.of_string_exn
    in
    Some [ left; right ]
  else None

let update_stone stone =
  if stone = 0 then [ 1 ]
  else match split stone with Some stones -> stones | None -> [ stone * 2024 ]

let update max stones =
  let rec loop count sum stone =
    if count = max then sum + 1
    else update_stone stone |> List.fold_left ~f:(loop (count + 1)) ~init:sum
  in
  List.fold_left ~f:(loop 0) ~init:0 stones

module Part_1 = struct
  let solve input =
    String.split ~by:" " input |> List.map ~f:Int.of_string_exn |> update 25

  let%test "sample data" = Test.(run int (solve sample) ~expect:55312)
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
