open ContainersLabels

let sample = {|
125 17
|} |> String.trim

module Point = Util.Point
module Store = Hashtbl.Make (Point)

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

let count_single max stone =
  let rec loop count sum stone =
    if count = max then sum + 1
    else update_stone stone |> List.fold_left ~f:(loop (count + 1)) ~init:sum
  in
  loop 0 0 stone

let build_cache () =
  let store = Store.create 75 in
  let rec count_for stone iteration =
    if iteration = 0 then 1
    else
      match Store.find_opt store (stone, iteration) with
      | Some x -> x
      | None ->
          let count =
            update_stone stone
            |> List.map ~f:(fun st -> count_for st (iteration - 1))
            |> Util.sum
          in
          Store.replace store (stone, iteration) count;
          count
  in
  List.(0 -- 9)
  |> List.iter ~f:(fun stone ->
         List.(1 -- 75)
         |> List.iter ~f:(fun count -> count_for stone count |> ignore));
  store

let count_all store max stones =
  let rec loop count sum stone =
    if count = max then sum + 1
    else
      match Store.find_opt store (stone, max - count) with
      | Some x -> sum + x
      | None ->
          update_stone stone |> List.fold_left ~f:(loop (count + 1)) ~init:sum
  in
  List.fold_left ~f:(loop 0) ~init:0 stones

module Part_1 = struct
  let solve input =
    let store = build_cache () in
    String.split ~by:" " input
    |> List.map ~f:Int.of_string_exn
    |> count_all store 25

  let%test "sample data" = Test.(run int (solve sample) ~expect:55312)
end

module Part_2 = struct
  let solve input =
    let store = build_cache () in
    String.split ~by:" " input
    |> List.map ~f:Int.of_string_exn
    |> count_all store 75

  let%test "sample data" = Test.(run int (solve sample) ~expect:65601038650482)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
