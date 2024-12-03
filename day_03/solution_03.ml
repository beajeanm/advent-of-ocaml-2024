open ContainersLabels

let sample =
  {|
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
|}
  |> String.trim

let is_digit = function '0' .. '9' -> true | _ -> false
let int = Angstrom.(take_while is_digit >>| Int.of_string_exn)
let mul = Angstrom.(string "mul(" *> both int (char ',' *> int) <* char ')')

let parse input =
  let open Result in
  let parse_aux remaining_input =
    Angstrom.parse_string ~consume:Angstrom.Consume.Prefix mul remaining_input
  in
  let cut_to_next_mul str =
    let len = String.length str in
    let next_m = String.find ~start:1 ~sub:"mul(" str in
    if next_m = -1 then "" else String.sub ~pos:next_m ~len:(len - next_m) str
  in
  let rec loop acc remaining_input =
    if String.is_empty remaining_input then List.rev acc
    else
      match parse_aux remaining_input with
      | Ok tuple -> loop (tuple :: acc) (cut_to_next_mul remaining_input)
      | Error _ -> loop acc (cut_to_next_mul remaining_input)
  in
  loop [] input

module Part_1 = struct
  let solve input =
    let tuples = parse input in
    List.map ~f:(fun (a, b) -> a * b) tuples |> Util.sum_list

  let%test "sample data" = Test.(run int (solve sample) ~expect:161)
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
