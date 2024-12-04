open ContainersLabels

let sample =
  {|
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
|}
  |> String.trim

type instruction = Do | Dont | Mul of int * int [@@deriving show]

let is_digit = function '0' .. '9' -> true | _ -> false
let int = Angstrom.(take_while is_digit >>| Int.of_string_exn)

let mul_parser =
  let open Angstrom in
  let* _ = string "mul(" in
  let* a = int in
  let* _ = char ',' in
  let* b = int in
  let+ _ = char ')' in
  Mul (a, b)

let do_parser = Angstrom.(string "do()" >>| fun _ -> Do)
let dont_parser = Angstrom.(string "don't()" >>| fun _ -> Dont)
let parser = Angstrom.(mul_parser <|> do_parser <|> dont_parser)

let parse input =
  let open Result in
  let parse_aux remaining_input =
    Angstrom.parse_string ~consume:Angstrom.Consume.Prefix parser
      remaining_input
  in
  let cut_str str = String.drop 1 str in
  let rec loop acc remaining_input =
    if String.is_empty remaining_input then List.rev acc
    else
      match parse_aux remaining_input with
      | Ok instruction -> loop (instruction :: acc) (cut_str remaining_input)
      | Error _ -> loop acc (cut_str remaining_input)
  in
  loop [] input

module Part_1 = struct
  let solve input =
    let isntructions = parse input in
    List.map
      ~f:(function Do -> 0 | Dont -> 0 | Mul (a, b) -> a * b)
      isntructions
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:161)
end

module Part_2 = struct
  let solve input =
    let process_instruction (enabled, acc) = function
      | Do -> (true, acc)
      | Dont -> (false, acc)
      | Mul _ when not enabled -> (enabled, acc)
      | Mul (a, b) -> (enabled, acc + (a * b))
    in
    let instructions = parse input in
    List.fold_left ~init:(true, 0) ~f:process_instruction instructions |> snd

  let%test "sample data" = Test.(run int (solve sample) ~expect:48)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
