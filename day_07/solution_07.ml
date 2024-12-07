open ContainersLabels

let sample =
  {|
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
|}
  |> String.trim

type equation = { total : int; numbers : int list } [@@deriving show]

let parse input =
  let parse_number numbers =
    String.split_on_char ~by:' ' numbers
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:Int.of_string_exn
  in
  let parse_line line =
    (* @a means scan the string until the first a or the end of input. *)
    (* It's the only way to make Scanf.sscanf parse white spaces. *)
    (* @a could be replace by an character we are not expecting.  *)
    Scanf.sscanf line "%i:%s@a" (fun total numbers ->
        { total; numbers = parse_number numbers })
  in
  String.lines input |> List.map ~f:parse_line

let update_result op new_val = function
  | [] -> [ new_val ]
  | xs -> List.map ~f:(fun x -> op x new_val) xs

let is_valid ops equation =
  let rec loop acc = function
    | [] -> acc
    | hd :: tl ->
        List.map ~f:(fun op -> loop (update_result op hd acc) tl) ops
        |> List.concat
  in
  let tests = loop [] equation.numbers in

  List.find_opt ~f:(Int.equal equation.total) tests |> Option.is_some

module Part_1 = struct
  let solve input =
    let equations = parse input in
    List.filter ~f:(is_valid Int.[ add; mul ]) equations
    |> List.map ~f:(fun e -> e.total)
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:3749)
end

module Part_2 = struct
  let concatenate a b = Int.of_string_exn (Int.to_string a ^ Int.to_string b)

  let solve input =
    let equations = parse input in
    List.filter ~f:(is_valid Int.[ add; mul; concatenate ]) equations
    |> List.map ~f:(fun e -> e.total)
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:11387)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
