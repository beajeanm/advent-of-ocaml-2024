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
    Scanf.sscanf line "%i:%s@a" (fun total numbers ->
        { total; numbers = parse_number numbers })
  in
  String.lines input |> List.map ~f:parse_line

let is_valid equation =
  let tests =
    List.fold_left
      ~init:[ List.hd equation.numbers ]
      ~f:(fun acc x ->
        List.concat [ List.map ~f:(Int.add x) acc; List.map ~f:(Int.mul x) acc ])
      (List.tl equation.numbers)
  in
  List.find_opt ~f:(Int.equal equation.total) tests |> Option.is_some

module Part_1 = struct
  let solve input =
    let equations = parse input in
    List.filter ~f:is_valid equations
    |> List.map ~f:(fun e -> e.total)
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:3749)
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
