open ContainersLabels

let sample =
  {|
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
|}
  |> String.trim

let parse input =
  let parse_rule line = Scanf.sscanf line "%i|%i" (fun i j -> (i, j)) in
  let parse_update line =
    String.split_on_char ~by:',' line |> List.map ~f:Int.of_string_exn
  in
  let rules, updates =
    String.lines input
    |> List.take_drop_while ~f:(fun str -> not (String.is_empty str))
  in
  (List.map ~f:parse_rule rules, List.map ~f:parse_update (List.tl updates))

module IntSet = Set.Make (Int)
module IntHash = Hashtbl.Make (Int)

let build_hash_rules rules =
  let table = IntHash.create (List.length rules) in
  List.iter
    ~f:(fun (a, b) ->
      let set =
        IntHash.find_opt table b |> Option.value ~default:IntSet.empty
      in
      IntHash.replace table b (IntSet.add a set))
    rules;
  table

module Part_1 = struct
  let rec is_sorted rule_table = function
    | [] -> true
    | x :: xs ->
        let predecessors =
          IntHash.find_opt rule_table x |> Option.value ~default:IntSet.empty
        in
        let sorted_so_far =
          List.find_opt ~f:(fun i -> IntSet.mem i predecessors) xs
          |> Option.is_none
        in
        if sorted_so_far then is_sorted rule_table xs else false

  let solve input =
    let filter_rule update =
      List.filter ~f:(fun (a, b) -> List.mem a update && List.mem b update)
    in
    let rules, updates = parse input in
    let rules_table = build_hash_rules rules in
    let valid_updates = List.filter ~f:(is_sorted rules_table) updates in
    List.map ~f:(fun l -> List.nth l (List.length l / 2)) valid_updates
    |> Util.sum

  let%test "sample data" = Test.(run int (solve sample) ~expect:143)
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
