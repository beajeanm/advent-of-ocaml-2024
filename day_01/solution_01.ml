open ContainersLabels
(* open Containers *)

(* Advent of Code usually provides of with some smaller examples.
   I usually do inline testing to verify that my solution matches the examples.

    OCaml supports multi-line strings by default. *)

(* Paste sample data here *)
let sample = {|
3   4
4   3
2   5
1   3
3   9
3   3
|} |> String.trim

let parse_input input =
  let input = String.trim input in
  let parser input = Scanf.sscanf input "%i   %i" (fun a b -> (a, b)) in
  input |> String.lines |> List.map ~f:parser |> List.split

module Part_1 = struct
  let solve input =
    let left, right = parse_input input in
    let left = List.sort ~cmp:Int.compare left in
    let right = List.sort ~cmp:Int.compare right in
    let distances = List.map2 ~f:(fun one two -> abs (one - two)) left right in
    Util.sum_list distances

  let%test "sample data" = Test.(run int (solve sample) ~expect:11)
end

module Part_2 = struct
  let solve input =
    let left, right = parse_input input in
    (* Nice O(n2) 😬 *)
    let left_frequencies =
      List.map ~f:(fun l -> l * List.count ~f:(Int.equal l) right) left
    in
    Util.sum_list left_frequencies

  let%test "sample data" = Test.(run int (solve sample) ~expect:31)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
