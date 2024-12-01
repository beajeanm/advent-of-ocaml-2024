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
  module IntHash = Hashtbl.Make (Int)

  let solve input =
    let left, right = parse_input input in
    let right_counts = IntHash.create (List.length right) in
    List.iter
      ~f:(fun i ->
        let count =
          match IntHash.find_opt right_counts i with
          | None -> 1
          | Some c -> c + 1
        in
        IntHash.replace right_counts i count)
      right;
    let left_frequencies =
      List.map
        ~f:(fun l ->
          match IntHash.find_opt right_counts l with
          | None -> 0
          | Some c -> c * l)
        left
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
