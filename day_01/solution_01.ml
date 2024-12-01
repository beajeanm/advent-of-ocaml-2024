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

module Part_1 = struct
  let solve input =
    let parser input =
      Scanf.sscanf (String.trim input) "%i   %i" (fun a b -> (a, b))
    in
    let first, second =
      input |> String.lines |> List.map ~f:parser |> List.split
    in
    let first = List.sort ~cmp:Int.compare first in
    let second = List.sort ~cmp:Int.compare second in
    let distances =
      List.map2 ~f:(fun one two -> abs (one - two)) first second
    in
    List.fold_left ~f:Int.add ~init:0 distances

  (* According to the description the expected value should be 7 given the
     sample data. *)
  let%test "sample data" = Test.(run int (solve sample) ~expect:11)
end

module Part_2 = struct
  let rec solve_aux accu l =
    match l with
    | x1 :: x2 :: x3 :: x4 :: tl ->
        let sliding_window_1 = Util.sum [ x1; x2; x3 ] in
        let sliding_window_2 = Util.sum [ x2; x3; x4 ] in
        let accu' =
          if sliding_window_2 > sliding_window_1 then succ accu else accu
        in
        solve_aux accu' (x2 :: x3 :: x4 :: tl)
    | _ -> accu

  let solve input =
    (input |> String.lines |> List.map ~f:int_of_string |> solve_aux 0)
    + 1 (* Remove the + 1 part to fix the test below. *)

  (* Set expect result of the sample data here: 5 *)
  let%test "sample data" = Test.(run int (solve sample) ~expect:5)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  (* When you are done, uncomment this to run the "real thing" *)
  (* Submit the result *)
  (* run `dune promote` *)
  (* Run.solve_int (module Part_2); *)
  (* Run.solve_string (module Part_2); *)
  ()
