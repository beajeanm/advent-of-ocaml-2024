open ContainersLabels

let file_is_not_empty path =
  match Aoc.Util.read_file_as_string path with
  | "" -> false
  | _ -> true
  | exception Sys_error _ -> false

let result_for_day day =
  let path i = Printf.sprintf "day_%02d/p%d.expected" day i in
  let result =
    match (file_is_not_empty (path 1), file_is_not_empty (path 2)) with
    | true, false -> "✅|❌"
    | true, true -> "✅|✅"
    | _ -> "❌|❌"
  in
  Printf.sprintf "|%i|%s|" day result

let () =
  print_endline "|Day|Part 1|Part2|";
  print_endline "|---|---|---|";
  List.(1 -- 25 >|= result_for_day) |> String.concat ~sep:"\n" |> print_endline
