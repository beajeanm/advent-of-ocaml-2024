open ContainersLabels

let sample = {|
2333133121414131402
|} |> String.trim

type file_block = { id : int; length : int }
[@@deriving show { with_path = false }]

type data = File of file_block | Space of int
[@@deriving show { with_path = false }]

let parse input =
  String.to_seq input
  |> Seq.map (fun c -> Char.(code c - code '0'))
  |> Seq.foldi
       (fun data idx n ->
         if idx mod 2 = 0 then File { id = idx / 2; length = n } :: data
         else Space n :: data)
       []
  |> List.rev

let check_sum file_blocks =
  List.map
    ~f:(function
      | File block -> List.init block.length ~f:(fun _ -> block.id)
      | Space _ -> [])
    file_blocks
  |> List.concat
  |> List.foldi ~init:0 ~f:(fun sum idx id -> sum + (idx * id))

module Part_1 = struct
  let compact (data_blocks : data CCDeque.t) =
    let rec loop acc =
      match CCDeque.take_front_opt data_blocks with
      | Some (File _ as fb) -> loop (fb :: acc)
      | Some (Space x as s) -> (
          match CCDeque.take_back_opt data_blocks with
          | Some (Space _) ->
              CCDeque.push_front data_blocks s;
              loop acc
          | Some (File block as fb) ->
              if block.length = x then loop (fb :: acc)
              else if block.length > x then (
                CCDeque.push_back data_blocks
                  (File { block with length = block.length - x });
                loop (File { block with length = x } :: acc))
              else (
                CCDeque.push_front data_blocks (Space (x - block.length));
                loop (fb :: acc))
          | None -> List.rev acc)
      | None -> List.rev acc
    in
    loop []

  let solve input = parse input |> CCDeque.of_list |> compact |> check_sum
  let%test "sample data" = Test.(run int (solve sample) ~expect:1928)
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
