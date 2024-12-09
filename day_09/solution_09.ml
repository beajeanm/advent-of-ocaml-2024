open ContainersLabels

let sample = {|
2333133121414131402
|} |> String.trim

let parse input =
  let files, spaces =
    String.to_seq input
    |> Seq.map (fun c -> Char.(code c - code '0'))
    |> Seq.foldi
         (fun (files, spaces) idx n ->
           if idx mod 2 = 0 then (n :: files, spaces) else (files, n :: spaces))
         ([], [])
  in
  (List.rev files, List.rev spaces)

type file_block = { id : int; length : int }
[@@deriving show { with_path = false }]

let to_file_blocks = List.mapi ~f:(fun id length -> { id; length })

module Part_1 = struct
  let move file_blocks spaces =
    let cut_block block space =
      if block.length = space then (block, [], None)
      else if block.length > space then
        ( { block with length = space },
          [ { block with length = block.length - space } ],
          None )
      else (block, [], Some (space - block.length))
    in
    let rec loop acc = function
      | [], _ -> List.rev acc
      | [ block ], _ -> List.rev (block :: acc)
      | blocks, [] -> List.concat [ List.rev acc; blocks ]
      | blocks, s :: ss -> (
          let len = List.length blocks in
          let front, back = List.take_drop (len - 1) blocks in
          let last_block = List.hd back in
          let left_most, remaining_block, remaining_space =
            cut_block last_block s
          in
          match remaining_space with
          | None ->
              loop
                (List.hd front :: left_most :: acc)
                (List.append (List.tl front) remaining_block, ss)
          | Some x ->
              loop (left_most :: acc)
                (List.append front remaining_block, x :: ss))
    in
    loop [ List.hd file_blocks ] (List.tl file_blocks, spaces)

  let check_sum file_blocks =
    List.map
      ~f:(fun block -> List.init block.length ~f:(fun _ -> block.id))
      file_blocks
    |> List.concat
    |> List.foldi ~init:0 ~f:(fun sum idx id -> sum + (idx * id))

  let solve input =
    let files, spaces = parse input in
    let file_blocks = to_file_blocks files in
    let compacted = move file_blocks spaces in
    check_sum compacted

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
