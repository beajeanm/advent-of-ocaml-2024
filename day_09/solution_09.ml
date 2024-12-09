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
  let id_sum idx block =
    List.(0 --^ block.length)
    |> List.map ~f:(fun i -> (i + idx) * block.id)
    |> Util.sum
  in
  List.fold_left ~init:(0, 0)
    ~f:(fun (idx, sum) block ->
      match block with
      | Space x -> (idx + x, sum)
      | File block -> (idx + block.length, sum + id_sum idx block))
    file_blocks
  |> snd

module Part_1 = struct
  let compact data_blocks =
    let rec loop acc =
      match CCDeque.(peek_front_opt data_blocks, peek_back_opt data_blocks) with
      | None, _ -> List.rev acc (* Empty deck *)
      | Some (File _ as fb), _ ->
          (* File is front is already compacted. *)
          let _ = CCDeque.take_front data_blocks in
          loop (fb :: acc)
      | _, Some (Space _) ->
          (* Space in the back is skipped *)
          let _ = CCDeque.take_back data_blocks in
          loop acc
      | Some (Space x), Some (File block as fb) ->
          (* Drop the front space and back file and figure out what to push back. *)
          let _ = CCDeque.take_front data_blocks in
          let _ = CCDeque.take_back data_blocks in

          if block.length = x then loop (fb :: acc)
          else if block.length > x then (
            CCDeque.push_back data_blocks
              (File { block with length = block.length - x });
            loop (File { block with length = x } :: acc))
          else (
            CCDeque.push_front data_blocks (Space (x - block.length));
            loop (fb :: acc))
      | _ -> failwith "Impossible"
    in
    loop []

  let solve input = parse input |> CCDeque.of_list |> compact |> check_sum
  let%test "sample data" = Test.(run int (solve sample) ~expect:1928)
end

module Part_2 = struct
  let insert data_blocks file_block =
    let blocks_list = CCDeque.to_list data_blocks in
    let idx =
      List.find_index
        ~f:(function Space x when x >= file_block.length -> true | _ -> false)
        blocks_list
    in
    match idx with
    | None -> (data_blocks, File file_block)
    | Some i ->
        let front, back = List.take_drop i blocks_list in
        let[@warning "-8"] (Space x) = List.hd back in
        let back =
          if file_block.length = x then File file_block :: List.tl back
          else File file_block :: Space (x - file_block.length) :: List.tl back
        in
        (CCDeque.of_list (List.append front back), Space file_block.length)

  let compact data_blocks =
    let rec loop deque last_id front back =
      match CCDeque.(peek_front_opt deque, peek_back_opt deque) with
      | None, _ -> List.append (List.rev front) back (* Empty deck *)
      | Some (File _ as fb), _ ->
          let _ = CCDeque.take_front deque in
          loop deque last_id (fb :: front) back
      | _, Some (Space _ as s) ->
          let _ = CCDeque.take_back deque in
          loop deque last_id front (s :: back)
      | Some (Space x), Some (File block as fb) when block.id > last_id ->
          let _ = CCDeque.take_back deque in
          loop deque last_id front (fb :: back)
      | Some (Space x), Some (File block as fb) ->
          let _ = CCDeque.take_back deque in
          let deque, push_back = insert deque block in
          loop deque block.id front (push_back :: back)
      | _ -> failwith "Impossible"
    in
    loop data_blocks Int.max_int [] []

  let solve input = parse input |> CCDeque.of_list |> compact |> check_sum
  let%test "sample data" = Test.(run int (solve sample) ~expect:2858)
end

let run_1 () =
  Run.solve_int (module Part_1);
  ()

let run_2 () =
  Run.solve_int (module Part_2);
  ()
