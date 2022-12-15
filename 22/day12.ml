exception Exception

let rec get_lines () =
  try
    let line = read_line () in
    line :: (get_lines ())
  with End_of_file ->
    [];;
  
let rec find_char arr c =
  match arr with
  | s::rest ->
    if String.contains s c then
      (0, String.index s c)
    else
      let (x, y) = find_char rest c in
      (x+1, y)
  | [] -> raise Exception;;

let get_start arr = find_char arr 'S';;

let get_end arr = find_char arr 'E';;

let parse_char c =
  match c with
  | 'S' -> 0
  | 'E' -> 25
  | _ -> Char.code c - Char.code 'a';;

let parse_line line = List.map parse_char @@ List.of_seq @@ String.to_seq @@ line;;

let parse_lines lines = List.map parse_line lines;;

let get_adj_coords (x, y) height length =
  let a1 = if x > 0 then (x-1, y)::[] else [] in
  let a2 = if x < height - 1 then (x+1, y)::a1 else a1 in
  let a3 = if y > 0 then (x, y-1)::a2 else a2 in
  let a4 = if y < length - 1 then (x, y+1)::a3 else a3 in
  a4;;

let lists_to_matrix lists =
  let arrs = List.map (fun x -> Array.of_seq @@ List.to_seq @@ x) lists in
  let matrix = Array.of_seq @@ List.to_seq @@ arrs in
  matrix;;

let create_adj matrix =
  let height = Array.length matrix in
  let length = Array.length (matrix.(0)) in
  let adj_list = Array.make (height*length) [] in
  let _ = 
    for i = 0 to height - 1 do
      for j = 0 to length - 1 do
        let adj_coords = List.filter (fun (x, y) -> matrix.(i).(j) + 1 >= matrix.(x).(y)) (get_adj_coords (i, j) height length) in
        let adj_vals = List.map (fun (x, y) -> x*length+y) adj_coords in
        adj_list.(i*length+j) <- adj_vals
      done
    done in
  adj_list;;

let bfs adj_list start ending =
  let visited = Array.make (Array.length adj_list) false in
  let q = Queue.create () in
  Queue.add (start, 0) q;
  let rec run_bfs () =
    if Queue.is_empty q then -1 else
    let (cur, length) = Queue.take q in
    if visited.(cur) then run_bfs () else
    let _ = visited.(cur) <- true in
    if cur = ending then length else
    let _ = List.map (fun x -> Queue.add (x, length+1) q) adj_list.(cur) in
    run_bfs () in
  run_bfs ();;

let main () =
  let lines = get_lines () in
  let length = String.length (List.hd lines) in
  let (sx, sy) = get_start lines in
  let (ex, ey) = get_end lines in
  let start = (sx*length) + sy in
  let ending = (ex*length) + ey in
  let matrix = lists_to_matrix @@ parse_lines @@ lines in
  let adj_list = create_adj matrix in
  let ans = bfs adj_list start ending in
  print_int ans;;

main ();;