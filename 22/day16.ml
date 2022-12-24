module Graph = struct
  let init n = Array.make_matrix n n 99999

  let add_edge matrix (strt, other) =
    matrix.(strt).(other) <- 1

  let floyd_warshall matrix n =
    for k = 0 to (n - 1) do
      for i = 0 to (n - 1) do
        for j = 0 to (n - 1) do
          matrix.(i).(j) <- min matrix.(i).(j) (matrix.(i).(k) + matrix.(k).(j))
        done
      done
    done
  
  let print_matrix matrix n =
    for i = 0 to (n-1) do
      for j = 0 to (n-1) do
        print_int matrix.(i).(j);
        print_string " ";
      done;
      print_endline "";
    done
  
  let remove_zeros matrix flows =
    let n = Array.length matrix in
    let vals = List.init n (fun x -> if (flows.(x) <> 0) || (x = 0) then x else -1) in
    let indices = Array.of_list (List.filter (fun x -> x <> -1) vals) in
    let new_n = Array.length indices in
    let new_matrix = Array.make_matrix new_n new_n (-1) in
    let _ = List.init new_n (fun i ->
      List.init new_n (fun j -> new_matrix.(i).(j) <- matrix.(indices.(i)).(indices.(j)))  
    ) in
    let new_flows = Array.make new_n (-1) in
    let _ = List.init new_n (fun i -> new_flows.(i) <- flows.(indices.(i))) in
    (new_matrix, new_flows)
  
  let explore matrix n flows =
    let memo = Array.init n (fun _ -> Array.init 32 (fun _ -> Array.make (1 lsl n) (-1))) in
    let rec run_explore index time mask =
      if (1 lsl index) land mask <> 0 then (-999999999) else
      if memo.(index).(time).(mask) <> -1 then memo.(index).(time).(mask) else
      let new_flows = List.init n (fun x ->
        if index = x then 0 else
        if matrix.(index).(x) + 1 > time then 0 else
        (flows.(x) * (time - matrix.(index).(x) - 1)) + run_explore x (time - matrix.(index).(x) - 1) (mask lor (1 lsl index))
      ) in
      let ans = List.fold_left max 0 new_flows in
      let _ = memo.(index).(time).(mask) <- ans in
      ans
    in
    max (run_explore 0 30 0) (flows.(0)*29 + run_explore 0 29 0)
end;;

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let get_start line = String.sub line 6 2;;

let get_flow line =
  let splitted_colon = String.split_on_char ';' line in
  let splitted_equal = String.split_on_char '=' (List.hd splitted_colon) in
  int_of_string (List.nth splitted_equal 1);;

let get_ends line =
  let first :: rest = String.split_on_char ',' line in
  let new_first = String.sub first (String.length first - 2) 2 in
  new_first :: (List.map String.trim rest);;

let parse_line line = (get_start line, get_flow line, get_ends line);;

let create_map vals =
  let map = Hashtbl.create 100 in
  let val_arr = Array.of_list vals in
  let _ = List.init (Array.length val_arr) (fun x -> Hashtbl.add map val_arr.(x) x) in
  map;;

let int_of_loc map s = Hashtbl.find map s;;

let rec parse_combo map (strt, flow, ends) =
  match ends with
  | s :: rest ->
    let start_i = int_of_loc map strt in
    let si = int_of_loc map s in
    (start_i, si) :: parse_combo map (strt, flow, rest)
  | [] -> [];;

let create_adj_matrix edge_pairs n =
  let matrix = Graph.init n in
  let _ = List.map (fun pair -> Graph.add_edge matrix pair) edge_pairs in
  matrix;;

let main () =
  let lines = get_lines () in
  let combos = List.map parse_line lines in
  let map = create_map (List.map (fun (x, _, _) -> x) combos) in
  let flows = Array.of_list (List.map (fun (_, x, _) -> x) combos) in
  let n = (Array.length flows) in
  let edges = List.flatten (List.map (parse_combo map) combos) in
  let matrix = create_adj_matrix edges n in
  let _ = Graph.floyd_warshall matrix n in
  let new_matrix, new_flows = Graph.remove_zeros matrix flows in
  let ans = Graph.explore new_matrix (Array.length new_matrix) new_flows in
  print_int ans;;

main ();;