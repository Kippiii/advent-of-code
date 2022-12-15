exception Exception

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let rec get_point_strs line =
  let dash_i = 
    try
      String.index line '-'
    with _ -> -1
  in
  match dash_i with
  | -1 -> [line]
  | _ ->
    let cur_point_str = String.sub line 0 (dash_i-1) in
    let rest_line = String.sub line (dash_i+3) (String.length line - dash_i - 3) in
    cur_point_str :: get_point_strs rest_line;;

let parse_point point_str =
  let comma_i = String.index point_str ',' in
  let x_str = String.sub point_str 0 comma_i in
  let y_str = String.sub point_str (comma_i+1) (String.length point_str - comma_i - 1) in
  (int_of_string x_str, int_of_string y_str);;

let get_point_lists () = 
  let lines = get_lines () in
  let point_strs = List.map get_point_strs lines in
  List.map (fun x -> List.map parse_point x) point_strs;;

let rec find_x_bounds point_lists =
  match point_lists with
  | point_list :: rest_lists ->
    (match point_list with
      | (x, _) :: rest -> 
        let (lower, upper) = find_x_bounds (rest :: rest_lists) in
        (min x lower, max x upper)
      | [] -> find_x_bounds rest_lists)
  | [] -> (999999, -99999999);;

let rec find_y_bounds point_lists =
  let xy_swapped = List.map (fun x -> List.map (fun (i, j) -> (j, i)) x) point_lists in
  find_x_bounds xy_swapped;;

let rec get_point_pairs point_list =
  match point_list with
  | p1 :: p2 :: rest -> (p1, p2) :: (get_point_pairs (p2 :: rest))
  | p :: rest -> []
  | [] -> raise Exception;;

let get_all_point_pairs point_lists = List.flatten (List.map get_point_pairs point_lists);;

(* Side effects! :( *)
let add_line matrix ((x1, y1), (x2, y2)) =
  if x1 = x2 then
    let y_min = min y1 y2 in
    let y_max = max y1 y2 in
    for i = y_min to y_max do
      matrix.(x1).(i) <- '#'
    done
  else
    let x_min = min x1 x2 in
    let x_max = max x1 x2 in
    for i = x_min to x_max do
      matrix.(i).(y1) <- '#'
    done;;

(* Side effects! :( *)
let add_all_lines matrix lines = List.map (add_line matrix) lines;;

(* Side effects! :( *)
let rec sand_fall matrix (x, y) =
  if matrix.(x).(y+1) = '.' then
    sand_fall matrix (x, y+1)
  else if matrix.(x-1).(y+1) = '.' then
    sand_fall matrix (x-1, y+1)
  else if matrix.(x+1).(y+1) = '.' then
    sand_fall matrix (x+1, y+1)
  else
    let _ = matrix.(x).(y) <- 'O' in
    (x, y);;

(* Side effects! :( *)
let rec get_num_sand matrix start =
  let run_sand = sand_fall matrix start in
  if run_sand = start then 1 else get_num_sand matrix start + 1;;

let main () =
  let point_lists = get_point_lists () in
  let (x_low, x_high) = find_x_bounds point_lists in
  let (y_low, y_high) = find_y_bounds point_lists in
  if x_low < 0 then raise Exception else
  if y_low < 0 then raise Exception else
  let matrix = Array.make_matrix (x_high + 200) (y_high + 10) '.' in
  let point_pairs = get_all_point_pairs point_lists in
  let _ = add_all_lines matrix point_pairs in
  let _ = add_line matrix ((0, y_high+2), (x_high+199, y_high+2)) in
  let ans = get_num_sand matrix (500, 0) in
  print_int ans;;

main ();;