let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let parse_line line =
  let splitted = String.split_on_char ',' line in
  (int_of_string (List.nth splitted 0), int_of_string (List.nth splitted 1), int_of_string (List.nth splitted 2));;

let are_adjacent (x1, y1, z1) (x2, y2, z2) =
  let dist = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) in
  dist = 1;;

let rec count_adjacent l =
  match l with
  | c1 :: rest1 -> 
    let rec count l2 =
      match l2 with
      | c2 :: rest2 ->
        let to_add = if are_adjacent c1 c2 then 1 else 0 in
        to_add + count rest2
      | [] ->
        0
    in
    count rest1 + count_adjacent rest1
  | [] ->
    0

let find_bounds l =
  let x_min = List.fold_left (fun v (x, _, _) -> min v x) 99999 l in
  let x_max = List.fold_left (fun v (x, _, _) -> max v x) (-9999) l in
  let y_min = List.fold_left (fun v (_, y, _) -> min v y) 99999 l in
  let y_max = List.fold_left (fun v (_, y, _) -> max v y) (-9999) l in
  let z_min = List.fold_left (fun v (_, _, z) -> min v z) 99999 l in
  let z_max = List.fold_left (fun v (_, _, z) -> max v z) (-9999) l in
  ((x_min, x_max), (y_min, y_max), (z_min, z_max));;

let make_arr coords =
  let ((x_min, x_max), (y_min, y_max), (z_min, z_max)) = find_bounds coords in
  let new_coords = List.map (fun (x, y, z) -> (x + (2 - x_min), y + (2 - y_min), z + (2 - z_min))) coords in
  let arr = Array.init (x_max + (2 - x_min) + 3) (fun i -> Array.init (y_max + (2 - y_min) + 3) (fun j -> Array.make (z_max + (2 - z_min) + 3) '.')) in
  let _ = List.map (fun (x, y, z) -> arr.(x).(y).(z) <- '#') new_coords in
  arr;;

let floodfill array =
  let x_max = Array.length array in
  let y_max = Array.length array.(0) in
  let z_max = Array.length array.(0).(0) in
  let dirs = [(1, 0, 0); (-1, 0, 0); (0, 1, 0); (0, -1, 0); (0, 0, 1); (0, 0, -1)] in
  let colors = Array.init x_max (fun i -> Array.init y_max (fun i -> Array.make z_max (-1))) in
  let rec run_fill x y z c =
    if x < 0 || x >= x_max || y < 0 || y >= y_max || z < 0 || z >= z_max then () else
    if array.(x).(y).(z) <> '.' || colors.(x).(y).(z) <> -1 then () else
    let _ = 1 in
    colors.(x).(y).(z) <- c;
    let _ = List.map (fun (dx, dy, dz) -> run_fill (x+dx) (y+dy) (z+dz) c) dirs in
    ()
  in
  let _ = run_fill 0 0 0 0 in
  let _ = for i = 0 to (x_max - 1) do
    for j = 0 to (y_max - 1) do
      for k = 0 to (z_max - 1) do
        run_fill i j k 1
      done
    done
  done in
  colors;;

let rec get_colored colors x y z =
  let x_max = Array.length colors in
  let y_max = Array.length colors.(0) in
  let z_max = Array.length colors.(0).(0) in
  if x >= x_max then [] else
  if y >= y_max then get_colored colors (x+1) 0 z else
  if z >= z_max then get_colored colors x (y+1) 0 else
  if colors.(x).(y).(z) = 1 then (x, y, z) :: get_colored colors x y (z+1) else get_colored colors x y (z+1);;

let main () =
  let lines = get_lines () in
  let coords = List.map parse_line lines in
  let overlaps = count_adjacent coords in
  let full_sa = 6*(List.length coords) - 2*overlaps in
  let arr = make_arr coords in
  let colors = floodfill arr in
  let colored = get_colored colors 0 0 0 in
  let new_overlaps = count_adjacent colored in
  let extra_sa = (6 * List.length colored) - 2*new_overlaps in
  let ans = full_sa - extra_sa in
  print_int ans;;

main ();;