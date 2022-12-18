module Ranges = struct
  let empty = []

  let rec consolidate l =
    match l with
    | (l1, u1) :: (l2, u2) :: rest ->
      if l2 <= u1 then
        consolidate ((min l1 l2, max u2 u1) :: rest)
      else
        (l1, u1) :: consolidate ((l2, u2) :: rest)
    | _ -> l
  
  let rec basic_insert li (l, u) =
    match li with
    | (cl, cu) :: rest ->
      if u < cl then
        (l, u) :: (cl, cu) :: rest
      else if l > cu then
        (cl, cu) :: basic_insert rest (l, u)
      else
        (min cl l, max cu u) :: rest
    | [] -> [(l, u)]

  let insert li (l, u) = 
    if (l, u) = (-1, -2) then li else
    consolidate (basic_insert li (l, u))

  let rec get_length li =
    match li with
    | (l, u) :: rest -> (u - l) + get_length rest
    | [] -> 0

  let rec not_in_range li (l, u) =
    if u < l then -1 else
    match li with
    | (cl, cu) :: rest ->
      if l < cl then
        l
      else if cu < l then
        not_in_range rest (l, u)
      else
        not_in_range rest (cu+1, u)
    | [] -> l
end;;

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let get_sensor_loc s =
  let comma_split = String.split_on_char ',' s in
  let first_equal_split = String.split_on_char '=' (List.hd comma_split) in
  let x = int_of_string (List.nth first_equal_split 1) in
  let second_equal_split = String.split_on_char '=' (List.nth comma_split 1) in
  let colon_split = String.split_on_char ':' (List.nth second_equal_split 1) in
  let y = int_of_string (List.hd colon_split) in
  (x, y);;

let get_close_beacon_loc s =
  let comma_split = String.split_on_char ',' s in
  let first_equal_split = String.split_on_char '=' (List.nth comma_split 1) in
  let x = int_of_string (List.nth first_equal_split 2) in
  let second_equal_split = String.split_on_char '=' (List.nth comma_split 2) in
  let y = int_of_string (List.nth second_equal_split 1) in
  (x, y);;

let get_locs s = (get_sensor_loc s, get_close_beacon_loc s);;

let get_input () = List.map get_locs (get_lines ());;

let man_dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2);;

let get_range y_coord ((sx, sy), (bx, by)) =
  let r = man_dist (sx, sy) (bx, by) in
  let y_dist = abs (y_coord - sy) in
  if y_dist > r then (-1, -2) else
  (sx - (r - y_dist), sx + (r - y_dist));;

let main () =
  let locs = get_input () in
  for y = 0 to 4000000 do
    let range = List.fold_left Ranges.insert Ranges.empty (List.map (get_range y) locs) in
    let ans = Ranges.not_in_range range (0, 4000000) in
    if ans <> -1 then
      print_int (ans*4000000 + y)
    else
      ()
  done;;

main ();;