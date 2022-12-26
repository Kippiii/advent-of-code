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

let main () =
  let lines = get_lines () in
  let coords = List.map parse_line lines in
  let overlaps = count_adjacent coords in
  let ans = 6*(List.length coords) - 2*overlaps in
  print_int ans;;

main ();;