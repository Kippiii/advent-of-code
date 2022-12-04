exception Exception

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let parse_line s =
  let (a1::middle::b2::[]) = String.split_on_char '-' s in
  let (b1::a2::[]) = String.split_on_char ',' middle in
  let pair1 = (int_of_string a1, int_of_string b1) in
  let pair2 = (int_of_string a2, int_of_string b2) in
  (pair1, pair2);;

let get_input () = List.map parse_line @@ get_lines @@ ();;

let has_overlap ((a1, b1), (a2, b2)) = if ((a2 >= a1) && (b2 <= b1)) || ((a2 <= a1) && (b2 >= b1)) then 1 else 0;;

let main () = print_int @@ List.fold_left (+) 0 @@ List.map has_overlap @@ get_input @@ ();;

main ();;