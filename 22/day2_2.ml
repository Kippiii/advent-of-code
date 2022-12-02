exception Invalid_input;;

let rec get_til_eof () =
  try
    let line = read_line () in
    (String.get line 0, String.get line 2) :: get_til_eof ()
  with End_of_file ->
    [];;

let get_input () = get_til_eof ();;

let get_comp_score a b =
  match (a, b) with
  | ('A', 'Y') | ('B', 'Z') | ('C', 'X') -> 6
  | ('A', 'Z') | ('B', 'X') | ('C', 'Y') -> 0
  | ('A', 'X') | ('B', 'Y') | ('C', 'Z') -> 3
  | _ -> raise Invalid_input;;

let get_pick_score a =
  match a with
  | 'X' -> 1
  | 'Y' -> 2
  | 'Z' -> 3
  | _ -> raise Invalid_input;;

let new_selection a b =
  match (a, b) with
  | ('A', 'Y') | ('B', 'X') | ('C', 'Z') -> 'X'
  | ('A', 'Z') | ('B', 'Y') | ('C', 'X') -> 'Y'
  | ('A', 'X') | ('B', 'Z') | ('C', 'Y') -> 'Z'
  | _ -> raise Invalid_input;;

let get_score (a, b) = 
  let new_b = new_selection a b in
  (get_comp_score a new_b) + (get_pick_score new_b);;

let get_ans l = List.fold_left (+) 0 @@ List.map get_score @@ l;;

let main () = print_int @@ get_ans @@ get_input @@ ();;

main ();;