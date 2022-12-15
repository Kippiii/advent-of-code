exception Exception

type my_type =
  | My_list of my_type list
  | My_int of int;;

let rec get_all_strs () =
  try
    let line = read_line () in
    if line = "" then get_all_strs () else line :: get_all_strs ()
  with End_of_file ->
    ["[[2]]"; "[[6]]"];;

let parse_my_int value_str =
  let comma_i =
    try
      String.index value_str ','
    with _ -> 9999999
  in
  let close_i =
    try
      String.index value_str ']'
    with _ -> 9999999
  in
  let i = min comma_i close_i in
  let int_str = if i = 9999999 then value_str else String.sub value_str 0 i in
  (int_of_string int_str, String.sub value_str i (String.length value_str - i));;

let rec parse_my_list value_str =
  let first_char = String.get value_str 0 in
  let rest = String.sub value_str 1 (String.length value_str - 1) in
  match first_char with
  | '[' -> 
    let (cur_list, new_rest) = parse_my_list rest in
    let (new_lists, newer_rest) = parse_my_list new_rest in
    (My_list cur_list :: new_lists, newer_rest)
  | ']' -> ([], rest)
  | ',' -> parse_my_list rest
  | _ ->
    let (cur_int, new_rest) = parse_my_int value_str in
    let (new_lists, newer_rest) = parse_my_list new_rest in
    (My_int cur_int :: new_lists, newer_rest);;

let parse_value value_str =
  if String.length value_str = 0 then raise Exception else
  if String.get value_str 0 = '[' then
    let (my_list, _) = parse_my_list (String.sub value_str 1 (String.length value_str - 1)) in
    My_list my_list
  else
    let (i, _) = parse_my_int value_str in
    My_int i;;

let parse_val_strs l = List.map parse_value l;;

let rec check_pair pair =
  match pair with
  | [My_int i; My_int j] -> if i = j then None else Some (i < j)
  | [My_list []; My_list []] -> None
  | [My_list _; My_list []] -> Some false
  | [My_list []; My_list _] -> Some true
  | [My_list (v1::rest1); My_list (v2::rest2)] -> 
      (let comp = check_pair [v1; v2] in
      match comp with
      | None -> check_pair [My_list rest1; My_list rest2]
      | _ -> comp)
  | [My_int i; My_list l] -> check_pair [My_list [My_int i]; My_list l]
  | [My_list l; My_int i] -> check_pair [My_list l; My_list [My_int i]]
  | _ -> raise Exception;;

let my_cmp v1 v2 =
  match check_pair [v1; v2] with
  | Some true -> -1
  | Some false -> 1
  | None -> 0;;

let sort_vals vals = List.sort my_cmp vals;;

let rec find_index vals v =
  match vals with
  | cur :: rest -> if cur = v then 1 else find_index rest v + 1
  | [] -> raise Exception

let main () =
  let val_strs = get_all_strs () in
  let vals = parse_val_strs val_strs in
  let sorted = sort_vals vals in
  let two_index = find_index sorted (My_list [My_list [My_int 2]]) in
  let six_index = find_index sorted (My_list [My_list [My_int 6]]) in
  print_int (two_index * six_index);;

main ();;