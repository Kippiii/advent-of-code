exception Exception

type my_type =
  | My_list of my_type list
  | My_int of int;;

let get_pair_str () =
  let s1 = read_line () in
  let s2 = read_line () in
  [s1; s2];;

let check_if_next_pair () =
  try
    let _ = read_line () in
    true
  with End_of_file -> false

let rec get_all_pair_str () =
  let pair_str = get_pair_str () in
  if check_if_next_pair () then
    pair_str :: get_all_pair_str ()
  else
    [pair_str];;

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

let parse_pair pair = List.map parse_value pair;;

let parse_all_pairs pairs = List.map parse_pair pairs;;

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

let rec get_index_sum l n =
  match l with
  | Some true :: rest -> n + get_index_sum rest (n+1)
  | _ :: rest -> get_index_sum rest (n+1)
  | [] -> 0;;

let main () =
  let pair_strs = get_all_pair_str () in
  let pairs = parse_all_pairs pair_strs in
  let vals = List.map check_pair pairs in
  let ans = get_index_sum vals 1 in
  print_int ans;;

main ();;