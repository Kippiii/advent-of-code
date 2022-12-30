let print_arr arr =
  for i = 0 to (Array.length arr - 1) do
    let (x, _) = arr.(i) in
    print_int x;
    print_string " "
  done;
  print_endline "";;

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let calc_index (i: int) (diff: int) (len: int) : int =
  let new_i = i + diff in
  if new_i < 0 then ((len-1) + (new_i mod (len-1))) mod (len-1) else
  new_i mod (len-1);;

let get_index (arr: (int*int) array) (n: int) : int =
  let ans = ref (-1) in
  for i = 0 to (Array.length arr - 1) do
    let (_, v) = arr.(i) in
    if !ans = (-1) && v = n then ans := i
  done;
  !ans;;

let rec get_zero_index (l: (int*int) list) : int =
  match l with
  | (x, _) :: rest -> if x = 0 then 0 else 1 + get_zero_index rest
  | [] -> raise (Failure "Could not find zero!")

let rec move_in_arr (arr: (int*int) array) (n: int) (diff: int) : unit =
  if diff = 0 then () else
  if diff < 0 then
    let tmp = arr.(n) in
    arr.(n) <- arr.(n-1);
    arr.(n-1) <- tmp;
    move_in_arr arr (n-1) (diff+1)
  else
    let tmp = arr.(n) in
    arr.(n) <- arr.(n+1);
    arr.(n+1) <- tmp;
    move_in_arr arr (n+1) (diff-1);;

let update_arr (arr: (int*int) array) ((n, m): (int*int)) : unit =
  let index = get_index arr m in
  let new_index = calc_index index n (Array.length arr) in
  let diff = (new_index - index) in
  move_in_arr arr index diff;;

let get_grove (arr: (int*int) array) : int =
  let index = get_zero_index (Array.to_list arr) in
  let len = Array.length arr in
  let (val1, _) = arr.((index + 1000) mod len) in
  let (val2, _) = arr.((index + 2000) mod len) in
  let (val3, _) = arr.((index + 3000) mod len) in
  val1 + val2 + val3;;

let main () =
  let lines = get_lines () in
  let num_list = List.map int_of_string lines in
  let num_arr = Array.map2 (fun x y -> (x, y)) (Array.of_list num_list) (Array.init (List.length lines) Fun.id) in
  ignore (List.map (update_arr num_arr) (Array.to_list num_arr));
  let ans = get_grove num_arr in
  print_int ans;;

main ();;