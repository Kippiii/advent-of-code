exception Exception

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let rec string_to_ints s = List.map (fun x -> (Char.code x) - (Char.code '0')) @@ List.of_seq @@ String.to_seq @@ s;;

let get_input () = List.map string_to_ints @@ get_lines @@ ();;

let rec all_less_horiz arr i j n =
  if i > 0 then all_less_horiz (List.tl arr) (i-1) (j-1) n else
  if j < 0 then true else
  match arr with
  | (cur::rest) -> if cur < n then all_less_horiz rest 0 (j-1) n else false
  | _ -> raise Exception;;

let check_left arrs i j = 
  let arr = List.nth arrs i in
  let v = List.nth arr j in
  all_less_horiz arr 0 (j-1) v;;

let check_right arrs i j =
  let arr = List.nth arrs i in
  let v = List.nth arr j in
  all_less_horiz arr (j+1) (List.length arr - 1) v;;

let rec all_less_vert arrs i j k n =
  if i > 0 then all_less_vert (List.tl arrs) (i-1) (j-1) k n else
  if j < 0 then true else
  match arrs with
  | (arr::rest) -> if (List.nth arr k) < n then all_less_vert rest 0 (j-1) k n else false;;

let check_up arrs i j = 
  let arr = List.nth arrs i in
  let v = List.nth arr j in
  all_less_vert arrs 0 (i-1) j v;;

let check_down arrs i j = 
  let arr = List.nth arrs i in
  let v = List.nth arr j in
  all_less_vert arrs (i+1) (List.length arrs - 1) j v;;

let check_point arrs i j = (||) ((||) (check_left arrs i j) (check_right arrs i j)) ((||) (check_up arrs i j) (check_down arrs i j));;

let rec count_row arrs i j =
  if j >= (List.length (List.nth arrs i)) then 0 else
  (if check_point arrs i j then 1 else 0) + (count_row arrs i (j+1));;

let rec count_col arrs i =
  if i >= (List.length arrs) then 0 else
  (count_row arrs i 0) + (count_col arrs (i+1));;

let get_ans arrs = count_col arrs 0;;

let main () = print_int @@ get_ans @@ get_input @@ ();;

main ();;