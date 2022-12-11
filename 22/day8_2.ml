exception Exception

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let rec string_to_ints s = List.map (fun x -> (Char.code x) - (Char.code '0')) @@ List.of_seq @@ String.to_seq @@ s;;

let get_input () = List.map string_to_ints @@ get_lines @@ ();;

let rec count_less_horiz arr i j n =
  if i > 0 then count_less_horiz (List.tl arr) (i-1) (j-1) n else
  if j < 0 then 0 else
  match arr with
  | (cur::rest) -> 
    if cur < n then 
      (count_less_horiz rest 0 (j-1) n) + 1
    else
      1;;

let count_left arrs i j = 
  let arr = List.nth arrs i in
  let v = List.nth arr j in
  count_less_horiz (List.rev arr) (List.length arr - j) (List.length arr - 1) v;;

let count_right arrs i j =
  let arr = List.nth arrs i in
  let v = List.nth arr j in
  count_less_horiz arr (j+1) (List.length arr - 1) v;;

let rec count_less_vert arrs i j k n =
  if i > 0 then count_less_vert (List.tl arrs) (i-1) (j-1) k n else
  if j < 0 then 0 else
  match arrs with
  | (arr::rest) -> 
    if (List.nth arr k) < n then
      (count_less_vert rest 0 (j-1) k n) + 1
    else
      1;;

let count_up arrs i j = 
  let arr = List.nth arrs i in
  let v = List.nth arr j in
  count_less_vert (List.rev arrs) (List.length arrs - i) (List.length arr - 1) j v;;

let count_down arrs i j = 
  let arr = List.nth arrs i in
  let v = List.nth arr j in
  count_less_vert arrs (i+1) (List.length arrs - 1) j v;;

let get_score arrs i j = (count_left arrs i j) * (count_right arrs i j) * (count_up arrs i j) * (count_down arrs i j);;

let rec analyze_row arrs i j =
  if j >= (List.length (List.nth arrs i)) then 0 else
  max (get_score arrs i j) (analyze_row arrs i (j+1));;

let rec analyze_col arrs i =
  if i >= (List.length arrs) then 0 else
  max (analyze_row arrs i 0) (analyze_col arrs (i+1));;

let get_ans arrs = analyze_col arrs 0;;

let main () = print_int @@ get_ans @@ get_input @@ ();;

main ();;