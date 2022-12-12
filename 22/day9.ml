let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let split_lines lines = List.map (fun line -> (String.get line 0, int_of_string (String.sub line 2 (String.length line - 2)))) lines;;

let rec parse_split lines =
  if List.length lines = 0 then [] else
  let (c, n) = List.hd lines in
  if n = 0 then parse_split (List.tl lines) else c::(parse_split ((c, n-1)::(List.tl lines)));;

let get_moves () = parse_split @@ split_lines @@ get_lines @@ ();;

let make_move c (x, y) =
  match c with
  | 'R' -> (x+1, y)
  | 'L' -> (x-1, y)
  | 'U' -> (x, y-1)
  | 'D' -> (x, y+1);;

let need_move (x_h, y_h) (x_t, y_t) =
  if x_h = x_t || y_h = y_t then
    (abs (y_t - y_h) + abs (x_t - x_h)) > 1
  else
    (abs (y_t - y_h) + abs (x_t - x_h)) <> 2;;

let tail_move (x_h, y_h) (x_t, y_t) c =
  if x_h = x_t || y_h = y_t then (x_t, y_t) else
  match c with
  | 'R' | 'L' -> 
    let new_c = if y_t < y_h then 'D' else 'U' in
    make_move new_c (x_t, y_t)
  | _ ->
    let new_c = if x_t < x_h then 'R' else 'L' in
    make_move new_c (x_t, y_t);;


let make_move h t c =
  let new_h = make_move c h in
  if need_move new_h t then
    let new_t = make_move c t in
    let newer_t = tail_move new_h new_t c in
    (new_h, newer_t)
  else
    (new_h, t);;
  
let update_list ((h, t)::rest) c = (make_move h t c)::(h, t)::rest;;

let get_pos_arr arr = List.fold_left update_list (((0, 0), (0, 0))::[]) arr;;

let rec count_unique arr =
  match arr with
  | a::rest -> (count_unique (List.filter (fun x -> a<>x) rest)) + 1
  | [] -> 0;;

let get_ans lines =
  let all_pos = get_pos_arr lines in
  let tails_pos = List.map (fun (h, t) -> t) all_pos in
  count_unique tails_pos;;

let main () = print_int @@ get_ans @@ get_moves @@ ();;

main ();;