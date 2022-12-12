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

let tail_move (x_h, y_h) (x_t, y_t) =
  if x_h = x_t then
    if y_h < y_t then
      (x_t, y_t - 1)
    else
      (x_t, y_t + 1)
  else if y_h = y_t then
    if x_h < x_t then
      (x_t - 1, y_t)
    else
      (x_t + 1, y_t)
  else
    if x_t < x_h then
      if y_t < y_h then
        (x_t + 1, y_t + 1)
      else
        (x_t + 1, y_t - 1)
    else
      if y_t < y_h then
        (x_t - 1, y_t + 1)
      else
        (x_t - 1, y_t - 1);;

let rec update arr =
  match arr with
  | h::t::rest -> 
    let new_t = if need_move h t then tail_move h t else t in
    new_t::(update (new_t::rest))
  | _ -> [];;

let make_move (h::rest) c =
  let new_h = make_move c h in
  new_h::(update (new_h::rest));;
  
let update_list (arr::rest) c = (make_move arr c)::arr::rest;;

let get_pos_arr arr = List.fold_left update_list [List.init 10 (fun _ -> (0, 0))] arr;;

let rec count_unique arr =
  match arr with
  | a::rest -> (count_unique (List.filter (fun x -> a<>x) rest)) + 1
  | [] -> 0;;

let get_ans lines =
  let all_pos = get_pos_arr lines in
  let tails_pos = List.map (fun arr -> List.nth arr 9) all_pos in
  count_unique tails_pos;;

let main () = print_int @@ get_ans @@ get_moves @@ ();;

main ();;