exception Exception

let rec get_rows () =
  let line = read_line () in
  match line with
  | "" -> []
  | _ -> line :: get_rows ();;

let rec parse_row n s =
  if n = 1 then (String.get s 1)::[] else
    let c = String.get s 1 in
    let subs = String.sub s 4 (String.length s - 4) in
    c :: parse_row (n-1) subs;;

let rec add_row stacks row =
  match row with
  | c :: rest ->
    let next_stack = add_row (List.tl stacks) rest in
    let mod_stack =
      if c = ' ' then
        List.hd stacks
      else
        c :: List.hd stacks
    in
    mod_stack :: next_stack
  | [] -> [];;

let get_stacks () =
  let rows = List.rev @@ get_rows @@ () in
  let len = Int.div ((String.length @@ List.hd @@ rows) + 1) 4 in
  List.fold_left add_row (List.init len (fun _ -> [])) @@ List.map (parse_row len) @@ List.tl @@ rows;;

let rec get_rest () =
  try
    let line = read_line () in
    line :: get_rest ()
  with End_of_file ->
    [];;

let parse_inst s =
  let splitted = String.split_on_char ' ' s in
  let num = int_of_string @@ List.nth splitted 1 in
  let a = int_of_string @@ List.nth splitted 3 in
  let b = int_of_string @@ List.nth splitted 5 in
  (num, a, b);;

let get_inst () = 
  List.map parse_inst @@ get_rest @@ ()

let rec pop_stack i (cur_stack::rest_stacks) =
  if i = 0 then 
    (List.tl cur_stack)::rest_stacks
  else
    cur_stack :: pop_stack (i-1) rest_stacks;;

let rec push_stack i f (cur_stack::rest_stacks) =
  if i = 0 then
    (f::cur_stack)::rest_stacks
  else
    cur_stack :: push_stack (i-1) f rest_stacks;;

let rec move_stack (n, i, j) stacks =
  if n = 0 then stacks else
    let v = List.hd @@ List.nth stacks @@ (i-1) in
    let new_stack = push_stack (j-1) v @@ pop_stack (i-1) @@ stacks in
    move_stack (n-1, i, j) new_stack;;

let move_stack2 stacks t = move_stack t stacks;;

let mod_stack insts stacks = List.fold_left move_stack2 stacks insts;;

let get_tops stacks = String.of_seq @@ List.to_seq @@ List.map List.hd @@ stacks

let main () = 
  let stacks = get_stacks () in
  let insts = get_inst () in
  print_string @@ get_tops @@ mod_stack insts @@ stacks;;

main ();;