exception Exception

let rec get_til_blank () =
  let line = read_line () in
  if line = "" then [] else line :: (get_til_blank ());;

let rec get_til_eof () =
  try
    let monk_str = get_til_blank () in
    monk_str :: (get_til_eof ())
  with End_of_file ->
    [];;

type monkey = Monkey of int * (int list) * (int -> int) * (int -> int);;

let parse_monkey_num s = 
  let ending = String.sub s 7 (String.length s - 7) in
  let num_str = String.sub ending 0 (String.length ending - 1) in
  int_of_string num_str;;

let parse_monkey_list s =
  let ending = String.sub s 17 (String.length s - 17) in
  let splitted = String.split_on_char ',' ending in
  let mod_splitted = List.map String.trim splitted in
  List.map int_of_string mod_splitted;;

let parse_monkey_op s =
  let op_chr = String.get s 23 in
  let num_str = String.sub s 25 (String.length s - 25) in
  match (op_chr, num_str) with
  | ('+', "old") -> (fun x -> x + x)
  | ('+', ns) -> (fun x -> x + (int_of_string ns))
  | ('*', "old") -> (fun x -> x * x)
  | ('*', ns) -> (fun x -> x * (int_of_string ns))
  | _ -> raise Exception;;

let parse_monkey_check arr =
  let s1 = List.nth arr 0 in
  let s2 = List.nth arr 1 in
  let s3 = List.nth arr 2 in
  let divis = int_of_string (String.sub s1 21 (String.length s1 - 21)) in
  let index1 = int_of_string (String.sub s2 29 (String.length s2 - 29)) in
  let index2 = int_of_string (String.sub s3 30 (String.length s3 - 30)) in
  let check = fun x -> x mod divis = 0 in
  fun x -> if check x then index1 else index2;;

let parse_monkey arr =
  let num = parse_monkey_num (List.nth arr 0) in
  let l = parse_monkey_list (List.nth arr 1) in
  let op = parse_monkey_op (List.nth arr 2) in
  let check = parse_monkey_check (List.tl @@ List.tl @@ List.tl @@ arr) in
  Monkey (num, l, op, check);;

let get_input () = List.map parse_monkey (get_til_eof ());;

let monkey_deal_with_top (Monkey (n, objs, u, f)) =
  match objs with
  | top::rest -> 
    let updated = (u top) / 3 in
    (updated, f updated)
  | [] -> raise Exception;;

let rec monkey_do (Monkey (n, arr, u, f)) =
  match arr with
  | v::rest -> (monkey_deal_with_top (Monkey (n, arr, u, f)))::(monkey_do (Monkey (n, rest, u, f)))
  | [] -> [];;

let rec update_monkeys monkeys (obj, i) =
  match monkeys with
  | (Monkey (n, arr, u, f))::rest ->
    if n = i then 
      (Monkey (n, (obj::arr), u, f))::rest 
    else 
      (Monkey (n, arr, u, f))::(update_monkeys rest (obj, i))
  | [] -> raise Exception;;

let run_monkey ((Monkey (n, arr, u, f))::rest) =
  let results = monkey_do (Monkey (n, arr, u, f)) in
  let new_rest = List.fold_left update_monkeys rest results in
  new_rest @ [Monkey (n, [], u, f)];;

let rec run_all monkeys n =
  if n = 0 then [] else
  match monkeys with
  | (Monkey (i, arr, u, f))::rest ->
    let updated = run_monkey monkeys in
    (i, List.length arr)::(run_all updated (n-1));;
  
let rec update_vals vals (i, n) =
  match vals with
  | v::rest -> 
      if i = 0 then
        (v+n)::rest
      else
        v::(update_vals rest (i-1, n))
  | [] -> raise Exception;;

let get_num_throws monkeys =
  let vals = run_all monkeys ((List.length monkeys)*20) in
  let val_list = List.fold_left update_vals (List.init (List.length monkeys) (fun _ -> 0)) vals in
  val_list;;

let get_ans monkeys =
  let nums = get_num_throws monkeys in
  let s = List.rev (List.sort (-) nums) in
  (List.nth s 0) * (List.nth s 1);;

let main () = print_int @@ get_ans @@ get_input @@ ();;

main ();;
