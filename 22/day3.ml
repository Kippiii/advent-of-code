module CharSet = Set.Make(Char)

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let get_input () = get_lines ();;

let rec first_n s n =
  match n with
  | 0 -> []
  | _ -> List.hd s :: (first_n (List.tl s) (n-1));;

let rec last_n s n =
  match n with
  | 0 -> s
  | _ -> last_n (List.tl s) (n-1);;

let split_string s =
  let len = String.length s in
  let list = List.of_seq @@ String.to_seq @@ s in
  (String.of_seq @@ List.to_seq @@ (first_n list) @@ (Int.div len 2), String.of_seq @@ List.to_seq @@ (last_n list) @@ (Int.div len 2));;

let get_unique (s1, s2) = List.hd @@ List.of_seq @@ CharSet.to_seq @@ (CharSet.inter (CharSet.of_seq @@ String.to_seq @@ s1) (CharSet.of_seq @@ String.to_seq @@ s2));;

let rank_char c = 
  let n = Char.code c in
  if n <= Char.code 'Z' then n - Char.code 'A' + 27 else n - Char.code 'a' + 1;;

let main () = print_int @@ List.fold_left (+) 0 @@ List.map rank_char @@ List.map get_unique @@ List.map split_string @@ List.map String.trim @@ get_input @@ ();;

main ();;