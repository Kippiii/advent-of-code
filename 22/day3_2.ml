module CharSet = Set.Make(Char)

exception Exception

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let get_input () = get_lines ();;

let rec group l =
  match l with
  | s1::s2::s3::rest -> (s1, s2, s3)::(group rest)
  | [] -> []
  | _ -> raise Exception

let get_unique (s1, s2, s3) = List.hd @@ List.of_seq @@ CharSet.to_seq @@ (CharSet.inter (CharSet.inter (CharSet.of_seq @@ String.to_seq @@ s1) (CharSet.of_seq @@ String.to_seq @@ s2)) (CharSet.of_seq @@ String.to_seq @@ s3));;

let rank_char c = 
  let n = Char.code c in
  if n <= Char.code 'Z' then n - Char.code 'A' + 27 else n - Char.code 'a' + 1;;

let main () = print_int @@ List.fold_left (+) 0 @@ List.map rank_char @@ List.map get_unique @@ group @@ List.map String.trim @@ get_input @@ ();;

main ();;