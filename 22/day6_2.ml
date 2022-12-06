module CharSet = Set.Make(Char)

let get_input () = read_line ();;

let all_unique a =
  (CharSet.cardinal @@ CharSet.of_seq @@ List.to_seq @@ a) = 14;;

let rec get_chars s =
  if String.length s < 14 then 9999999 else
  let a = List.of_seq @@ String.to_seq @@ String.sub s 0 @@ 14 in
  if all_unique a then
    14
  else
    (get_chars @@ String.sub s 1 @@ (String.length s - 1)) + 1;;

let main () = print_int @@ get_chars @@ get_input @@ ();;

main ();;