module CharSet = Set.Make(Char)

let get_input () = read_line ();;

let all_unique a =
  (CharSet.cardinal @@ CharSet.of_seq @@ List.to_seq @@ a) = 4;;

let rec get_chars s =
  if String.length s < 4 then 9999999 else
  let a = (String.get s 0)::(String.get s 1)::(String.get s 2)::(String.get s 3)::[] in
  if all_unique a then
    4
  else
    (get_chars @@ String.sub s 1 @@ (String.length s - 1)) + 1;;

let main () = print_int @@ get_chars @@ get_input @@ ();;

main ();;