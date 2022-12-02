open List

let rec get_til_eol () =
    let line = read_line () in
    match line with 
    |   "" -> []
    |   s -> (int_of_string s) :: get_til_eol ()

let rec get_til_eof () =
    try
        let thing = get_til_eol () in
        thing :: (get_til_eof ())
    with End_of_file ->
        [];;

let get_input () = get_til_eof ();;

let flatten = map (fold_left (+) 0);;

let get_ans x = fold_left max 0 @@ flatten @@ x;;

let main () = print_int @@ get_ans @@ get_input @@ ();;
(* let main () = print_string @@ read_line @@ ();; *)

main ();;