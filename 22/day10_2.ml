exception Exception

type command =
| Noop
| Addx of int;;

let rec get_lines () =
  try
    let line = read_line () in
    line :: (get_lines ())
  with End_of_file ->
    [];;

let rec parse_command s =
  match String.sub s 0 4 with
  | "noop" -> Noop
  | "addx" -> Addx (int_of_string (String.sub s 5 (String.length s - 5)))
  | _ -> raise Exception;;

let rec parse_commands arr = List.map parse_command arr;;

let get_input () = parse_commands @@ get_lines @@ ();;

let convert_command c =
  match c with
  | Noop -> [fun x -> x]
  | Addx n -> (fun x -> x)::(fun x -> x + n)::[];;

let rec comms_to_funcs comms =
  match comms with
  | c::rest -> convert_command c @ comms_to_funcs rest
  | [] -> [];;

let rec find_vals funcs x =
  match funcs with
  | f::rest -> (f x)::(find_vals rest (f x))
  | [] -> [];;

let rec print_screen s =
  if String.length s = 0 then () else
  let _ = print_endline (String.sub s 0 40) in
  print_screen (String.sub s 40 (String.length s - 40));;

let get_s comms =
  let funcs = comms_to_funcs comms in
  let narr = 1::(find_vals funcs 1) in
  let barr = List.init 240 (fun n -> abs ((List.nth narr n) - (n mod 40)) <= 1) in
  String.of_seq @@ List.to_seq @@ List.map (fun b -> if b then '#' else '.') @@ barr;;

let main () = print_screen @@ get_s @@ get_input @@ ();;

main ();;