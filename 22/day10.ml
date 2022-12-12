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

let get_ans comms =
  let funcs = comms_to_funcs comms in
  let arr = 1::(find_vals funcs 1) in
  (List.nth arr 19)*20 + (List.nth arr 59)*60 + (List.nth arr 99)*100 + (List.nth arr 139)*140 + (List.nth arr 179)*180 + (List.nth arr 219)*220;;

let main () = print_int @@ get_ans @@ get_input @@ ();;

main ();;