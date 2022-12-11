exception Exception

type file_system =
  | File of int
  | Folder of file_system list;;
type command =
  | CD of string
  | LS of string list;;

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let rec parse_group arr =
  if List.length arr = 0 then ([], []) else
  let cmd = List.hd arr in
  if String.get cmd 0 = '$' then
    ([], arr)
  else
    let (a, b) = parse_group @@ List.tl @@ arr in
    (cmd::a, b);;

let rec group_commands arr =
  if List.length arr = 0 then [] else
  let first_cmd = List.hd arr in
  match String.sub first_cmd 2 2 with
  | "ls" ->
    let (a, b) = parse_group @@ List.tl @@ arr in
    (LS a) :: group_commands b
  | "cd" ->
    let dir = String.sub first_cmd 5 (String.length first_cmd - 5) in
    (CD dir) :: (group_commands @@ List.tl @@ arr)
  | _ -> raise Exception;;

let get_commands () = group_commands @@ get_lines @@ ();;

let rec get_files arr =
  if List.length arr = 0 then [] else
  let first = String.sub (List.hd arr) 0 3 in
  if first = "dir" then
    get_files @@ List.tl @@ arr
  else
    let splitted = String.split_on_char ' ' (List.hd arr) in
    (File (int_of_string @@ List.hd @@ splitted))::(get_files @@ List.tl @@ arr);;

let rec parse_commands comms =
  if List.length comms = 0 then (Folder [], comms) else
  let next_comm = List.hd comms in
  match next_comm with
  | CD s ->
    if s = ".." then (Folder [], List.tl comms) else
    let (folder, new_comms) = parse_commands @@ List.tl @@ comms in
    let (Folder file_system, final_comms) = parse_commands new_comms in
    (Folder (folder::file_system), final_comms)
  | LS arr ->
    let files = get_files arr in
    let (Folder file_system, final_comms) = parse_commands @@ List.tl @@ comms in
    (Folder (List.append file_system files), final_comms)

let rec full_parse_commands comms =
  let (file_system, _) = parse_commands comms in
  file_system;;

let rec get_folder_size (Folder arr) =
  if List.length arr = 0 then 0 else
  let rest = get_folder_size (Folder (List.tl arr)) in
  match List.hd arr with
  | File n -> n + rest
  | f -> (get_folder_size f) + rest

let rec get_folder_sum (Folder arr) = 
  if List.length arr = 0 then 0 else
  let rest = get_folder_sum (Folder (List.tl arr)) in
  match List.hd arr with
  | File n -> rest
  | f -> 
    let folder_size = get_folder_size f in
    (if folder_size <= 100000 then folder_size else 0) + (get_folder_sum f) + rest;;

(* let main () = print_int @@ get_folder_sum @@ full_parse_commands @@ get_commands @@ ();;` *)
let main () = print_int @@ get_folder_sum @@ full_parse_commands @@ get_commands @@ ();;

main ();;