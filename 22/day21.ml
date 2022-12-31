type monkey = Num of int | Op of string*string*(int -> int -> int);;

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let get_op (op: char) : (int -> int -> int) =
  match op with
  | '+' -> (+)
  | '-' -> (-)
  | '*' -> (fun x y -> x*y)
  | '/' -> (/)
  | _ -> raise (Failure "Invalid operator")

let gen_monkey (line: string) : string*monkey =
  let name = String.sub line 0 4 in
  if String.contains line '+' || String.contains line '-' || String.contains line '*' || String.contains line '/' then
    (name, Op(String.sub line 6 4, String.sub line 13 4, get_op (String.get line 11)))
  else
    (name, Num (int_of_string (String.sub line 6 (String.length line - 6))));;

let add_to_map (map: (string, monkey) Hashtbl.t) (name: string) (monk: monkey) : unit =
  Hashtbl.add map name monk;;

let rec evaluate_monkey (map: (string, monkey) Hashtbl.t) (monk: monkey) : int =
  match monk with
  | Num x -> x
  | Op (m1, m2, op) -> op (evaluate_monkey map (Hashtbl.find map m1)) (evaluate_monkey map (Hashtbl.find map m2));;

let main () =
  let lines = get_lines () in
  let monkeys_with_names = List.map gen_monkey lines in
  let map = Hashtbl.create 99999 in
  ignore (List.map (fun (x, y) -> add_to_map map x y) monkeys_with_names);
  let ans = evaluate_monkey map (Hashtbl.find map "root") in
  print_int ans;;

main ();;
