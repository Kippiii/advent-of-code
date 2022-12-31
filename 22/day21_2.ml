type monkey = Num of (int option) | Op of string*string*char;;

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

let gen_undo (op: char) (x: int option) (y: int option) : (int -> int) =
  match (x, y) with
  | (Some _, Some _) -> raise (Failure "Cannot undo double Some")
  | (Some v, None) ->
    begin
    match op with
    | '+' -> (fun i -> i - v)
    | '-' -> (fun i -> v - i)
    | '*' -> (fun i -> i / v)
    | '/' -> (fun i -> v / i)
    end
  | (None, Some v) ->
    begin
    match op with
    | '+' -> (fun i -> i - v)
    | '-' -> (fun i -> i + v)
    | '*' -> (fun i -> i / v)
    | '/' -> (fun i -> i * v)
    end
  | (None, None) -> raise (Failure "Cannot undo double None")

let gen_monkey (line: string) : string*monkey =
  let name = String.sub line 0 4 in
  if String.contains line '+' || String.contains line '-' || String.contains line '*' || String.contains line '/' then
    (name, Op(String.sub line 6 4, String.sub line 13 4, String.get line 11))
  else
    (name, Num (Some (int_of_string (String.sub line 6 (String.length line - 6)))));;

let add_to_map (map: (string, monkey) Hashtbl.t) (name: string) (monk: monkey) : unit =
  Hashtbl.add map name monk;;

let rec evaluate_monkey (map: (string, monkey) Hashtbl.t) (monk: monkey) : (int option) =
  match monk with
  | Num x -> x
  | Op (m1, m2, op) -> 
    let v1 = evaluate_monkey map (Hashtbl.find map m1) in
    let v2 = evaluate_monkey map (Hashtbl.find map m2) in
    match (v1, v2) with
    | (Some x, Some y) -> Some ((get_op op) x y)
    | _ -> None

let rec get_undo (map: (string, monkey) Hashtbl.t) (monk: monkey) : (int -> int) =
  match monk with
  | Num _ -> Fun.id
  | Op (s1, s2, opc) ->
    let v1 = evaluate_monkey map (Hashtbl.find map s1) in
    let v2 = evaluate_monkey map (Hashtbl.find map s2) in
    let f1 = gen_undo opc v1 v2 in
    let f2 = 
      match (v1, v2) with
      | (Some _, None) -> get_undo map (Hashtbl.find map s2)
      | _ -> get_undo map (Hashtbl.find map s1)
    in
    (fun i -> f2 (f1 i));;

let main () =
  let lines = get_lines () in
  let monkeys_with_names = List.map gen_monkey lines in
  let map = Hashtbl.create 99999 in
  ignore (List.map (fun (x, y) -> add_to_map map x y) monkeys_with_names);
  Hashtbl.replace map "humn" (Num None);
  let Op (m1, m2, _) = Hashtbl.find map "root" in
  let e1 = evaluate_monkey map (Hashtbl.find map m1) in
  let e2 = evaluate_monkey map (Hashtbl.find map m2) in
  let ans =
    match (e1, e2) with
    | (Some n, None) -> get_undo map (Hashtbl.find map m2) n
    | (None, Some n) -> get_undo map (Hashtbl.find map m1) n
    | _ -> raise (Failure "Double monkey")
  in
  print_int ans;;

main ();;
