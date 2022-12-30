module Blueprint = struct
  type blueprint = {
    num: int;
    costs: int list list;
    most: int list
  }

  let gen_blueprint (line: string) : blueprint =
    let splitted = String.split_on_char ' ' line in
    let num_with_colon = List.nth splitted 1 in
    let num = int_of_string (String.sub num_with_colon 0 (String.length num_with_colon - 1)) in
    let ore_ore_cost = int_of_string (List.nth splitted 6) in
    let clay_ore_cost = int_of_string (List.nth splitted 12) in
    let obsidian_ore_cost = int_of_string (List.nth splitted 18) in
    let obsidian_clay_cost = int_of_string (List.nth splitted 21) in
    let geode_ore_cost = int_of_string (List.nth splitted 27) in
    let geode_obsidian_cost = int_of_string (List.nth splitted 30) in
    let most_ore = max (max ore_ore_cost clay_ore_cost) obsidian_ore_cost in
    let most_clay = obsidian_clay_cost in
    let most_obsidian = geode_obsidian_cost in
    let most_geode = 9999 in
    {
      num = num;
      costs = [
        [ore_ore_cost; 0; 0; 0];
        [clay_ore_cost; 0; 0; 0];
        [obsidian_ore_cost; obsidian_clay_cost; 0; 0];
        [geode_ore_cost; 0; geode_obsidian_cost; 0]
      ];
      most = [most_ore; most_clay; most_obsidian; most_geode]
    }
end;;

module State = struct
  type state = {
    bp: Blueprint.blueprint;
    time: int;
    supply: int list;
    deltas: int list
  }

  let init (bp: Blueprint.blueprint) (time: int) =
    {
      bp = bp;
      time = time;
      supply = [0; 0; 0; 0];
      deltas = [1; 0; 0; 0]
    }

  let add_time (s: state) (n: int) : state =
    let new_supply = List.map2 (fun x y -> x + y*n) s.supply s.deltas in
    let new_time = s.time - n in
    {s with time=new_time; supply=new_supply}
  
  let length_to_afford (s: state) (n: int) : int =
    let needed = List.map2 (fun x y -> max 0 (x - y)) (List.nth s.bp.costs n) s.supply in
    if List.fold_left (||) false (List.map2 (fun x y -> x = 0 && y > 0) s.deltas needed) then 99999 else
    let ceil_div x y = if x mod y = 0 then x / y else x / y + 1 in
    let time_for_resource = List.map2 (fun x y -> if x = 0 then 0 else ceil_div y x) s.deltas needed in
    List.fold_left max 0 time_for_resource

  let should_buy (s: state) (n: int) : bool =
    if List.nth s.deltas n >= List.nth s.bp.most n then false else
    if length_to_afford s n >= s.time then false else
    true
  
  let buy (s: state) (n: int) : state =
    let rec update_list (l: int list) (i: int) (diff: int) : int list =
      match l with
      | v :: rest -> if i = 0 then (v + diff) :: rest else v :: (update_list rest (i-1) diff)
      | [] -> raise (Failure "List index out of range")
    in
    let new_state = add_time s (length_to_afford s n) in
    let new_deltas = update_list new_state.deltas n 1 in
    let new_supply = List.map2 (fun x y -> x - y) new_state.supply (List.nth new_state.bp.costs n) in
    let newer_supply = update_list new_supply n (-1) in
    add_time {new_state with deltas=new_deltas; supply=newer_supply} 1
  
  let max_geodes (s: state) : int =
    let q = Queue.create () in
    let best = ref 0 in
    Queue.add s q;
    while not (Queue.is_empty q) do
      let cur_state = Queue.take q in
      if cur_state.time = 0 then best := max !best (List.nth cur_state.supply 3) else
      if cur_state.time < 0 then raise (Failure "Time got below zero") else
      let affordable = List.filter (should_buy cur_state) (List.init 4 Fun.id) in
      ignore (List.map (fun i -> Queue.add (buy cur_state i) q) affordable);
      Queue.add (add_time cur_state (cur_state.time)) q
    done;
    !best
end;;

let get_quality (bp: Blueprint.blueprint) : int =
  let num = bp.num in
  let max_geodes = State.max_geodes (State.init bp 24) in
  num * max_geodes;;

let rec get_lines () =
  try
    let line = read_line () in
    line :: get_lines ()
  with End_of_file ->
    [];;

let main () =
  let lines = get_lines () in
  let blueprints = List.map Blueprint.gen_blueprint lines in
  let qualities = List.map get_quality blueprints in
  let ans = List.fold_left (+) 0 qualities in
  print_int ans;;

main ();;