module Grid = struct
  let init n stream = 
    let grid = Array.make_matrix n 7 '.' in
    let _ = List.init 7 (fun x -> grid.(0).(x) <- '#') in
    (grid, Array.of_seq (String.to_seq stream), 0, 0, 0, [])

  let get_grid (x, _, _, _, _, _) = x
  let get_stream (_, x, _, _, _, _) = x
  let get_turn_num (_, _, x, _, _, _) = x
  let get_time (_, _, _, x, _, _) = x
  let get_top (_, _, _, _, x, _) = x
  let get_falling (_, _, _, _, _, x) = x

  let set_grid (_, a, b, c, d, e) x = (x, a, b, c, d, e)
  let set_stream (a, _, b, c, d, e) x = (a, x, b, c, d, e)
  let set_turn_num (a, b, _, c, d, e) x = (a, b, x, c, d, e)
  let set_time (a, b, c, _, d, e) x = (a, b, c, x, d, e)
  let set_top (a, b, c, d, _, e) x = (a, b, c, d, x, e)
  let set_falling (a, b, c, d, e, _) x = (a, b, c, d, e, x)

  let get_minus_falling top = [(top + 4, 2); (top + 4, 3); (top + 4, 4); (top + 4, 5)]
  let get_plus_falling top = [(top + 4, 3); (top + 5, 2); (top + 5, 3); (top + 5, 4); (top + 6, 3)]
  let get_angle_falling top = [(top + 4, 2); (top + 4, 3); (top + 4, 4); (top + 5, 4); (top + 6, 4)]
  let get_pipe_falling top = [(top + 4, 2); (top + 5, 2); (top + 6, 2); (top + 7, 2)]
  let get_square_falling top = [(top + 4, 2); (top + 4, 3); (top + 5, 2); (top + 5, 3)]
  let falling_map = [get_minus_falling; get_plus_falling; get_angle_falling; get_pipe_falling; get_square_falling]
  
  let move_left play =
    let grid = get_grid play in
    let falling = get_falling play in
    let can_move_list = List.map (fun (x, y) -> y > 0 && grid.(x).(y-1) = '.') falling in
    let can_move = List.fold_left (&&) true can_move_list in
    if can_move then List.map (fun (x, y) -> (x, y-1)) falling else falling
  
  let move_right play =
    let grid = get_grid play in
    let falling = get_falling play in
    let can_move_list = List.map (fun (x, y) -> y < 6 && grid.(x).(y+1) = '.') falling in
    let can_move = List.fold_left (&&) true can_move_list in
    if can_move then List.map (fun (x, y) -> (x, y+1)) falling else falling
  
  let move_down play =
    let grid = get_grid play in
    let falling = get_falling play in
    let can_move_list = List.map (fun (x, y) -> grid.(x-1).(y) = '.') falling in
    let can_move = List.fold_left (&&) true can_move_list in
    if can_move then List.map (fun (x, y) -> (x-1, y)) falling else falling

  let draw_fallen play =
    let grid = get_grid play in
    let falling = get_falling play in
    let _ = List.map (fun (x, y) -> grid.(x).(y) <- '#') falling in
    [];;

  let rec move play =
    let time = get_time play in
    let stream = get_stream play in
    let air = stream.(time mod (Array.length stream)) in
    let left_right_fallen = if air = '<' then move_left play else move_right play in
    let down_fallen = move_down (set_falling play left_right_fallen) in
    let new_play = set_time (set_falling play down_fallen) (time + 1) in
    if down_fallen = left_right_fallen then new_play else move new_play

  let go play =
    let turn_num = get_turn_num play in
    let top = get_top play in
    let fall_f = List.nth falling_map (turn_num mod 5) in
    let falling = fall_f top in
    let moved = move (set_falling play falling) in
    let _ = draw_fallen moved in
    let new_top = max top (List.fold_left (fun x1 (x2, _) -> max x1 x2) 0 (get_falling moved)) in
    set_turn_num (set_top (set_falling moved []) new_top) (turn_num + 1)
  
  let rec iterate play n = if n = 0 then play else iterate (go play) (n-1)
end;;

let get_input = read_line;;

let main () =
  let stream = get_input () in
  let play = Grid.init 99999 stream in
  let updated = Grid.iterate play 2022 in
  print_int (Grid.get_top updated);;

main ();;