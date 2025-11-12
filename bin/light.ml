open World

let rec get_touched_cells (src : int * int) (dst : int * int) : (int * int) list =
  if src = dst then [src] else
  let x, y = src in
  let x', y' = dst in
  if x' < x then (* On réduit les cas étudiés en s'intéressant uniquement au droite orientée allant de gauche à droite*)
    List.rev (get_touched_cells dst src)
  else
    if x = x' then (* On gère à part les droites verticales*)
      if y' > y then (* droite verticale montante *)
        List.init (y' - y + 1) (fun k -> (x, y + k))
      else (* y' <= y *) (* droite verticale descendante *)
        List.init (y - y' + 1) (fun k -> (x, y - k))
    else
      begin
        let a = ((float_of_int y') -. (float_of_int y)) /. ((float_of_int x') -. (float_of_int x)) in
        let b = a *. (float_of_int x) in
        let f_right_border = fun x ->  int_of_float (ceil ((a *. ((float_of_int x) +. 0.5) -. b) -. 0.5)) in
        let rec construct_list (x : int) (left_y : int) (acc : (int * int) list) : (int * int) list =
          if x > x' then
            acc
          else
            let right_y = f_right_border x in
            let x_cell_list =
              if right_y > left_y then (* droite montante *)
                List.init (right_y - left_y +1) (fun k -> (x, left_y + k))
              else if right_y < left_y then (* droite descendante *)
                List.init (left_y - right_y +1) (fun k -> (x, left_y - k))
              else
                [x, left_y]
            in
            construct_list (x+1) right_y (acc@x_cell_list)
        in
        let rec cut_out_of_range (cell_list : (int*int) list) (begin_pass : bool) : (int*int) list =
          match cell_list with
          | head::_ when head = (x', y') -> [head]
          | head::tail when head = (x, y) -> head::(cut_out_of_range tail true)
          | head::tail when begin_pass -> head::(cut_out_of_range tail begin_pass)
          | _::tail -> cut_out_of_range tail begin_pass
          | [] -> []
        in
        cut_out_of_range (construct_list x (f_right_border (x-1)) []) false
      end

let enlighten_the_world (camel_position : int * int) : unit =
  let rec enlight (path_of_light : (int * int) list) : unit =
    match path_of_light with
    | (i, j)::tail when get (i, j) = Empty -> 
      begin
        shadowed_world.(i).(j) <- world.(i).(j); 
        enlight tail
      end
    | (i, j)::_ -> shadowed_world.(i).(j) <- world.(i).(j)
    | [] -> ()
  in  
  for i = 0 to 0 do
    for j = 0 to 0 do
      let path_of_light = get_touched_cells camel_position (i, j) in
      List.iter (fun (i, j) -> ) path_of_light 
    done; 
  done;