open World

(** Point du joueur *)
let point = ref 0


(** Déplacement d'une entité *)

(** Opérateur somme pour les paires d'entiers*)
let ( ++ ) (x, y : int * int) (dx, dy : int * int) : int * int = 
  (x + dx, y + dy)

(** [move old_pos new_pos] déplace le contenu de la case en [old_pos] vers la case [new_pos].
    Si la case [new_pos] est occupé, laisse le monde inchangé.
    Renvoie [new_pos] si le mouvement a eu lieu, et [old_pos] sinon.*)
let move (old_position : int * int) (new_position : int * int) : int * int =
  match get new_position with
  | Empty ->
      let character = get old_position in
      set old_position Empty ;
      set new_position character ;
      new_position
  | Camel ->
    point := !point - 5;
    old_position
  | _ -> old_position

exception No_cell_avaible

let get_free_nearby_cell (position : int * int) : (int * int) list =
  let x, y = position in
  let rec check_nearby_cell (i:int) (j:int) (free_nearby_cell : (int * int) list): (int * int) list =
    if i <= 1 then
      match get (x+i, y+j) with
      | Empty -> check_nearby_cell (i+1) j ((x+i, y+j)::free_nearby_cell)
      | _ -> check_nearby_cell (i+1) j free_nearby_cell
    else
      if j <= 1 then
        check_nearby_cell (-1) (j+1) free_nearby_cell
      else
        free_nearby_cell
  in
  check_nearby_cell (-1) (-1) []
  
let get_random_nearby_cell (position : int * int) : (int * int) =
  let free_nearby_cell = get_free_nearby_cell position in
  let nb_free_nearby_cell = List.length free_nearby_cell in
  if nb_free_nearby_cell = 0 then
    raise No_cell_avaible
  else
    List.nth free_nearby_cell (Random.int nb_free_nearby_cell)