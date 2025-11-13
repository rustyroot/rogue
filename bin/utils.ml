open World
open Ui

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
    if get old_position != Camel then (* Le chameau ne se retire pas de point quand il se recontre lui même *)
      point := !point - 5
    else
      ();
    old_position
  | _ -> old_position

(** [random_direction ()]
    Choisie alléatoirement une direction et renvoie le changement à appliquer sur les coordonnées
    du serpent pour aller dans la direction correspondante.*)
let random_direction () : int * int =
  let random_move = (Random.int 4) in
  match random_move with
  | 0 -> (- 1, 0) (*Left*)
  | 1 -> (+ 1, 0) (*Right*)
  | 2 -> (0, + 1) (*Down*)
  | 3 -> (0, - 1) (*Up*)
  | _ -> (0, 0) (*Exhaustive pattern*)

(*****************************************************************************)

(* Obtenir une case libre adjacente *)

(** [get_free_nearby_cell position] renvoit la liste des cases [Empty] parmi 
    le carré de 3x3 cases autour de position. *)
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
  
exception No_cell_avaible

(** [get_random_nearby_cell position] choisit aléatoirement une cases [Empty] parmi 
    le carré de 3x3 cases autour de position. 
    Si aucune n'est libre alors [No_cell_avaible] est soulevée. *)
let get_random_nearby_cell (position : int * int) : (int * int) =
  let free_nearby_cell = get_free_nearby_cell position in
  let nb_free_nearby_cell = List.length free_nearby_cell in
  if nb_free_nearby_cell = 0 then
    raise No_cell_avaible
  else
    List.nth free_nearby_cell (Random.int nb_free_nearby_cell)