open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open Entity
open World
open Egg

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

let spawn_egg (spider_position : int * int) : unit =
  let egg_position = get_random_nearby_cell spider_position in
  let egg_instance = new entity egg_position in
  set egg_position Egg;
  Queue.add (fun () -> player (fun () -> egg egg_instance)) queue



(** [spider spider_instance] effectue tous les prochains tours de l'araigné à partir de la position 
    [current_pos] (choisir aléatoirement une entrée, se déplacer en conséquence, recommencer)*)
let rec spider (spider_instance : entity) : unit =

  (* set new position *)
  let current_position = spider_instance#get_pos in
  let new_position = current_position ++ random_direction () in
  let new_position = move current_position new_position in
  spider_instance#set_pos new_position;

  (* try to spawn an egg *)
  if (Random.int 100) = 1 then
    try spawn_egg new_position with
    | No_cell_avaible -> ()
  else
    ();

  perform End_of_turn;  
  spider spider_instance