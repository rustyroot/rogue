open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open Entity

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

(** [snake current_pos] effectue tous les prochains tours du serpent à partir de la position 
    [current_pos] (choisir aléatoirement une entrée, se déplacer en conséquence, recommencer)*)
let rec snake (snake_instance : entity) : unit =
  let current_position = snake_instance#get_pos in
  let new_position = current_position ++ random_direction () in
  let new_position = move current_position new_position in
  snake_instance#set_pos new_position;
  perform End_of_turn;
  snake snake_instance