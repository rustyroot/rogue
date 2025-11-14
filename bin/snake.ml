open Utils
open Effect
open Effect.Deep
open Engine
open Entity

(** [snake current_pos] effectue tous les prochains tours du serpent à partir de
    la position [current_pos] (choisir aléatoirement une entrée, se déplacer en
    conséquence, recommencer)*)
let rec snake (snake_instance : entity) : unit =
  let current_position = snake_instance#get_pos in
  let new_position = current_position ++ random_direction () in
  let new_position = move current_position new_position in
  snake_instance#set_pos new_position;

  perform End_of_turn;
  snake snake_instance
