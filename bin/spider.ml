open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open Entity
open World

class egg entity_instance =
  object
    inherit entity entity_instance
    val mutable generation_remaining = 3
    val mutable nb_turns_before_generation = 20


    method get_generation_remaining = generation_remaining
    method set_generation_remaining new_generation_remaining = generation_remaining <- new_generation_remaining

    method get_nb_turns_before_generation = nb_turns_before_generation
    method set_nb_turns_before_generation new_nb_turns_before_generation =nb_turns_before_generation <- new_nb_turns_before_generation

  end

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
    try spawn_egg new_position with No_cell_avaible -> ();
  else
    ();

  perform End_of_turn;  
  spider spider_instance

and spawn_spider (egg_position: int * int) =
  let spider_position = get_random_nearby_cell egg_position in
  let spider_instance = new entity spider_position in
  set spider_position Spider;
  Queue.add (fun () -> player (fun () -> spider spider_instance)) queue

and egg (egg_instance : egg) : unit =

  let nb_turns_before_generation = egg_instance#get_nb_turns_before_generation in
  if nb_turns_before_generation > 0 then
    begin
      egg_instance#set_nb_turns_before_generation (nb_turns_before_generation - 1);
    end
  else
    begin
      let egg_position = egg_instance#get_pos in
      begin
      try spawn_spider egg_position with No_cell_avaible -> ();
      end;
      egg_instance#set_nb_turns_before_generation 20;
      egg_instance#set_generation_remaining (egg_instance#get_generation_remaining -1);
    end;
  
  if egg_instance#get_generation_remaining > 0 then
    begin
      perform End_of_turn;
      egg egg_instance
    end
  else
    set egg_instance#get_pos Empty;
  
and spawn_egg (spider_position : int * int) : unit =
  let egg_position = get_random_nearby_cell spider_position in
  let egg_instance = new egg egg_position in
  set egg_position Egg;
  Queue.add (fun () -> player (fun () -> egg egg_instance)) queue



