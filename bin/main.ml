open Roguelib
open Notty_unix

open World
open Ui
open Utils
open Light
open Flag
open Engine

open Player
open Snake
open Elephant
open Spider
open Entity
open Monkey

open Player

(* Initialisation des niveaux *)
let base_level = [|1; 1; 1; 1|]

let levels_enemies = [|
  [|3; 0; 1; 0|]; (* level 0 *)
  [|3; 0; 1; 0|]; (* level 1 *)
  [|3; 1; 2; 0|]; (* level 2 *)
  [|4; 1; 2; 0|]; (* level 3 *)
  [|4; 2; 2; 1|]; (* level 4 *)
  [|4; 2; 2; 1|]; (* level 5 *)
  [|4; 2; 3; 1|]; (* level 6 *)
  [|6; 3; 3; 1|]; (* level 7 *)
  [|6; 3; 3; 1|]; (* level 8 *)
  [|6; 3; 4; 2|]; (* level 9 *)
  [|6; 4; 4; 2|]  (* level 10 *) (* niveau en boucle à l'infini *)
|]

type enemy = | Entity of (entity -> unit) | Elephant of (elephant -> unit)
let enemies = [|Entity snake; Elephant elephant; Entity spider; Entity monkey|]
let enemies_cell = [|Snake; Elephant; Spider; Monkey|]

let set_level () : unit =
  Queue.clear queue;
  world_clear ();
  fill_world ();
  
  let level_enemies =
    (if level_activated then
      let number_of_level = Array.length levels_enemies in
      if !level_number < number_of_level then
        levels_enemies.(!level_number)
      else
        levels_enemies.(number_of_level-1)
    else
      base_level)
  in
  for i = 0 to (Array.length enemies -1) do
      for _ = 1 to (level_enemies.(i)) do
        match enemies.(i) with
        |Elephant(elephant) ->
          let elephant_initial_position = random_position () in
          let () = set elephant_initial_position enemies_cell.(i) in
          let elephant_instance = new elephant elephant_initial_position in
          Queue.add (fun () -> player (fun () -> elephant elephant_instance)) queue
        |Entity(enemy) ->
          let entity_initial_position = random_position () in
          let () = set entity_initial_position enemies_cell.(i) in
          let entity_instance = new entity entity_initial_position in
          Queue.add (fun () -> player (fun () -> enemy entity_instance)) queue
        done
  done;
  set (random_position ()) Key;
  let camel_initial_position = random_position () in
  set camel_initial_position Camel;
  if is_curse_darkness_on then
    enlighten_the_world camel_initial_position
  else
    ();
  let camel_instance = new entity camel_initial_position in 
  Queue.add (fun () -> player (fun () -> camel camel_instance)) queue

(* Initialisation du module Random*)
let () = Random.self_init ()

(* Initialisation du monde *)
let () = fill_world ()


(* Début du jeu *)
let rec play () : unit =
  try run_queue ()
  with
  | effect End_of_level, _ ->
    set_level();
    if !lives < 3 then
      lives := !lives + 1
    else () ;
    render();
    play ()
  | effect End_of_game camel_position, _ ->
    set camel_position Tomb;
    render();
    match Term.event terminal with
    | _ -> exit 0

let () = set_level ()
let () = play ()


