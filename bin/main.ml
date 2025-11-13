open World
open Ui
open Utils
open Light
open Flag

open Player
open Snake
open Elephant
open Spider
open Entity
open Monkey

open Engine
open Player

(* Initialisation des niveaux *)

let levels_enemies = [|
  [|3; 0; 1; 0|]; (* level 1 *)
  [|3; 0; 1; 0|]; (* level 2 *)
  [|3; 1; 2; 0|]; (* level 3 *)
  [|4; 1; 2; 0|]; (* level 4 *)
  [|4; 2; 2; 1|]; (* level 5 *)
  [|4; 2; 2; 1|]; (* level 6 *)
  [|4; 2; 3; 1|]; (* level 7 *)
  [|6; 3; 3; 1|]; (* level 8 *)
  [|6; 3; 3; 1|]; (* level 9 *)
  [|6; 3; 4; 2|]; (* level 10 *)
  [|6; 4; 4; 2|]  (* level 11 *) (* niveau en boucle à l'infini *)
|]

type enemy = | Entity of (entity -> unit) | Elephant of (elephant -> unit)
let enemies = [|Entity snake; Elephant elephant; Entity spider; Entity monkey|]
let enemies_cell = [|Snake; Elephant; Spider; Monkey|]

let set_level () : unit =
  Queue.clear queue;
  world_clear ();
  fill_world ();
  let level_enemies = levels_enemies.(!level_number) in
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
  let () = set camel_initial_position Camel in
  let camel_instance = new entity camel_initial_position in 
  Queue.add (fun () -> player (fun () -> camel camel_instance)) queue

(* Initialisation du module Random*)
let () = Random.self_init ()

(* Initialisation du monde *)
let () = fill_world ()

(* Début du jeu *)
let rec play () : unit =
  try run_queue ()
  with effect End_of_level, _ ->
    set_level(); render(); play ()

let () = set_level ()
let () = play ()


