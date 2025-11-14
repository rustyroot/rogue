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
open Level


(* Initialisation du module Random*)
let () = Random.self_init ()

(* Initialisation du monde *)
let () = fill_world ()

(** [play] lance le jeu
    Si on arrive à la fin d'un niveau, on lance le suivant en redonnant une vie au joueur
    Si on est mort, le jeu se termine lorsqu'il reçoit le prochain input*)
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
    if level_activated then
      Printf.printf "Final Level : %d\n" !level_number;
    match Term.event terminal with
    | _ -> exit 0


(* Début du jeu *)

let () = init_hud ()
let () = set_level ()
let () = play ()


