open World
open Ui
open Utils

open Player
open Snake
open Elephant
open Spider
open Entity
open Monkey

open Engine
open Player

(* Initialisation du monde *)

(* Initialisation du module Random*)
let () = Random.self_init ()

(** [random_position ()] renvoie une position aléatoire dans le monde*)
let random_position () : int * int = (Random.int width, (Random.int height) + 1)

(* Place les cactus et le chameau initialement.*)

let () =
  for _ = 0 to 200 do set (random_position ()) Cactus   done 

let camel_initial_position = random_position ()
let () = set camel_initial_position Camel

let snake_initial_position = random_position ()
let () = set snake_initial_position Snake
let snake_instance = new entity snake_initial_position

let elephant_initial_position = random_position ()
let () = set elephant_initial_position Elephant
let elephant_instance = new elephant elephant_initial_position

let spider_initial_position = random_position ()
let () = set spider_initial_position Spider
let spider_instance = new entity spider_initial_position

let () = 
  begin
    for i = 0 to (width - 1) do 
      set (i, 0) (HUD ' ')
    done
  end

let () = set (0, 0) (HUD 'P')
let () = set (1, 0) (HUD 'o')
let () = set (2, 0) (HUD 'i')
let () = set (3, 0) (HUD 'n')
let () = set (4, 0) (HUD 't')
let () = set (5, 0) (HUD 's')
let () = set (7, 0) (HUD ':')




(* La file contient uniquement le tour du chameau *)

let () = Queue.add (fun () -> player (fun () -> camel camel_initial_position)) queue

let () = Queue.add (fun () -> player (fun () -> snake snake_instance)) queue

let () = Queue.add (fun () -> player (fun () -> elephant elephant_instance)) queue

let () = Queue.add (fun () -> player (fun () -> spider spider_instance)) queue

(* Début du jeu *)
let () = run_queue ()


