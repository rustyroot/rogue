open Utils
open Effect
open Effect.Deep
open Engine
open Entity
open World

type state_type = Stunned | Charging | Looking

(** Pour un éléphant, on doit se souvenir de sa position (hérité depuis entity) de son état et durée (state et stunned (resp. charge)) *)
class elephant entity_instance =
  object
    inherit entity entity_instance
    val mutable stunned = 0
    val mutable charge = 0
    val mutable charge_direction = (0, 0)
    val mutable state = Looking


    method get_stunned = stunned
    method set_stunned new_stunned = stunned <- new_stunned

    method get_charge = charge
    method set_charge new_charge = charge <- new_charge

    method get_charge_direction = charge_direction
    method set_charge_direction new_charge_direction = charge_direction <- new_charge_direction
  
    method get_state = state
    method set_state new_state = state <- new_state
  end

let rec camel_on_sight_direction (current_position : int * int) (direction : int * int) : bool =
  match current_position with
  | (x, y) ->
    (
      try (match world.(x).(y) with
           | Empty -> camel_on_sight_direction (current_position ++ direction) direction
           | Camel -> true
           | _ -> false
          )
      with
      | _ -> false
    )

(** [camel_on_sight] regarde dans les 4 directions depuis [current_position]
si le chameau y est en vue. Si c'est le cas, on renvoie la direction du chameau sinon (0,0) *)
let camel_on_sight (current_position : int * int) : (int * int) =
  if camel_on_sight_direction (current_position ++ (- 1, 0)) (- 1, 0) then (- 1, 0)
  else if camel_on_sight_direction (current_position ++ (+ 1, 0)) (+ 1, 0) then (+ 1, 0)
  else if camel_on_sight_direction (current_position ++ (0, + 1)) (0, + 1) then (0, + 1)
  else if camel_on_sight_direction (current_position ++ (0, - 1)) (0, - 1) then (0, - 1)
  else (0, 0)


(** [random_direction ()] Renvoie une direction aléatoire dans le cas où
l'éléphant se déplace sans objectif *)
let random_direction () : int * int =
  let random_move = (Random.int 4) in
  match random_move with
  | 0 -> (- 1, 0) (*Left*)
  | 1 -> (+ 1, 0) (*Right*)
  | 2 -> (0, + 1) (*Down*)
  | 3 -> (0, - 1) (*Up*)
  | _ -> (0, 0) (*Exhaustive pattern*)

(** [elephant elephant_instance] calcule le nouvel état de l'éléphant en fonction du précédent
- Si l'éléphant était en [Looking] du chameau et qu'il l'a trouvé, il passe en [Charging]
- Si il était en [Charging] ou [Stunned], il y reste jusqu'à la fin du compteur puis il repasse en [Looking]
- Si il rencontre un obstacle pendant [Charging], il passe en [Stunned]*)
let rec elephant (elephant_instance : elephant) : unit =
  let current_position = elephant_instance#get_pos in
  let state_start = elephant_instance#get_state in

  (* Calcul de la nouvelle position à partir de la position précédente et de l'état de l'éléphant*)
  let new_position =
    match state_start with

    (* Si il cherche le chameau, il regarde si il le voit
       Si c'est le cas, il le charge, sinon il se déplace aléatoirement *)
    | Looking -> 
      begin
        match (camel_on_sight current_position) with
        | (0, 0) -> current_position ++ random_direction ()
        | direction ->
          (elephant_instance#set_charge_direction direction;
           elephant_instance#set_charge 9;
           elephant_instance#set_state Charging;
           current_position ++ direction)
      end

    (* Si il chargeait, on diminue sa durée de 1 sauf si elle est finie et on update sa position*)
    | Charging -> 
      elephant_instance#set_charge (elephant_instance#get_charge -1);
      begin 
        if elephant_instance#get_charge = 0 then
          elephant_instance#set_state Looking
      end;
      current_position ++ elephant_instance#get_charge_direction
    
    (* Si il était stunned, on diminue la durée de 1 sauf si c'est 0 et il ne se déplace pas*)
    | Stunned -> 
      elephant_instance#set_stunned (elephant_instance#get_stunned -1);
      if elephant_instance#get_stunned = 0 then
        begin
          elephant_instance#set_state Looking
        end;
      current_position
  in

  let new_position = move current_position new_position in
  let is_blocked = (new_position = current_position) in
  (* si true, c'est que l'éléphant n'a pas réussi à charger en avant, il a été bloqué et passe en [Stunned] *)
  (match (is_blocked, elephant_instance#get_state) with
  | (true, Charging) ->
    begin
      elephant_instance#set_state Stunned;
      elephant_instance#set_stunned 20
    end
  | _ -> ()
  );
  elephant_instance#set_pos new_position;
  perform End_of_turn;
  elephant elephant_instance