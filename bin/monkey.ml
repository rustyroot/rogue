open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open Entity
open World

(** [get_cell_camel ()] renvoie la position du chameau*)
let get_pos_camel () : int * int =
  let rec boucle (i : int) (j : int) : int * int =
    if i >= width then failwith "pas de chameau"
    else begin
      if j >= height then boucle (i + 1) 0
      else match get (i, j) with
      | Camel -> (i, j)
      | _ -> boucle i (j + 1)
    end
  in
  boucle 0 0

let abs (x : int) = if x >= 0 then x else -x

(** [distance] calcul la distance de Manhattan entre deux positions*)
let distance (pos1 : int * int) (pos2 : int * int) : int =
  let l1, c1 = pos1 in
  let l2, c2 = pos2 in
  abs (l1 - l2) + abs (c1 - c2)

(** [reconstruct_path] reconstruit le chemin de [start] à [goal] pour le singe
*)
let reconstruct_path (map : (int * int) array array) (goal : int * int)
    (start : int * int) : (int * int) list =
  let rec reconstruct_from_goal position path_acc =
    if position = start then path_acc
    else
      let l, c = position in
      reconstruct_from_goal map.(l).(c) (position :: path_acc)
  in
  reconstruct_from_goal goal []

(** [get_neighbors] renvoie la liste des voisins [Empty] ou [Camel] de [pos] *)
let get_neighbors (position : int * int) : (int * int) list =
  let l, c = position in
  let is_empty_or_camel pos =
    match get pos with Empty | Camel -> true | _ -> false
  in
  List.filter is_empty_or_camel
    [ (l + 1, c); (l - 1, c); (l, c + 1); (l, c - 1) ]

let pop_min (liste : ((int * int) * int) list) :
    (int * int) * ((int * int) * int) list =
  let rec cherche l acc x_min v_min =
    match l with
    | [] -> (x_min, acc)
    | (x, v) :: q ->
        if v < v_min then cherche q ((x_min, v_min) :: acc) x v
        else cherche q ((x, v) :: acc) x_min v_min
  in
  match liste with
  | [] -> failwith "impossible de pop dans une liste vide"
  | (x1, v1) :: lst -> cherche lst [] x1 v1

let appartient (liste : ((int * int) * int) list) (x : int * int) : bool =
  let rec cherche l =
    match l with
    | [] -> false
    | (y, _) :: q -> if x = y then true else cherche q
  in
  cherche liste

exception No_path_found

let a_star (start : int * int) (goal : int * int)
    (h : int * int -> int * int -> int) : (int * int) list =
  let map = Array.make_matrix width height (-1, -1) in
  let vrai_scores = Array.make_matrix width height max_int in
  let faux_scores = Array.make_matrix width height max_int in
  let ls, cs = start in
  map.(ls).(cs) <- (ls, cs);
  vrai_scores.(ls).(cs) <- 0;
  faux_scores.(ls).(cs) <- h start goal;
  let rec upgrade lst_voisins lc cc file =
    match lst_voisins with
    | [] -> file
    | (l, c) :: q ->
        let tentative_vrai_score = vrai_scores.(lc).(cc) + 1 in
        if tentative_vrai_score < vrai_scores.(l).(c) then (
          map.(l).(c) <- (lc, cc);
          vrai_scores.(l).(c) <- tentative_vrai_score;
          faux_scores.(l).(c) <- tentative_vrai_score + h (l, c) goal;
          if appartient file (l, c) then upgrade q lc cc file
          else upgrade q lc cc (((l, c), faux_scores.(l).(c)) :: file))
        else upgrade q lc cc file
  in
  let rec boucle file =
    if file = [] then raise No_path_found
    else begin
      let current, new_file = pop_min file in
      if current = goal then reconstruct_path map goal start
      else begin
        let lc, cc = current in
        let lst_voisins = get_neighbors (lc, cc) in
        boucle (upgrade lst_voisins lc cc new_file)
      end
    end
  in
  boucle [ ((ls, cs), 0) ]

(** [next_pos_a_star] cherche le prochain mouvement à faire pour le singe depuis
    [pos] selon [a_star] *)
let next_pos_a_star (pos : int * int) : int * int =
  match a_star pos (get_pos_camel ()) distance with
  | [] -> failwith "pas possible"
  | p :: _ -> p

(** [monkey] cherche le prochain mouvement pour le singe selon [a_star] et
    l'effectue depuis sa position *)
let rec monkey (monkey_instance : entity) : unit =
  let current_position = monkey_instance#get_pos in
  let new_position = next_pos_a_star current_position in
  let new_position = move current_position new_position in
  monkey_instance#set_pos new_position;
  perform End_of_turn;
  monkey monkey_instance
