open Utils
open Effect
open Effect.Deep
open Engine
open Entity
open World
open Heap

(** [get_cell_camel ()] renvoie la position du chameau*)
let get_pos_camel () : int * int =
  let rec boucle (i : int) (j : int) : int * int =
    if i >= width then failwith "pas de chameau"
    else begin
      if j >= height then boucle (i + 1) 0
      else match get (i, j) with Camel -> (i, j) | _ -> boucle i (j + 1)
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

(** [compare elt1 elt2] compare deux sommets pondérés selon leur poids *)
let compare (elt1 : (int * int) * int) (elt2 : (int * int) * int) : bool =
  let _, prio1 = elt1 in
  let _, prio2 = elt2 in
  prio1 >= prio2

exception No_path_found

(** [a_star start goal h] renvoit un chemin de [start] à [goal] selon
    l'heuristique [h]*)
let a_star (start : int * int) (goal : int * int)
    (h : int * int -> int * int -> int) : (int * int) list =
  let map = Array.make_matrix width height (-1, -1) in
  let distance = Array.make_matrix width height max_int in
  let goal_found = ref false in

  let x_start, y_start = start in
  map.(x_start).(y_start) <- start;
  distance.(x_start).(y_start) <- 0;
  let file = Heap.init (width * height) compare (start, 0) in

  while (not !goal_found) && not (is_empty file) do
    let (x, y), _ = Heap.pop file in
    if (x, y) = goal then goal_found := true
    else
      let voisins = get_neighbors (x, y) in
      let upgrade (voisin : int * int) =
        let x', y' = voisin in
        let nouvelle_distance = distance.(x).(y) + 1 in
        if nouvelle_distance < distance.(x').(y') then (
          map.(x').(y') <- (x, y);
          distance.(x').(y') <- nouvelle_distance;
          let poids = nouvelle_distance + h voisin goal in
          Heap.push file (voisin, poids))
      in
      List.iter upgrade voisins
  done;

  if !goal_found then reconstruct_path map goal start else raise No_path_found

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
