open Notty_unix
open Ui
open Utils
open Effect
open Effect.Deep
open Engine
open Entity
open World
open File

(** [get_cell_camel ()]
    renvoie la position du chameau*)
let get_pos_camel () : int*int = 
  let rec boucle (i : int) (j : int) : int*int = 
    if i >= height then failwith "pas de chameau" 
    else begin
      if j >= width then boucle (i+1) 0 
      else match get (i,j) with
        | Camel -> (i,j)
        | _ -> boucle i (j+1)
    end
  in
  boucle 0 0

let abs (x : int) = if x>=0 then x else (-x)

(** [distance]
  calcul la distance de Manhattan entre deux positions*)
let distance (pos1 : int*int) (pos2 : int*int) : int = 
  let (l1,c1) = pos1 in
  let (l2,c2) = pos2 in
  (abs (l1 - l2) + abs (c1 - c2))


let reconstruct_path (map : (int*int) array array) (goal : int*int) (start : int*int) : (int*int) list =
  let rec aux p acc = 
    if p = start then acc 
    else let (l,c) = p in aux (map.(l).(c)) (p::acc)
  in
  aux goal []

let get_neighbors (pos : int*int) : (int*int) list =
  let lst = ref [] in
  let (l,c) = pos in
  if l+1 < height then match world.(l+1).(c) with |Empty -> lst := (l+1,c)::(!lst) |_ -> () else () ;
  if l-1 >=0 then match world.(l-1).(c) with |Empty -> lst := (l-1,c)::(!lst) |_ -> () else () ;
  if c+1 < width then match world.(l).(c+1) with |Empty -> lst := (l,c+1)::(!lst) |_ -> () else () ;
  if c-1 <= 0 then match world.(l).(c-1) with |Empty -> lst := (l,c-1)::(!lst) |_ -> () else () ;
  !lst

let a_star (start:int*int) (goal:int*int) (h:(int*int) -> (int*int) -> int) : (int*int) list =
  let file = file_create () in
  let map = Array.make_matrix height width (-1,-1) in
  let vrai_scores = Array.make_matrix height width max_int in
  let faux_scores = Array.make_matrix height width max_int in
  let (ls,cs) = start in
  map.(ls).(cs) <- (ls,cs);
  vrai_scores.(ls).(cs) <- 0;
  faux_scores.(ls).(cs) <- h start goal;
  let rec boucle current =
    if current = goal then reconstruct_path map goal start else begin
      if file_is_empty file then failwith "No path" 
      else begin
        let neighbors = get_neighbors current in
        let rec update_neighbors neighbors =
          match neighbors with
          |[] -> ()
          |(l,c)::tail -> (
            let tentative_vrai_score = vrai_scores.(l).(c) + 1 in
            if tentative_vrai_score < vrai_scores.(l).(c) then (
              map.(l).(c) <- current;
              vrai_scores.(l).(c) <- tentative_vrai_score;
              faux_scores.(l).(c) <- tentative_vrai_score + (h (l,c) goal);
              if not (file_mem file (l,c)) then
                file_push file (l,c) faux_scores.(l).(c)
            );
            update_neighbors tail
          )
        in
        update_neighbors neighbors;
        boucle (file_pop file)
      end
    end
  in
  boucle (file_pop file)



let next (pos : int*int) : int*int= 
  match a_star pos (get_pos_camel()) distance with
    |[] -> failwith "pas possible"
    |p::_ -> p

(** [snake current_pos] effectue tous les prochains tours du serpent à partir de la position 
    [current_pos] (choisir aléatoirement une entrée, se déplacer en conséquence, recommencer)*)
let rec monkey (monkey_instance : entity) : unit =
  let current_position = monkey_instance#get_pos in
  let new_position = next current_position in
  let new_position = move current_position new_position in
  monkey_instance#set_pos new_position;
  perform End_of_turn;
  monkey monkey_instance