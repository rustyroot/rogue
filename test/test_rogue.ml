open Roguelib
open World
open Entity
open Light

let _ = descrete_ray_cast (0, 0) (0, 4)
let _ = descrete_ray_cast (0, 0) (5, 0)
let _ = descrete_ray_cast (0, 0) (5, 3)
let _ = descrete_ray_cast (5, 3) (0, 0)
let _ = descrete_ray_cast (2, 0) (3, 3)
let _ = descrete_ray_cast (0, 3) (5, 0)




let () = override_world [|
  [|Key   ;Camel ;Empty ;Empty ;Empty|];
  [|Snake ;Empty ;Empty ;Empty ;Empty|];
  [|Empty ;Empty ;Empty ;Empty ;Empty|];
  [|Camel ;Empty ;Spider;Empty ;Empty|];
  [|Empty ;Empty ;Empty ;Empty ;Empty|]|];;

(* On déplace l'araignée de 1 *)
assert (world.(3).(2) = Spider);;
assert (world.(3).(1) = Empty);;
assert (move (3, 2) (3, 1) = (3, 1));;
assert (world.(3).(2) = Empty);;
assert (world.(3).(1) = Spider)

(* L'araignée attaque le chameau du bas *)
let () = lives := 12;;
assert (move (3, 1) (3, 0) = (3, 1));;
assert (world.(3).(1) = Spider);;
assert (world.(3).(0) = Camel);;
assert (!lives = 11)

(* Le serpent essaye de récupérer la clé *)
let () = level_number := 42;; 
assert (move (1, 0) (0, 0) = (1, 0));;
assert (world.(1).(0) = Snake);;
assert (world.(0).(0) = Key);;
assert (!level_number = 42)

(* Le chameau du haut essaye de récupérer la clé *)
let () = level_number := 42;;
assert (move (0, 1) (0, 0) = (0, 1));;
assert (world.(0).(1) = Camel);;
assert (world.(0).(0) = Key);;
assert (!level_number = 42) (* Comportement voulu car ici level_activated = false *)

(* L'araignée tue le chameau du bas : 
   Le assert ne passe que si l'effet End_of_game est catch
   et que la position renvoyée est celle du camel *)
let () = lives := 1;;
assert (
  try let _ = move (3, 1) (3, 0) in false with
  | effect End_of_game pos, _ -> (pos = (3, 0))
  | _ -> false
  );;
assert (world.(3).(1) = Spider);;
assert (world.(3).(0) = Camel);;
assert (!lives = 0);;

(*(* On charge le niveau 9 et on vérifie que chaque entité est présente le bon
  nombre de fois *)
let () = level_number := 9
let () = set_level ()
let enemies_number = Array.make (Array.length enemies) 0
let () = 
for i = 0 to height - 1 do
  for j = 0 to width - 1 do
    for enemy = 0 to (Array.length enemies - 1) do
      if get (i, j) = enemies.(enemy) then
        enemies_number.(enemy) <- enemies_number.(enemy) + 1
      else();
    done;
  done;
done
let () = assert (enemies_number = levels_enemies.(!level_number));*)

    

















