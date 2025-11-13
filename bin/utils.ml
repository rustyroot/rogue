(** Opérateur somme pour les paires d'entiers *)
let ( ++ ) (x, y : int * int) (dx, dy : int * int) : int * int = 
  (x + dx, y + dy)