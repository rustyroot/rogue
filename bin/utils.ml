(** Opérateur somme pour les paires d'entiers *)
let ( ++ ) ((x, y) : int * int) ((dx, dy) : int * int) : int * int =
  (x + dx, y + dy)

(** [string_of_array] transforme le tableau [tab] en un string *)
let string_of_array (tab : string array) : string =
  List.fold_left ( ^ ) "" (Array.to_list tab)
