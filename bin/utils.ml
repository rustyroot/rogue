(** Opérateur somme pour les paires d'entiers *)
let ( ++ ) (x, y : int * int) (dx, dy : int * int) : int * int = 
  (x + dx, y + dy)

(** [concat_char] concatène [c] à la fin de [str] *)
let concat_char (str : string) (c : char) : string =
  str ^ (String.make 1 c)

(** [string_of_array] transforme le tableau [tab] en un string *)
let string_of_array (tab : char array) : string =
  List.fold_left concat_char "" (Array.to_list tab) 