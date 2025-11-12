open Notty
open World

(* Affichage des points du joueur *)
let point = ref 0

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

let () = set (9, 0) (HUD '0')

let start_point_writting = 9

let update_point (point : int) =
  begin
    for i = start_point_writting to (width - 1) do 
      set (i, 0) (HUD ' ')
    done
  end;
  if point < 0 then
    exit 0
  else if point = 0 then
    set (start_point_writting, 0) (HUD '0')
  else
    begin
      let length = int_of_float (floor (log10 (float_of_int point) ) )  +1 in
      let i = ref (length - 1) in
      let point = ref point in
      while !point > 0 do
        let c = !point mod 10 in
        (
        set (start_point_writting + !i, 0) (HUD (char_of_int(
                                                      (int_of_char ('0') + c)
                                                            )
                                                )
                                           );
        i := !i - 1;
        point := !point/10
        );
      done
    end


(** Affichage du contenu d'une cellule.*)
let string_of_cell : cell -> string = function
  | Empty      -> "  "
  | Cactus     -> "\u{1F335}"
  | Camel      -> "\u{1F42A}"
  | Snake      -> "\u{1F40D}"
  | Elephant   -> "\u{1F418}"
  | Spider     -> "\u{1F577}"
  | Egg        -> "\u{1F95A}"
  | Monkey     -> "\u{1F412}"
  | HUD (c)    -> String.make 1 c


(* Codes des emojis pour les animaux pertinents
   serpent : "\u{1F40D}"
   éléphant : "\u{1F418}"
   araignée : "\u{1F577} "
   oeuf : "\u{1F95A}"
   monkey "\u{1F412}"
   Des sites comme l'emojipedia peuvent vous donner plus de codes.
*)

(** Fonctions de création de l'image correspondant à l'état actuel du monde.*)
let draw_cell (c : cell) : image = I.string A.empty (string_of_cell c)

let draw_world () : image =
  I.hcat
  @@ Array.to_list
  @@ Array.map
       (fun column -> I.vcat @@ Array.to_list @@ Array.map draw_cell column)
       world


open Notty_unix

(** [terminal] est une constante qui correspond au terminal où le jeu est joué*)
let terminal : Term.t = Term.create ()

(** [render ()] met à jour l'affichage courant dans le terminal*)
let render () : unit = Term.image terminal (draw_world ())
let () = render ()