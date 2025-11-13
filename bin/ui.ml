open Notty
open World
open Utils

(* HUD *)

let point = ref 0
let broken_heart = "\u{1F494}"
let heart = "\u{2764}"
let start_live_writting = 0
let start_level_writting = start_live_writting + 13
let hud = Array.make width " "

(** [clear_hud ()] Remplace tout les caractères de l'HUD par " " *)
let clear_hud () = 
  begin
    for i = 0 to (width - 1) do 
      hud.(i) <- " "
    done
  end

(** [init_hud ()] écrit "Point : 0" dans l'HUD *)
let init_hud () =
  clear_hud ();
  hud.(start_live_writting) <- "L";
  hud.(start_live_writting + 1) <- "i";
  hud.(start_live_writting + 2) <- "v";
  hud.(start_live_writting + 3) <- "e";
  hud.(start_live_writting + 4) <- "s";

  hud.(start_live_writting + 6) <- ":";

  hud.(start_live_writting + 8) <- heart;
  hud.(start_live_writting + 9) <- heart;
  hud.(start_live_writting + 10) <- heart;

  hud.(start_level_writting) <- "L";
  hud.(start_level_writting + 1) <- "e";
  hud.(start_level_writting + 2) <- "v";
  hud.(start_level_writting + 3) <- "e";
  hud.(start_level_writting + 4) <- "l";

  hud.(start_level_writting + 6) <- ":"


let () = init_hud ()

(** [update_point] update l'HUD avec le nouveaux nombre de [point] *)
let update_level () : unit =
  if !level_number = 0 then
    hud.(start_level_writting + 8) <- "0"
  else
    begin
      let length = int_of_float (floor (log10 (float_of_int !level_number) ) )  +1 in
      let i = ref (length - 1) in
      let level_number_temp = ref (!level_number) in
      while !level_number_temp > 0 do
        let c = !level_number_temp mod 10 in
        (
        hud.(start_level_writting + 8 + !i) <- String.make 1 (char_of_int( (int_of_char ('0') + c) ) );
        i := !i - 1;
        level_number_temp := !level_number_temp/10
        );
      done
    end

(** Affichage du contenu d'une cellule.*)
let string_of_cell : cell -> string = function
  | Empty      -> "  "
  | Cactus     -> "\u{1F335}"
  | Key        -> "\u{1F511}"
  | Camel      -> "\u{1F42A}"
  | Snake      -> "\u{1F40D}"
  | Elephant   -> "\u{1F418}"
  | Spider     -> "\u{1F577}"
  | Egg        -> "\u{1F95A}"
  | Monkey     -> "\u{1F412}"
  | Fog        -> "░░"


(** Fonctions de création de l'image correspondant à l'état actuel du monde.*)
let draw_cell (c : cell) : image = I.string A.empty (string_of_cell c)

let draw_world () : image =
  I.(<->) 
    (I.string A.empty (string_of_array hud))
    (I.hcat
      @@ Array.to_list
      @@ Array.map
          (fun column -> I.vcat @@ Array.to_list @@ Array.map draw_cell column)
          shadowed_world
    )


open Notty_unix

(** [terminal] est une constante qui correspond au terminal où le jeu est joué*)
let terminal : Term.t = Term.create ()

(** [render ()] met à jour l'affichage courant dans le terminal*)
let render () : unit = Term.image terminal (draw_world ())
let () = render ()