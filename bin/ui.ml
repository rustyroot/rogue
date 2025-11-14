open Notty
open World
open Utils
open Flag

(* HUD *)
(* Constantes : *)
let lives_str = "lives : "
let level_str = "level : "
let broken_heart = "\u{1F494}"
let heart = "\u{2764}"
let start_lives_writting = 0
let start_heart_writting = start_lives_writting + String.length lives_str
let start_level_writting = start_heart_writting + 4
let start_level_number_writting = start_level_writting + String.length level_str

let hud = Array.make width " "

(** [clear_hud ()] Remplace tout les caractères de l'HUD par " " *)
let clear_hud () = 
  begin
    for i = 0 to (width - 1) do 
      hud.(i) <- " "
    done
  end

(** [string_to_hud] écrit [str] dans l'HUD à partir de [starting_pos] *)
let string_to_hud (starting_pos : int) (str : string) : unit =
  String.iteri (fun i x -> (hud.(starting_pos + i) <- String.make 1 x)) str

(** [update_level_hud] update l'HUD avec le [level] actuel *)
let update_level_hud () : unit =
  string_to_hud start_level_number_writting (string_of_int !level_number)

(** [update_lives_hud] update l'HUD avec le bon nombre vies pleines et vides *)
let update_lives_hud () : unit =
  match !lives with
  | 0 -> 
    hud.(start_lives_writting + 8) <- broken_heart;
    hud.(start_lives_writting + 9) <- broken_heart;
    hud.(start_lives_writting + 10) <- broken_heart
  | 1 ->
    hud.(start_lives_writting + 8) <- heart;
    hud.(start_lives_writting + 9) <- broken_heart;
    hud.(start_lives_writting + 10) <- broken_heart
  | 2 ->
    hud.(start_lives_writting + 8) <- heart;
    hud.(start_lives_writting + 9) <- heart;
    hud.(start_lives_writting + 10) <- broken_heart
  | 3 ->
    hud.(start_lives_writting + 8) <- heart;
    hud.(start_lives_writting + 9) <- heart;
    hud.(start_lives_writting + 10) <- heart
  | _ -> failwith "lives > 3 or lives < 0"

(** [init_hud ()] écrit "lives : ❤❤❤ level : 0" dans l'HUD *)
let init_hud () =
  clear_hud ();
  
  string_to_hud start_lives_writting lives_str;
  hud.(start_heart_writting) <- heart;
  hud.(start_heart_writting + 1) <- heart;
  hud.(start_heart_writting + 2) <- heart;

  if level_activated then
    (
    string_to_hud start_level_writting level_str;
    update_level_hud ()
    )
  else ()



let () = init_hud ()


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
  | Tomb       -> "\u{1FAA6}"


(** Fonctions de création de l'image correspondant à l'état actuel du monde.*)
let draw_cell (c : cell) : image = I.string A.empty (string_of_cell c)

let draw_world () : image =
  I.(<->) 
    (I.(<->)
      (I.string A.empty (string_of_array hud))
      (I.hcat @@ Array.to_list @@ Array.make width (I.string A.empty "══"))
      )
    (I.hcat
      @@ Array.to_list
      @@ Array.map
          (fun column -> I.vcat @@ Array.to_list @@ Array.map draw_cell column)
          (if is_curse_darkness_on then shadowed_world else world)
    )


open Notty_unix

(** [terminal] est une constante qui correspond au terminal où le jeu est joué*)
let terminal : Term.t = Term.create ()

(** [render ()] met à jour l'HUD puis l'affichage courant dans le terminal*)
let render () : unit =
  if level_activated then
    update_level_hud ()
  else () ;
  update_lives_hud () ;
  Term.image terminal (draw_world ())
let () = render ()