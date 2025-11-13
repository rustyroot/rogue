open Notty
open World

(* Affichage des points du joueur *)
let point = ref 0

let start_point_writting = 9

let hud = Array.make width ' '

let clear_hud () = 
  begin
    for i = 0 to (width - 1) do 
      hud.(i) <- ' '
    done
  end

let init_hud () =
  clear_hud ();
  hud.(0) <- 'P';
  hud.(1) <- 'o';
  hud.(2) <- 'i';
  hud.(3) <- 'n';
  hud.(4) <- 't';
  hud.(5) <- 's';

  hud.(7) <- ':';

  hud.(9) <- '0'

let () = init_hud ()

let update_point (point : int) =
  init_hud();
  if point < 0 then
    exit 0
  else if point = 0 then
    hud.(start_point_writting) <- '0'
  else
    begin
      let length = int_of_float (floor (log10 (float_of_int point) ) )  +1 in
      let i = ref (length - 1) in
      let point = ref point in
      while !point > 0 do
        let c = !point mod 10 in
        (
        hud.(start_point_writting + !i) <- (char_of_int( (int_of_char ('0') + c) ) );
        i := !i - 1;
        point := !point/10
        );
      done
    end


let concat_char (str : string) (c : char) : string =
  str ^ (String.make 1 c)

let string_of_array (tab : char array) : string =
  List.fold_left concat_char "" (Array.to_list tab) 

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
  | Fog        -> "\u{1F7E6}"


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