open Utils

(** Type du contenu d'une case du monde. *)
type cell = Empty | Fog | Cactus | Key |
            Camel | Snake | Elephant | Spider | Egg | Monkey


(** Paramètres du monde *)
let width, height = 50, 30
let cactus_density = 0.10
let level_number = ref 0
let lives = ref 0

(** Le monde [world] est un tableau mutable. *)
let world : cell array array = Array.make_matrix width height Empty
let shadowed_world : cell array array = Array.make_matrix width height Fog

(** [get (x,y)] renvoie le contenu de la case en position [x,y] du monde. 
    Renvoie un cactus pour toutes les cases hors du monde.*)
let get (x, y : int * int) : cell = try world.(x).(y) with _ -> Cactus

(** [set (x,y) v] remplit la case en position [x,y] du monde avec l'entité [v].
    Lève [Exception: Invalid_argument] si la position est hors du monde.*)
let set (x, y : int * int) (v : cell) : unit = world.(x).(y) <- v

(** [random_position ()] renvoie une position aléatoire dans le monde*)
let random_position () : int * int = (Random.int width, Random.int height)

(** [world_clear] *)
let world_clear () : unit =
    for i = 0 to width - 1 do
        for j = 0 to height - 1 do
            set (i, j) Empty
        done;
    done

(** [fill_world] met des cactus dans le monde *)
let fill_world () : unit =
    let nb_cells = width * height in
    let nb_cactus = int_of_float (cactus_density *. (float_of_int nb_cells)) in
    for _ = 0 to nb_cactus do set (random_position ()) Cactus   done;
    set (random_position ()) Key


    