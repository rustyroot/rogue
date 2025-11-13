(** [file_create] Crée une file vide *)
let file_create () : ('a * int) list ref = ref []

let comp a b = let (_,va) = a in let (_,vb) = b in 
  if va > vb then 1 else (if va = vb then 0 else (-1))

(** [file_push] rajoute l'élement [x] avec la priorité [v] à [f] *)
let file_push (f : ('a * int) list ref) (x : 'a) (v : int) : unit = 
  let rec insere l = match l with
    |[] -> [(x,v)]
    |(t,vt)::q -> if v<vt then (x,v)::(l) else (t,vt)::(insere q)
  in
f := insere (!f)

(** [file_pop] renvoie l'élement de plus faible priorité de [f] en l'enlevant à la file*)
let file_pop (f : ('a * int) list ref) : 'a =
  match (!f) with
  |[] -> failwith "file vide"
  |(t,_)::q -> f := q; t

(** [file_mem] renvoie true si [x] appartient à [f], false sinon *)
let file_mem f (x : 'a) : bool = 
  let rec aux l = match l with
    |[] -> false
    |(t,_)::q -> if t=x then true else aux q
  in
  aux (!f)

(** [file_is_empty] renvoie true si la file est vide *)
let file_is_empty (f : ('a * int) list ref) : bool = (!f) = []

