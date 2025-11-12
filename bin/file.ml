let file_create () = ref []

let file_push f x v = 
  let rec insere l = match l with
    |[] -> [(x,v)]
    |(t,vt)::q -> if v < vt then (x,v)::l else (t,vt)::(insere q)
  in
  f := insere (!f)

let file_pop f = match (!f) with
  |[] -> failwith "file vide"
  |(t,_)::q -> f := q; t

let file_mem f x = 
  let rec aux l = match l with
    |[] -> false
    |(t,_)::q -> if t=x then true else aux q
  in
  aux (!f)

let file_is_empty f = if (!f) = [] then true else false