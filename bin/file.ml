let file_create () = ref []

let comp a b = let (_,va) = a in let (_,vb) = b in 
  if va > vb then 1 else (if va = vb then 0 else (-1))

let file_push f x v = 
  let rec insere l = match l with
    |[] -> [(x,v)]
    |(t,vt)::q -> if v<vt then (x,v)::(l) else (t,vt)::(insere q)
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

let file_is_empty f = match (!f) with |[] -> true |_ -> false