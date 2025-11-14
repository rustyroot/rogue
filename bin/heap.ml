type 'a heap = {
  capacity : int;
  mutable index : int;
  array : 'a array;
  gt : 'a -> 'a -> bool;
}
(** ['a heap] est une structure de tas min *)

exception Pop_in_empty_heap
exception Push_in_full_heap

(** [init capacity compare first_elt] crée un tas min de capacité [capacity] où
    les élements sont ordonnés selon la relation binaire [compare].*)
let init (capacity : int) (compare : 'a -> 'a -> bool) (first_elt : 'a) :
    'a heap =
  { capacity; index = 1; array = Array.make capacity first_elt; gt = compare }

(** [is_empty heap] renvoit [true] si et seulement si le tas ne contient aucun
    élément.*)
let is_empty (heap : 'a heap) : bool = heap.index = 0

(** [has_a_right_child heap i] renvoit [true] si et seulement si le noeud [i] a
    un fils droit.*)
let has_a_right_child (heap : 'a heap) (i : int) = (2 * i) + 1 < heap.index

(** [has_a_left_child heap i] renvoit [true] si et seulement si le noeud [i] a
    un fils gauche.*)
let has_a_left_child (heap : 'a heap) (i : int) = (2 * i) + 2 < heap.index

(** [is_greater_than_his_right_child heap i] renvoit [true] si et seulement si
    le noeud [i] a un fils droit plus petit que lui. *)
let is_greater_than_his_right_child (heap : 'a heap) (i : int) =
  has_a_left_child heap i && heap.gt heap.array.(i) heap.array.((2 * i) + 1)

(** [is_greater_than_his_left_child heap i] renvoit [true] si et seulement si le
    noeud [i] a un fils gauche plus petit que lui. *)
let is_greater_than_his_left_child (heap : 'a heap) (i : int) =
  has_a_right_child heap i && heap.gt heap.array.(i) heap.array.((2 * i) + 2)

(** [is_greater_than_his_childs heap i] renvoit [true] si et seulement si le
    noeud [i] a un fils plus petit que lui. *)
let is_greater_than_his_childs (heap : 'a heap) (i : int) =
  is_greater_than_his_left_child heap i
  || is_greater_than_his_right_child heap i

(** [swap heap i j] échange les noeuds [i] et [j] sans vérifier leur existence.*)
let swap (heap : 'a heap) (i : int) (j : int) : unit =
  let temp = heap.array.(i) in
  heap.array.(i) <- heap.array.(j);
  heap.array.(j) <- temp

(** [get_smaller_child heap i] renvoit l'indice du plus petit des fils de [i],
    et soulève si [i] n'a pas de fils. *)
let get_smaller_child (heap : 'a heap) (i : int) : int =
  match (has_a_left_child heap i, has_a_right_child heap i) with
  | true, true ->
      if heap.gt heap.array.((2 * i) + 1) heap.array.((2 * i) + 2) then
        (2 * i) + 2
      else (2 * i) + 1
  | false, true -> (2 * i) + 2
  | true, false -> (2 * i) + 1
  | false, false -> failwith "no child"

(** [bury_top heap] replace correctement la racine du tas en comparant celle-ci
    à ses fils *)
let bury_top (heap : 'a heap) : unit =
  let i = ref 0 in
  while is_greater_than_his_childs heap !i do
    let smaller_child = get_smaller_child heap !i in
    swap heap !i smaller_child;
    i := smaller_child
  done

(** [pop head] renvoit le plus petit élément de [head] selon l'ordre [heap.gt]*)
let pop (heap : 'a heap) : 'a =
  if is_empty heap then raise Pop_in_empty_heap;
  let elt = heap.array.(0) in
  heap.array.(0) <- heap.array.(heap.index - 1);
  heap.index <- heap.index - 1;
  bury_top heap;
  elt

(** [push head elt] ajout [elt] dans [heap]*)
let push (heap : 'a heap) (elt : 'a) : unit =
  if heap.capacity = heap.index then raise Push_in_full_heap;
  heap.array.(heap.index) <- elt;
  heap.index <- heap.index + 1;
  let i = ref (heap.index - 1) in
  while !i > 0 && heap.gt heap.array.((!i - 1) / 2) heap.array.(!i) do
    swap heap ((!i - 1) / 2) !i;
    i := (!i - 1) / 2
  done
