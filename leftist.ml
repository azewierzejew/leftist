(* Autor: Antoni Żewierżejew *)
(* Reviewer: Jagoda Kamińska *)


(* typ złączalnej kolejki priorytetowej                                  *)
(* prawa wysokość to długość ścieżki do skrajnie prawego liścia          *)
(* struktura Node to (lewy syn, korzeń, prawa wysokość, prawy syn)       *)
(* niezminikiem kolejki jest lewicowość drzewa dla każdego wierzchołka   *)
(* lewicowość oznacza, że prawa wysokość to najkrótsza ścieżka do liścia *)
type 'a queue = 
    | Leaf
    | Node of 'a queue * 'a * int * 'a queue

(* empty to pusta kolejka priorytetowa *)
let empty = Leaf

(* right_height zwraca prawą wysokość drzewa *)
let right_height q = 
    match q with 
    | Leaf -> 0
    | Node (_, _, g, _) -> g

(* attach do korzenia v przypina q1, q2 jako synów zachowując lewicowość *)
let attach q1 v q2 =
    let g1, g2 = right_height q1, right_height q2 in
    if g1 < g2 then Node (q2, v, g1 + 1, q1)
    else Node (q1, v, g2 + 1, q2)

(* join łączy kolejki q1, q2 *)
let rec join q1 q2 =
    match q1, q2 with
    | Leaf, q -> q
    | q, Leaf -> q
    | Node (l1, v1, _, r1), Node (_, v2, _, _) ->
        if v1 > v2 then join q2 q1
        else attach l1 v1 (join r1 q2)


(* add do kolejki q dodaje element w *)
let add w q = join q (Node (Leaf, w, 1, Leaf))

(* Empty to wyjątek zwracany przy próbie usuwania z pustej kolejki *)
exception Empty

(* delete_min podnosi wyjątek Empty jeśli kolejka q jest pusta, w.p.p. *)
(* zwraca parę (pierwszy element q, q z usuniętym pierwszym elementem) *)
let delete_min q = 
    match q with
    | Leaf -> raise Empty
    | Node (l, v, _, r) -> (v, join l r)

(* zwraca czy kolejka q jest pusta *)
let is_empty q = 
    q = Leaf
