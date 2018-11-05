type 'a queue = 
    | Leaf
    | Node of 'a queue * 'a * int * 'a queue

let empty = Leaf

let lewicowosc q = 
    match q with 
    | Leaf -> 0
    | Node (_, _, g, _) -> g

let attach q1 v q2 =
    let g1, g2 = lewicowosc q1, lewicowosc q2 in
    if g1 < g2 then Node (q2, v, g1 + 1, q1)
    else Node (q1, v, g2 + 1, q2)

let rec join q1 q2 =
    match q1, q2 with
    | Leaf, q -> q
    | q, Leaf -> q
    | Node (l1, v1, _, r1), Node (_, v2, _, _) ->
        if v1 > v2 then join q2 q1
        else attach l1 v1 (join r1 q2)


let add w q = join q (Node (Leaf, w, 1, Leaf))

exception Empty

let delete_min q = 
    match q with
    | Leaf -> raise Empty
    | Node (l, v, _, r) -> (v, join l r)

let is_empty q = 
    q = Leaf
