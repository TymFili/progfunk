type 'a heap =
  | Leaf
  | Node of int * 'a heap * 'a * 'a heap;;

let rank = function
  | Leaf -> 0
  | Node (r, _, _, _) -> r;;

let make_node x h1 h2 =
  if rank h1 >= rank h2 then
    Node (rank h2 + 1, x, h1, h2)
  else
    Node (rank h1 + 1, x, h2, h1);;

let rec merge h1 h2 = match h1, h2 with
  | Leaf, h -> h
  | h, Leaf -> h
  | Node (_, x1, l1, r1), Node (_, x2, l2, r2) ->
      if x1 <= x2 then
        make_node x1 l1 (merge r1 h2)
      else
        make_node x2 l2 (merge h1 r2);;

let insert x h = merge (make_node x Leaf Leaf) h;;

let delete_min = function
  | Leaf -> failwith "Empty heap"
  | Node (_, x, l, r) -> (x, merge l r);;
