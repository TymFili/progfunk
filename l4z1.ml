(* "ra" znaczy "random access", prosze Panstwa. *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

type 'a digit =
  | Zero
  | One of 'a tree
  | Two of 'a tree * 'a tree

type 'a ralist = 'a digit list

let rec ins_tree t digits = match digits with
  | [] -> [One t]
  | Zero :: ts -> One t :: ts
  | One t1 :: ts -> Two (t, t1) :: ts
  | Two (t1, t2) :: ts -> One t :: ins_tree (Node (t1, t2)) ts

let cons x ral = ins_tree (Leaf x) ral

let rec rm_tree digits = match digits with
  | [] -> failwith "pred 0"
  | Zero :: ts -> match rm_tree ts with
    | (Leaf _, _) -> assert false
    | (Node (t1, t2), ts') -> (t1, One t2 :: ts')
  | One t :: ts -> match ts with
    | [] -> (t, [])
    | _ :: _ -> (t, Zero :: ts)
  | Two (t1, t2) :: ts -> (t1, One t2 :: ts)

let hd ral = match rm_tree ral with
  | (Leaf x, _) -> x
  | (Node _, _) -> assert false

let tl ral = snd (rm_tree ral)

let rec tree_lookup size i t = match t with
  | Leaf x -> if i = 0 then x else failwith "out of bounds"
  | Node (t1, t2) -> let half = size / 2 in 
    if i < half then tree_lookup half i t1 
    else tree_lookup half (i - half) t2

let lookup i ral =
  let rec lookup_aux i ral weight = match ral with
    | [] -> failwith "out of bounds"
    | Zero :: ts -> lookup_aux i ts (2 * weight)
    | One t :: ts -> 
      if i < weight then tree_lookup weight i t 
      else lookup_aux (i - weight) ts (2 * weight)
    | Two (t1, t2) :: ts -> 
      if i < weight then tree_lookup weight i t1 
      else if i < 2 * weight then tree_lookup weight (i - weight) t2
      else lookup_aux (i - 2 * weight) ts (2 * weight)
  in lookup_aux i ral 1
