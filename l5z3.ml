type 'a lazy_tree = Node of 'a lazy_tree Lazy.t * 'a * 'a lazy_tree Lazy.t

type rational = int * int

let mix ((a, b) : rational) ((c, d) : rational) : rational = (a + c, b + d)

let rec tree p q =
  let m = mix p q in
  Node (lazy (tree p m), m, lazy (tree m q))

let rec unwind n t =
  if n = 0 then ()
  else match t with
  | Node (l, _, r) ->
    unwind (n - 1) (Lazy.force l);
    unwind (n - 1) (Lazy.force r);
    ()

let rec stream_of_tree tree () =
  match tree with
  | Node (l, x, r) -> Seq.Cons (x, Seq.interleave (stream_of_tree (Lazy.force l)) (stream_of_tree (Lazy.force r)))

let s = stream_of_tree (tree (0, 1) (1, 0))
