type 'a my_lazy = 'a state ref
and 'a state =
  | Unevaluated of (unit -> 'a)
  | Evaluating
  | Evaluated   of 'a

let force x = match !x with
  | Unevaluated f -> x := Evaluating; let y = f () in x := Evaluated y; y
  | Evaluating -> failwith "Sins of the mutable state"
  | Evaluated x -> x

let rec fix f = let rec box = ref (Unevaluated (fun () -> f box)) in box

type 'a llist = 'a llist_data my_lazy
and 'a llist_data =
  | Nil
  | Cons of 'a * 'a llist

let rec from n =
  fix (fun _self -> Cons (n, from (n + 1)))

let rec filter p lst =
  fix (fun _self ->
    let rec loop l =
      match force l with
      | Nil -> Nil
      | Cons (x, xs) ->
          if p x then Cons (x, filter p xs)
          else loop xs
    in
    loop lst
  )

let rec sieve lst =
  fix (fun _self ->
    match force lst with
    | Nil -> Nil
    | Cons (p, xs) ->
        Cons (p, sieve (filter (fun x -> x mod p <> 0) xs))
  )

let primes = sieve (from 2)

let rec take n l =
  if n <= 0 then []
  else match force l with
  | Nil -> []
  | Cons (x, xs) -> x :: take (n - 1) xs
