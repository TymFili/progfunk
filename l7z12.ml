module type RandomMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t
end

module Shuffle(R : RandomMonad) : sig
  val shuffle : 'a list -> 'a list R.t
end = struct
  let (let* ) = R.bind

  let pick n xs =
    let rec pick_aux n (xs, ys) =
      match xs with
      | [] -> failwith "n too big"
      | x :: xs -> if n = 0 then (x, xs @ ys) else pick_aux (n - 1) (xs, x :: ys)
  in (pick_aux n (xs, []))

  let rec shuffle xs =
    match xs with
    | [] -> R.return []
    | _ :: _ ->
      let m = List.length xs in
      let* r = R.random in
      let n = ((r mod m) + m) mod m in
      let (y, ys) = (pick n xs) in
      let* zs = shuffle ys in
      R.return (y :: zs)
end

module RS : sig include RandomMonad val run : int -> 'a t -> 'a
end = struct
    type 'a t = int -> 'a * int
    let return x = fun s -> (x, s)
    let bind m f = fun s -> let x = m n in (f x n, s)
    let run n = fun m ->
end

module S = Shuffle(RS)
let result = RS.run 12345 (S.shuffle [1;2;3;4;5;6])

(* let b = 16807 * (a mod 127773) - 2836 * (a / 127773) in let a = if b > 0 then b else b + 2147483647 *)
