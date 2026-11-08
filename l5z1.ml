let rec fix f x = f (fix f) x

let rec fix_with_limit n f x =
  if n > 0 then f (fix_with_limit (n - 1) f) x
  else failwith "Recursion limit exceeded"

let fix_memo (f : ('a -> 'b) -> 'a -> 'b) : 'a -> 'b =
  let memo = Hashtbl.create 64 in
  let rec fix_memo_aux f x =
    match Hashtbl.find_opt memo x with
    | None -> let y = (f (fix_memo_aux f) x) in Hashtbl.add memo x y; y
    | Some y -> y
  in fix_memo_aux f

let fib_f fib n =
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)

let fib1 = fix fib_f
let fib2 = fix_with_limit 10 fib_f
let fib3 = fix_memo fib_f
