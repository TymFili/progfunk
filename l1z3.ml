let hd = fun s -> (s 0);;
let tl = fun s -> (fun x -> s (x + 1));;
let add = fun s x -> (fun y -> (s y) + x);;
let map = fun s f -> (fun x -> (f (s x)));;
let map2 = fun f s1 s2 -> (fun x -> (f (s1 x) (s2 x)));;
let replace s n t = fun x -> if x = n then t else (s x);;
let take_every s n = fun x -> (s (x * n));;

let rec scan f a s = fun x -> if x = 0 then (f a (s 0)) else f ((scan f a s) (x - 1)) (s x);;

let rec tabulate s ?(x=0) y = if x > y then [] else (s x) :: (tabulate s ~x:(x + 1) y);;

let cfalse = fun x -> fun y -> y;;
let ctrue = fun x -> fun y -> x;;

let cand = fun x -> fun y -> fun t -> fun f -> y (x t f) f;;
let cor = fun x -> fun y -> fun t -> fun f -> y t (x t f);;
let cbool_of_bool x = if x = false then cfalse else ctrue;;
let bool_of_cbool x = x true false;;

let zero = fun f -> fun x -> x;;
let succ = fun n -> fun f -> fun x -> f (n f x);;
let add = fun m -> fun n -> fun f -> fun x -> n f (m f x);;
let mul = fun m -> fun n -> fun f -> fun x -> n (fun x -> m f x) x;;
