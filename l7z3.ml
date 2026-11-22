type 'a t = 'a

let return x = x

let bind x f = f x

type 'a s = unit -> 'a

let return x = fun () -> x

let bind x f () = f (x ()) ()
