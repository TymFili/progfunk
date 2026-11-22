type symbol = string
type 'v term =
  | Var of 'v
  | Sym of symbol * 'v term list

let return v = Var v

let rec bind t f =
  match t with
  | Var v -> f v
  | Sym (s, xs) -> Sym (s, List.map (fun x -> bind x f) xs)

let f x =
  match x with
  | 'a' -> Var 2
  | 'b' -> Var 3
  | _ -> failwith ""

let term = Sym ("f", [Sym ("g", [Var 'a']); Sym ("f", [Var 'b'; Var 'a'])])
