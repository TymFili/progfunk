type 'a zlist = 'a list * 'a list

let of_list xs = ([], xs)

let to_list (xs, ys) = List.rev_append xs ys

let elem (xs, ys) = match ys with
  | [] -> None
  | x :: _ -> Some x

let move_left (xs, ys) = match xs with
  | [] -> (xs, ys) 
  | x :: xs -> (xs, x :: ys)

let move_right (xs, ys) = match ys with
  | [] -> (xs, ys)
  | y :: ys -> (y :: xs, ys)

let insert x (xs, ys) = (x :: xs, ys)

let remove (xs, ys) = match xs with
  | [] -> (xs, ys)
  | x :: xs -> (xs, ys)
