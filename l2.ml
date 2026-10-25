let length1 = List.fold_left (fun x -> fun _ -> x + 1) 0;;
let rev1 = List.fold_left (fun xs x -> x :: xs) [];;
let map1 f xs = List.fold_right (fun x xs -> f x :: xs) xs [];;
let append1 = List.fold_right (fun x xs -> x :: xs);;
let rev_append1 xs ys = List.fold_left (fun ys x -> x :: ys) ys xs;;
let filter1 p xs = List.fold_right (fun x xs -> if p x then x :: xs else xs) xs [];;
let rev_map1 f = List.fold_left (fun acc x -> f x :: acc) [];;

let sublists xs = List.fold_right (fun x xs -> List.fold_right (fun y ys -> (x :: y) :: y :: ys) xs []) xs [[]];;
let sublists1 xs = List.fold_right (fun x xs -> List.fold_right (fun y ys -> (x :: y) :: ys) xs xs) xs [[]];;

let suffixes xs = List.fold_right (fun x xs -> (x :: (List.hd xs)) :: xs) xs [[]];;

let rec merge cmp xs ys = match xs, ys with
  | xs, [] -> xs
  | [], ys -> ys
  | x :: xs, y :: ys -> if (cmp x y) then x :: (merge cmp xs (y :: ys)) else y :: (merge cmp (x :: xs) ys);;

let merge_tr cmp xs ys =
  let rec merge_aux xs ys zs = match xs, ys with
    | [], [] -> zs
    | x :: xs, [] -> merge_aux xs ys (x :: zs)
    | [], y :: ys -> merge_aux xs ys (y :: zs)
    | x :: xs, y :: ys -> if (cmp x y) then merge_aux xs (y :: ys) (x :: zs) else merge_aux (x :: xs) ys (y :: zs)
  in merge_aux xs ys [];;

let halve xs =
  let rec halve_aux xs ys zs = match xs with
    | x :: y :: xs -> halve_aux xs (x :: ys) (y :: zs)
    | x :: [] -> x :: ys, zs
    | [] -> ys, zs
  in halve_aux xs [] []

let rec mergesort cmp xs =
  let cmp' x y = cmp y x in
  let ys, zs = halve xs in
  if ys = [] || zs = [] then xs else merge_tr cmp' (mergesort cmp' ys) (mergesort cmp' zs);;

let g x y z = List.rev_append y (x :: z);;

let f x xs = List.map2 (g x) ((List.rev xs) |> suffixes |> List.rev) (suffixes xs);;

let permutations = List.fold_left (fun xs x -> List.concat_map (fun perm -> f x perm) xs) [[]];;
