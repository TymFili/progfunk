type 'a regexp =
  | Eps
  | Lit of ('a -> bool)
  | Or of 'a regexp * 'a regexp
  | Cat of 'a regexp * 'a regexp
  | Star of 'a regexp

let ( +% ) r1 r2 = Or (r1, r2)
let ( *% ) r1 r2 = Cat (r1, r2)

let ( let* ) = BT.bind

let rec match_regexp (r : 'a regexp) (w : 'a list) : 'a list option BT.t =
  match r with
  | Eps ->
      BT.return None

  | Lit p ->
      (match w with
       | x :: xs when p x -> BT.return (Some xs)
       | _ -> BT.fail)

  | Or (r1, r2) ->
      let* choice = BT.flip in
      if choice then match_regexp r1 w else match_regexp r2 w

  | Cat (r1, r2) ->
      let* res1 = match_regexp r1 w in
      (match res1 with
       | None ->
           match_regexp r2 w
       | Some rest1 ->
           let* res2 = match_regexp r2 rest1 in
           BT.return (match res2 with
             | None -> Some rest1
             | Some _ as rest2 -> rest2))

  | Star r ->
      let* continue_star = BT.flip in
      if not continue_star then
        BT.return None
      else
        let* res = match_regexp r w in
        (match res with
         | None ->
             BT.return None
         | Some rest ->
             let* res' = match_regexp (Star r) rest in
             BT.return (Some (Option.value res' ~default:rest)))
