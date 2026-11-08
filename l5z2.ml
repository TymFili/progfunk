type 'a fix = Fix of ('a fix -> 'a)

let fix (Fix f) = f (Fix f)

let f_fix (Fix g) x =
  if x = 0 then 0
  else x + g (Fix g) (x - 1)

let f = fix (Fix f_fix)

let fix_mut f =
  let f' = ref (fun _ -> failwith "not initialised") in
  let g x = !(f') x in
  f' := f g; !f'

let g_fix f x =
  if x = 0 then 0
  else x + f (x - 1)

let g = fix_mut (g_fix)
