module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type t
  val apply : t -> key -> key
  val id : t
  val invert : t -> t
  val swap : key -> key -> t
  val compose : t -> t -> t
  val compare: t -> t -> int
end

module Make(Key : OrderedType) : S with type key = Key.t = struct
  type key = Key.t

  module KeyMap = Map.Make(Key)

  type t = key KeyMap.t * key KeyMap.t

  let id = (KeyMap.empty, KeyMap.empty)

  let apply (p, _) k = match KeyMap.find_opt k p with
    | None -> k
    | Some pk -> pk

  let invert (x, y) = (y, x)

  let swap k1 k2 = if Key.compare k1 k2 = 0 then id else let p = KeyMap.add k2 k1 (KeyMap.add k1 k2 KeyMap.empty) in (p, p)

  let compose (p, p') (q, q') = (* compose s t odpowiada t \circ s *)
    let f (s, s') k x y = match x with
    | None -> y
    | Some z -> let w = apply (s, s') z in if Key.compare w k = 0 then None else Some w
    in (KeyMap.merge (f (q, q')) p q, KeyMap.merge (f (p', p)) q' p')

  let compare (p1, _) (p2, _) = KeyMap.compare Key.compare p1 p2
end

