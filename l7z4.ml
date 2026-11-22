module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
end

module Err : sig
  include Monad

  val fail  : 'a t
  val catch : 'a t -> (unit -> 'a t) -> 'a t

  val run : 'a t -> 'a option
end = struct
  type 'r ans = 'r option
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans }

  let return x = { run = fun k -> k x }

  let bind m f = {
    run = fun k -> m.run (fun x -> (f x).run k)
  }

  let fail = { run = fun _ -> None }

  let catch m h = {
    run = fun k ->
      match m.run (fun x -> Some (k x)) with
      | None     -> (h ()).run k
      | Some res -> res
  }

  let run m = m.run Some
end

module BT : sig
  include Monad

  val fail : 'a t
  val flip : bool t

  val run : 'a t -> 'a Seq.t
end = struct
  type 'r ans = 'r Seq.t
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans }

  let return x = { run = fun k -> k x }

  let bind m f = {
    run = fun k -> m.run (fun x -> (f x).run k)
  }

  let fail = { run = fun _ -> Seq.empty }

  let flip = {
    run = fun k -> Seq.append (k true) (k false)
  }

  let run m = m.run Seq.singleton
end

module St(State : sig type t end) : sig
  include Monad

  val get : State.t t
  val set : State.t -> unit t

  val run : State.t -> 'a t -> 'a
end = struct
  type 'r ans = State.t -> 'r * State.t
  type 'a t = { run : 'r. ('a -> 'r ans) -> 'r ans }

  let return x = { run = fun k -> k x }

  let bind m f = {
    run = fun k -> m.run (fun x -> (f x).run k)
  }

  let get = { run = fun k s -> k s s }

  let set s' = { run = fun k _s -> k () s' }

  let run s m = fst (m.run (fun x st -> (x, st)) s)
end
