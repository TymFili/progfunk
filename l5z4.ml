type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data =
  { prev : 'a dllist
  ; elem : 'a
  ; next : 'a dllist
  }

let prev d = (Lazy.force d).prev
let elem d = (Lazy.force d).elem
let next d = (Lazy.force d).next

let of_list lst =
  match lst with
  | [] -> invalid_arg "empty list"
  | _ ->
      let arr = Array.of_list lst in
      let n = Array.length arr in
      let rec dll_arr =
        Array.init n (fun i -> lazy {
          prev = dll_arr.((i - 1 + n) mod n);
          elem = arr.(i);
          next = dll_arr.((i + 1) mod n);
        } ) in dll_arr.(0)
