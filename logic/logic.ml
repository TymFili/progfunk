type formula =
  | Bot
  | Var of string
  | Imp of formula * formula

let rec string_of_formula f = match f with
  | Bot -> "⊥"
  | Var s -> s
  | Imp (phi, psi) -> match phi with
    | Bot -> string_of_formula phi ^ " → " ^ string_of_formula psi
    | Var _ -> string_of_formula phi ^ " → " ^ string_of_formula psi
    | Imp _ -> "(" ^ string_of_formula phi ^ ") → " ^ string_of_formula psi

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem = formula list * formula

let assumptions (gamma, _) = gamma

let consequence (_, phi) = phi

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f = ([f], f)

let imp_i f (gamma, phi) = (List.filter (fun x -> x <> f) gamma, Imp (f, phi))

let imp_e (gamma, xi) (delta, phi) = match xi with
  | Bot -> failwith "wrong proof"
  | Var _ -> failwith "wrong proof"
  | Imp (xi, psi) -> if xi = phi then (List.sort_uniq compare (gamma @ delta), psi) else failwith "wrong proof"

let bot_e f (gamma, phi) = match phi with
  | Bot -> (gamma, f)
  | Var _ -> failwith "wrong proof"
  | Imp _ -> failwith "wrong proof"
