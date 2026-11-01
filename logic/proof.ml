open Logic

type ptree =
  | Goal of (string * formula) list * formula
  | Thm of theorem
  | ImpI of string * ptree
  | ImpE of ptree * ptree
  | BotE of ptree

type frame =
  | CImpI of string
  | CImpELeft of ptree
  | CImpERight of ptree
  | CBotE

type context = frame list

type ptree =
  | Complete of theorem
  | Incomplete of goal * context

let proof g f =
  Incomplete ((g, f), [])

let qed pf =
  match pf with
  | Complete thm -> thm
  | Incomplete _ -> failwith "proof incomplete"

let goal pf =
  match pf with
  | Complete _ -> None
  | Incomplete (g, _) -> Some g

let rec first_goal pt ctx =
  match pt with
  | Goal g -> Some (g, ctx)
  | Thm _ -> None
  | ImpI (name, body) ->
      first_goal body (CImpI name :: ctx)
  | ImpE (left, right) ->
      (match first_goal left (CImpELeft right :: ctx) with
       | Some res -> Some res
       | None -> first_goal right (CImpERight left :: ctx))
  | BotE body ->
      first_goal body (CBotE :: ctx)

let rec next_goal ctx current_ptree =
  match ctx with
  | [] ->
      (* Korzeń *)
      first_goal current_ptree []
  | CImpELeft right :: rest ->
      (match first_goal right (CImpERight current_ptree :: rest) with
       | Some res -> Some res
       | None -> next_goal rest (ImpE (current_ptree, right)))
  | CImpERight left :: rest ->
      next_goal rest (ImpE (left, current_ptree))
  | CImpI name :: rest ->
      next_goal rest (ImpI (name, current_ptree))
  | CBotE :: rest ->
      next_goal rest (BotE current_ptree)

let next pf =
  match pf with
  | Complete _ -> pf
  | Incomplete (g, ctx) ->
      (match next_goal ctx (Goal g) with
       | Some (g', ctx') -> Incomplete (g', ctx')
       | None -> pf)

let intro name pf =
  match pf with
  | Complete _ -> failwith "proof complete"
  | Incomplete ((g, f), ctx) ->
      match f with
      | Imp (psi, phi) ->
          let new_goal = ((name, psi) :: g, phi) in
          Incomplete (new_goal, CImpI name :: ctx)
      | _ -> failwith "intro: goal not an implication" (* Tutaj "_" jest uzasadnione *)

let apply f pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_thm thm pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let apply_assm name pf =
  (* TODO: zaimplementuj *)
  failwith "not implemented"

let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()
