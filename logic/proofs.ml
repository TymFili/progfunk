let p = Var "p";;
let t = by_assumption p;;
let thm1 = imp_i p t;;

let p = Var "p";;
let q = Var "q";;
let t = by_assumption p;;
let t = imp_i q t;;
let thm2 = imp_i p t;;

let p = Var "p";;
let q = Var "q";;
let r = Var "r";;
let t1 = by_assumption (Imp (p, Imp (q, r)));;
let t2 = by_assumption p;;
let t3 = by_assumption (Imp (p, q));;
let t1 = imp_e t1 t2;;
let t2 = imp_e t3 t2;;
let t = imp_e t1 t2;;
let t = imp_i p t;;
let t = imp_i (Imp (p, q)) t ;;
let thm3 = imp_i (Imp (p, (Imp (q, r)))) t;;

let p = Var "p";;
let t = by_assumption Bot;;
let t = bot_e p t;;
let thm4 = imp_i Bot t;;

(* Zauważmy, że te cztery twierdzenia gwarantują pełność naszego systemu dowodowego (bo to już system zawierający reguły intuicjonistycznego systemu Hilberta). *)
