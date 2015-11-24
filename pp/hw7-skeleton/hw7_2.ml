module type SKI = sig
  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)
  val react: liquid -> liquid
  val pprint: liquid -> string
end

module SkiLiquid : SKI = struct
  exception ETODO
  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)

  let rec react: liquid -> liquid =
    let rec helper (l: liquid) : liquid =
      match l with
      | M (I, e) -> e
      | M ((M (K, e0)), e1) -> e0
      | M ((M ((M (S, e0)), e1)), e2) -> (M ((M (e0, e2)), (M (e1, e2))))
      | M (l0, l1) -> M ((helper l0), (helper l1))
      | _ -> l in
    fun l ->
      if l = helper l 
      then l
      else react (helper l)

  let rec pprint: liquid -> string =
    fun l ->
      match l with
      | S -> "S"
      | K -> "K"
      | I -> "I"
      | V var -> var
      | M (l0,l1) -> "("^(pprint l0)^" "^(pprint l1)^")" 

let _ = print_endline (pprint (react 
  (M (M ((M (S,K)), I), V "y"))
))
let _ = print_endline (pprint (react 
  (M (M ((M (S,K)), I), K))
))
let _ = print_endline (pprint (react 
  (M (M ((M (S,V "K")), V "B"), V "ILL"))
))
let _ = print_endline (pprint (react 
  (M (M (M (M (I,K), K), (M (S,S))), (M (V "I", (M (S, S))))))
))
let _ = print_endline (pprint (react 
   (M ((M ((M (S, (M (I,I)))), I)), (M (M (I,I), (M (V "up",V "down"))))))
))

let _ = print_endline (pprint (react 
   (M ((M ((M ( (M ((I, (M (K, V "I"))))), (M ((M (S, I)), I) ))), V "SEOUL")), V "U"))
))


end
