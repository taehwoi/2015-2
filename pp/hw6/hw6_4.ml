exception TODO

type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec eval (f: formula): bool =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT f -> not (eval f)
  | ANDALSO (f0, f1) -> (eval f0) && (eval f1)
  | ORELSE (f0, f1) -> (eval f0) || (eval f1)
  | IMPLY (f0, f1) ->
      if (eval f0) = true
      then (eval f1)
      else true
  | LESS (e0, e1) ->
      (calcul e0) < (calcul e1)
  and calcul (e: expr): int =
  match e with
  | NUM n -> n
  | PLUS (n0, n1) ->  (calcul n0) + (calcul n1)
  | MINUS (n0, n1) -> (calcul n0) - (calcul n1) 
