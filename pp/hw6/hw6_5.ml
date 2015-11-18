exception TODO

type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff (e: ae) (x: string): ae =
  match e with
  | CONST n -> CONST 0
  | VAR v -> 
      if  v = x
      then CONST 1
      else CONST 0
  | POWER (v,n) -> 
      if v = x
      then TIMES [CONST n;POWER (v,n-1)]
      else CONST 0
  | TIMES v -> 
      (match v with
      | [] -> CONST 0
      | h::t -> (*ae::ae_list*)
          SUM [TIMES ((diff h x):: t) ; TIMES [h;(diff (TIMES t) x)] ]
          )
  | SUM v -> SUM (List.map (fun y -> diff y x) v)
