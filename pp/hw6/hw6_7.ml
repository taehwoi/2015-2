type pgm = cmd
and cmd = ASSIGN of exp
 | SEQUENCE of cmd * cmd
 | REPEAT of cmd
 | CHOICE of cmd * cmd
 | EQ of exp * cmd
 | NEQ of exp * cmd
and exp = NUM of int
 | ADD of exp * exp
 | SUB of exp * exp
 | VAR

type state = int
type fin = bool
let print_list lst = print_string "["; List.iter (fun k -> Printf.printf " %d " k) lst; print_string "]\n"; flush stdout;; 

let out_of_bounds (n:int): bool =
  ((n < -5) || (n > 5))

let fst (a,_) = a
let rst (_,a) = a

let rec exeval (p:pgm) (st:state): state list =
  let rec exp_eval (e:exp) (st:state): state =
    match e with
    | NUM v -> v
    | ADD (e1, e2) ->
      exp_eval e1 st + exp_eval e2 st 
    | SUB (e1, e2) ->
      exp_eval e1 st - exp_eval e2 st
    | VAR -> st 
  and helper (e:pgm) (st:state): ((fin * state) list) =
    match e with
    | ASSIGN exp -> 
      let new_st = exp_eval exp st in
      if out_of_bounds new_st then
        [(true,st)]
      else [(false, new_st)]
    | CHOICE (c0, c1) ->
        let a = helper c0 st in
        let b = helper c1 st in
        a @ b
    | SEQUENCE (c0, c1) ->
        let new_st_l = helper c0 st in
        let rec a (st:state) (l: (fin * state) list): (fin * state) list = 
          match l with
          | [] -> []
          | (f,s)::t -> 
              if f then
              [(f,s)] else
              (helper c1 s) @ (a st t) in (List.sort_uniq compare (a st new_st_l))
    | REPEAT c -> 
        let rec rep (st:state) (n:int): ((fin * state) list) =
          if (n>300 || (rep_N c st n) = (rep_N c st (n+1))) then
            (List.sort_uniq compare (rep_N c st n)@(helper c st)) else
            (List.sort_uniq compare (rep_N c st n)@(rep st (n + 1))) in
        (List.sort_uniq compare (rep st 0))
    | EQ (exp, c) -> 
        if st = exp_eval exp st then
          helper c st else
            [(false,st)]
    | NEQ (exp, c) -> 
        if st = exp_eval exp st then
            [(false,st)]else
          helper c st 
and rep_N (c:cmd) (var:state) (n:int) : ((fin * state) list)=
  match n with
  | 0 -> [(false,var)]
  | _ -> (List.sort_uniq compare 
  (List.concat (List.map 
  (fun x ->
    if true=(fst x) then
      [true,(rst x)] else
      (helper c (rst x)))
  (rep_N c var (n - 1))))) in

 (List.sort_uniq compare (List.map (fun x -> rst x) (helper p st)))

let prg1 = SEQUENCE (ASSIGN (NUM 1), REPEAT ( CHOICE (EQ (NUM 1, ASSIGN (ADD (VAR, NUM 1) )), (NEQ (NUM 1, ASSIGN (SUB (VAR, NUM 1) ) ) ))));; 
print_list (exeval prg1 0);; (* [1 2] *) 

let prg2 = SEQUENCE (ASSIGN (NUM 1), REPEAT (ASSIGN (ADD (VAR, NUM 1))) );;     (* x=1; (x=x+1); *) 
print_list (exeval prg2 0);; (* [1 2 3 4 5] *) 

let prg3 = SEQUENCE ( ASSIGN (NUM 1),   REPEAT (SEQUENCE( EQ ((NUM (-5)), ASSIGN (NUM 5)),      ASSIGN (SUB (VAR, NUM 1)) )));; 
print_list (exeval prg3 0);;

let prg4 = SEQUENCE( ASSIGN (NUM 6), REPEAT( ASSIGN (ADD (VAR, NUM 1))) )       ;; 
print_list (exeval prg4 1);; (* [1] *) 

let prg5 = REPEAT( ASSIGN (NUM 3) ) ;; 
print_list (exeval prg5 2);; (* [2 3] *) 

let prg6 = CHOICE( CHOICE( REPEAT (ASSIGN (ADD (VAR, VAR))), ASSIGN (NUM (-2))), ASSIGN (NUM (-5)) );;
print_list (exeval prg6 1);;    (* [-5 -2 1 2 4] *) 

let prg7 = REPEAT (CHOICE( ASSIGN ( ADD (VAR, NUM 1)), ASSIGN ( SUB (VAR, NUM 1) ) ));; 
print_list (exeval prg7 4);;

let prg8 = SEQUENCE(EQ( NUM 3, prg5), prg6);; 
print_list (exeval prg8 0);;    (* [-5 -2 0] *) 

let prg9 = SEQUENCE( CHOICE( 
        CHOICE(ASSIGN (NUM 1), ASSIGN(ADD (NUM 3, VAR))), 
        CHOICE(ASSIGN (NUM (-1)), ASSIGN(NUM (-4)) ) 
), ASSIGN (NUM (-4)));; 
print_list (exeval prg9 5);;    (* [-4 5] *) 
print_list (exeval prg9 2);;    (* [-4] *) 
let prg10 = SEQUENCE( ASSIGN (NUM 2), CHOICE( REPEAT ( NEQ (ADD (VAR, NUM 1), prg5) ), ASSIGN (NUM 4)) );; 
print_list (exeval prg10 0);;   (* [2 3 4] *) 

let prg11 = CHOICE( CHOICE(prg4, prg1), REPEAT( CHOICE(prg6, prg10) ) );; 
print_list (exeval prg11 0);;   (* [-5 -4 -2 0 1 2 3 4] *) 

let _ = print_list (exeval (SEQUENCE(ASSIGN (ADD ((NUM 4), VAR)), ASSIGN (ADD((NUM 3), VAR)))) 1)
let _ = print_list (exeval (SEQUENCE(ASSIGN (ADD ((NUM 4), VAR)), ASSIGN (ADD((NUM 3), VAR)))) 2)
let _ = print_list (exeval (CHOICE(ASSIGN (ADD ((NUM 6), VAR)), ASSIGN (ADD((NUM 1), VAR)))) 0)

