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

let out_of_bounds (n:int): bool =
  ((n < -5) || (n > 5))

let rec exeval (p:pgm) (st:state): state list =
  if out_of_bounds st then
    [] else 
    match p with
    | ASSIGN exp -> [exp_eval exp st]
    | CHOICE (c0,c1) -> exeval c0 st @ exeval c1 st
    | EQ (exp,c) -> 
        if st = exp_eval exp st then
          exeval c st else
            [st]
    | NEQ (exp,c) -> 
        if st = exp_eval exp st then
          [st] else
            exeval c st
    | SEQUENCE (c0,c1) -> 
        let new_st_l = exeval c0 st in
        (List.concat (List.map (fun x -> (exeval c1 x)) new_st_l))
    | REPEAT cmd -> 
        (*let repeat *)
        if out_of_bounds st then
          []
        else
        let new_st_l = exeval cmd st in
        (*let new_new_st_l = (List.fold_left (fun x a -> (exeval cmd a) @ x) [] new_st_l) in*)
        (*let n_n_n_st_l = (List.fold_left (fun x a -> (exeval cmd a) @ x) [] new_new_st_l) in*)
        (*let nnnn_st_l = (List.fold_left (fun x a -> (exeval cmd a) @ x) [] n_n_n_st_l) in*)
        [st] @ new_st_l
        (*st :: exeval p (st+1)*)
        (*@ new_new_st_l *)
        (*@ n_n_n_st_l *)
        (*@ nnnn_st_l*)
and exp_eval (e:exp) (var:state): int =
  match e with
    | NUM n -> 
        if out_of_bounds n then
          var else
          n
    | ADD (n0,n1) -> 
        let a = (exp_eval n0 var) + (exp_eval n1 var) in
        if out_of_bounds a then
          var else
          a
    | SUB (n0,n1) ->
        let a = (exp_eval n0 var) - (exp_eval n1 var) in
        if out_of_bounds a then
          var else
          a
    | VAR -> var



(*test clause*)

let p0 =
  SEQUENCE (ASSIGN (NUM 2), ASSIGN (NUM 0)
  )
let p1 = 
  SEQUENCE (ASSIGN (NUM 1),
	    REPEAT
	      (CHOICE (EQ (NUM 1, ASSIGN (ADD (VAR,NUM 1))),
		       NEQ (NUM 1, ASSIGN (SUB (VAR,NUM 1))))
	   ))
let p2 = REPEAT (ASSIGN (ADD (VAR,NUM 1)))
let p4 = (REPEAT (SEQUENCE ((CHOICE ((ASSIGN (ADD (VAR, NUM 1))), (ASSIGN (SUB (VAR, NUM 1))))), (ASSIGN (ADD (VAR, VAR))))))
let _ = print_endline (String.concat " " (List.map string_of_int 
  (List.sort_uniq compare (exeval p4 (2)))
))

(*let _ = print_endline (String.concat " " (List.map string_of_int 
  (*(exeval p1 (-1))*)
))*)
(*let _ = print_endline (String.concat " " (List.map string_of_int
  (*(exeval p2 1)*)
))*)
(*let concat = List.fold_left (fun a x -> a ^ x) "" ["a";"b"]*)
(*let _ = print_endline (String.concat " " (["a"]@["b"]))*)
(*let _ = print_endline (String.concat " " (["a"]@["b"]))*)
(*(List.fold_left (fun x a -> (exeval cmd1 a) @ x) [] new_st_list) *)
