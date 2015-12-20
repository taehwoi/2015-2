open Syntax
open Lexer
open Hashtbl

exception RUNTIME_EXCEPTION of string
exception NOT_IMPLEMENTED
exception UNCAUGHT_EXCEPTION

(* this is for testing the lexer and the parser *)

type env_t = 
  (Syntax.var_t , value_t) t list
and value_t = 
  | INT of int
  | BOOL of bool
  | NULL
  | CLOS of (var_t list * exp_t * env_t)
  (*| CLOS_MEM of (var_t list * exp_t * env_t)*)
  | PAIR of (value_t * value_t)
  | MPAIR of (value_t ref * value_t ref)
  | VOID
  | UNDEF (*internal usage*)
exception EXCEPTION_HANDLER of value_t

(* If value_to_string does not work well for your code, *)
(*  adjust this function manually to make it work *)
(* Content of mpair is hidden when printing *)
let rec value_to_string (v:value_t): string =
  match v with
  | INT n -> string_of_int n
  | BOOL b -> if b then "#t" else "#f"
  | NULL -> "'()"
  | CLOS _ -> "#<procedure>"
  (*| CLOS_MEM _ -> "#<procedure-memo>"*)
  | PAIR (a, b) -> "(" ^ (value_to_string a) ^ " . " ^ (value_to_string b) ^ ")"
  | MPAIR (a, b) -> "(mcons ? ?)"
  | VOID -> "#<void>"
  | UNDEF -> "'UNDEF"

let e_msg_need = "expected"
let e_msg_need_pair = "pair " ^ e_msg_need
let e_msg_need_mpair = "mutable " ^ e_msg_need_pair
let e_msg_need_bool = "boolean " ^ e_msg_need
let e_msg_need_proc = "procedure " ^ e_msg_need
let e_msg_non_int = " of non-ints not allowed"
let e_msg_undef = "variable undefined"
let e_msg_operand = "Number of operands doesn't match"

let rev_3 = function
  (a, b, c) -> (c, b, a)

let cnt = ref 0
let slow = ref false
let debug exp exp_string =
  let _ = print_string "input: " in
  let _ = print_endline exp_string in
  let _ = print_string "exp: " in
  print_endline (exp_to_string exp)

let rec myeval (exp_string: string): value_t =
  let lexbuf = Lexing.from_string exp_string in
  let lexer () = Lexer.token lexbuf in
  let exp = Parser.parse lexer in
  (*let _ = debug exp exp_string in*)

  let env = [] in 
  let hndl_env = [] in
  let table = Hashtbl.create 100 in

  let ret = (eval exp env hndl_env false table true) in
  let _ = Hashtbl.reset table in ret

and eval (exp: exp_t) env hndl to_mem tbl to_mut: value_t =
    match exp with
    | CONST (CINT n) -> INT n
    | CONST (CTRUE) -> BOOL true
    | CONST (CFALSE) -> BOOL false
    | CONST (CNULL) -> NULL
    | VAR v -> (look_up v env)
    | ADD (e0, e1) ->
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl to_mut), (eval e0 env hndl to_mem tbl to_mut), '+')))
    | SUB (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl to_mut), (eval e0 env hndl to_mem tbl to_mut), '-')))
    | MUL (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl to_mut), (eval e0 env hndl to_mem tbl to_mut), '*')))
    | EQ (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl to_mut), (eval e0 env hndl to_mem tbl to_mut), '=')))
    | LT (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl to_mut), (eval e0 env hndl to_mem tbl to_mut), '<')))
    | GT (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl to_mut), (eval e0 env hndl to_mem tbl to_mut), '>')))
    | CONS (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl to_mut), (eval e0 env hndl to_mem tbl to_mut), 'p')))
    | MCONS (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl to_mut), (eval e0 env hndl to_mem tbl to_mut), 'm')))
    | CAR p -> 
        begin match (eval p env hndl to_mem tbl to_mut) with
        | PAIR (el, _) -> el
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_pair)
        end
    | CDR p -> 
        begin match (eval p env hndl to_mem tbl to_mut) with
        | PAIR (_, el) -> el
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_pair)
        end
    | MCAR p -> 
        begin match (eval p env hndl to_mem tbl to_mut) with
        | MPAIR (el, _) -> !el
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end
    | MCDR p -> 
        begin match (eval p env hndl to_mem tbl to_mut) with
        | MPAIR (_, er) -> !er
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end
    | SETMCAR (e, el_new) ->
        if (to_mut = false) then
          VOID
        else
        (begin match (eval e env hndl to_mem tbl to_mut) with
        | MPAIR (el, _)  ->
            let _ = el:= (eval el_new env hndl to_mem tbl to_mut) in
            VOID
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end)
    | SETMCDR (e, er_new) ->
        if (to_mut = false) then
          VOID
        else
        (begin match (eval e env hndl to_mem tbl to_mut) with
        | MPAIR (_, er)  ->
            let _ = er:= (eval er_new env hndl to_mem tbl to_mut) in
            VOID
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end)
    | IF (b, e0, e1) -> 
        begin match (eval b env hndl to_mem tbl to_mut) with
        | BOOL true -> (eval e0 env hndl to_mem tbl to_mut)
        | BOOL false -> (eval e1 env hndl to_mem tbl to_mut)
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_bool)
        end
    | LET (blist, exp) -> 
          let ht = Hashtbl.create (List.length blist) in
          let add_to_env = 
            (fun (v, e) -> 
              if (Hashtbl.mem ht v ) && (Hashtbl.find ht v <> UNDEF) then
                raise (RUNTIME_EXCEPTION "already defined")
              else
                Hashtbl.add ht v (eval e env hndl to_mem tbl to_mut)) in
          let _ = List.iter add_to_env blist in
          (eval exp (ht::env) hndl to_mem tbl to_mut)
    | LETREC (blist, exp) -> 
          let ht = Hashtbl.create (List.length blist) in
          let _ = List.iter (fun (v, _) -> Hashtbl.add ht v UNDEF) blist in
          let add_to_env_rec = 
            (fun (v, e) -> 
              if (Hashtbl.mem ht v ) && (Hashtbl.find ht v <> UNDEF) then
                raise (RUNTIME_EXCEPTION "already defined")
              else
                Hashtbl.add ht v (eval e (ht::env) hndl to_mem tbl to_mut)) in
          let _ = List.iter add_to_env_rec blist in
          (eval exp (ht::env) hndl to_mem tbl to_mut)
    (*FIXME: memoize lambda also?*)
    | APP (LAMBDA (vlist, exp), elist) ->
        if has_dup vlist then
          raise (RUNTIME_EXCEPTION "duplicate argument name")
        else
        (let ht = Hashtbl.create (List.length elist) in
        let add_to_env = 
          (fun v e ->
            Hashtbl.add ht v (eval e env hndl to_mem tbl to_mut)) in
        let _ = 
          try (let l = List.iter2 add_to_env vlist elist in l) with 
          | Invalid_argument "List.iter2" -> 
              raise (RUNTIME_EXCEPTION e_msg_operand) in
        (eval exp (ht::env) hndl to_mem tbl to_mut) )
    | APP (e, elist) ->
        let ht = Hashtbl.create (List.length elist) in
        let f = (eval e env hndl to_mem tbl to_mut) in
        let add_to_env =
          (fun v e -> Hashtbl.add ht v (eval e env hndl to_mem tbl to_mut)) in
        begin match (f, to_mem) with
        | CLOS (vlist, exp, en), false -> 
            let _ = 
              try (let l = List.iter2 add_to_env vlist elist in l) with 
              | Invalid_argument "List.iter2" -> 
                  raise (RUNTIME_EXCEPTION e_msg_operand) in
            (eval exp (ht::en) hndl to_mem tbl to_mut)
        | CLOS (vlist, exp, en), true -> 
            let _ = 
              try (let l = List.iter2 add_to_env vlist elist in l) with 
              | Invalid_argument "List.iter2" -> 
                  raise (RUNTIME_EXCEPTION e_msg_operand) in
            let arg_list = List.map 
              (fun e -> (eval e env hndl true tbl to_mut)) elist in
            let f_memo = (f, arg_list) in
            if (Hashtbl.mem tbl f_memo) then
              (Hashtbl.find tbl f_memo)
            else
              let _ = 
                (Hashtbl.add tbl f_memo (eval exp (ht::en) hndl true tbl to_mut)) in
              (Hashtbl.find tbl f_memo)
        | _ -> raise (RUNTIME_EXCEPTION e_msg_need_proc)
        end
    | RAISE excptn ->  (*lookup handlers environment*)
        (exception_handler excptn env hndl to_mem tbl to_mut)
    | HANDLERS (hdl_list, exp) ->
        begin
          try 
            let add_to_hndl_env = 
              (fun (p, e) -> 
                ((eval p env hndl to_mem tbl to_mut), (eval e env hndl to_mem tbl to_mut))) in
            let h_list = 
              List.map add_to_hndl_env hdl_list in
            (eval exp env (h_list::hndl) to_mem tbl to_mut) 
          with EXCEPTION_HANDLER a -> a
        end
    | LAMBDA (vlist, exp) ->
        if has_dup vlist then
          raise (RUNTIME_EXCEPTION "duplicate argument name")
        else
          CLOS (vlist, exp, env) 


and binary_eval exp =
  match exp with
  | ('+', (INT a), (INT b)) ->  INT (a + b) 
  | ('-', (INT a), (INT b)) ->  INT (a - b) 
  | ('*', (INT a), (INT b)) ->  INT (a * b) 
  | ('=', (INT a), (INT b)) ->  BOOL (a = b) 
  | ('<', (INT a), (INT b)) ->  BOOL (a < b) 
  | ('>', (INT a), (INT b)) ->  BOOL (a > b) 
  | ('p', a, b) ->  PAIR (a , b) 
  | ('m', a, b) ->  MPAIR (ref a , ref b) 
  | '+', _, _ ->  
      raise (RUNTIME_EXCEPTION ("addition" ^ e_msg_non_int ) )
  | '-', _, _ ->  
      raise (RUNTIME_EXCEPTION ("subtraction" ^ e_msg_non_int))
  | '*', _, _ ->  
      raise (RUNTIME_EXCEPTION ("multiplication" ^ e_msg_non_int))
  | ('=' | '<' | '>'), _, _ ->  
      raise (RUNTIME_EXCEPTION ("comparison" ^ e_msg_non_int) )
  | _ -> raise NOT_IMPLEMENTED

and look_up v env =
  match env with 
  | [] -> raise (RUNTIME_EXCEPTION e_msg_undef)
  | ht::tl -> 
      if (Hashtbl.mem ht v) then
        let value = Hashtbl.find ht v in
        (match value with
        | UNDEF -> raise (RUNTIME_EXCEPTION e_msg_undef)
        | _ -> value)
      else
        look_up v tl

(*raise appropriate exception to be caught at the top*)
and exception_handler excptn env hndls to_mem tbl  to_mut=
  let ht = Hashtbl.create 1 in
  match hndls with 
  | [] -> raise UNCAUGHT_EXCEPTION
  | []::tl -> (exception_handler excptn env tl to_mem tbl to_mut)
  | ((((CLOS ([v], e0, en)), (CLOS ([x], e1, _))))::tail) :: tl ->
      (*closures in handlers only have one parameter*)
      let _ = Hashtbl.add ht v (eval excptn env hndls to_mem tbl to_mut) in
      if ( (eval e0 (ht::en) tl to_mem tbl to_mut) = BOOL true) then
        raise (EXCEPTION_HANDLER (eval e1 (ht::en) tl to_mem tbl to_mut))
      else
        exception_handler excptn env (tail::tl) to_mem tbl to_mut
  | _ ->  raise (RUNTIME_EXCEPTION "with-handlers expect proc. with 1 argument")

and has_dup l =
  match l with
  | [] -> false
  | ht::tl ->
      (List.mem ht tl) || (has_dup tl)


let rec myeval_memo (exp_string: string): value_t =
  let lexbuf = Lexing.from_string exp_string in
  let lexer () = Lexer.token lexbuf in
  let exp = Parser.parse lexer in

  let table = Hashtbl.create 100 in
  (*let _ = debug exp exp_string in*)

  if (can_memo exp) then (*check for purity*)
    (eval exp [] [] true table true)
  else
    (eval exp [] [] false table true)

(*checks if a function is referentially transparent*)
and can_memo exp : bool =
  let table = Hashtbl.create 100 in
  let _ = cnt:= 0 in
  let _ = slow:= false in
  let _ = 
    try (eval_cnt exp [] [])
    with 
    | (RUNTIME_EXCEPTION "too deep") -> 
        let _ = slow:= true in UNDEF in

  (*0th net: if function can be executed in reasonable time, no need to memoize*)
  if (!slow = false) then
    false
  (*1st net: a function can be memoized if everything is pure*)
  else if (all_pure exp) then
    true
  (*2nd net: a function can be memoized if mutables don't matter*)
  else if  
      begin
      (*compare the results of doing mset!, and not doing it (whilst memoizing) *)
        let a = (eval exp [] [] true table true) in
        let _ = Hashtbl.reset table in
        let b = 
          begin
            try (let c = (eval exp [] [] true table false) in c) with
            | (RUNTIME_EXCEPTION a) -> UNDEF
            | Stack_overflow -> UNDEF
          end in
        a=b
      end
  then true
  (*don't memoize*)
  else
    false

and all_pure exp : bool =
  match exp with
  | CONST _ -> true
  | VAR _ -> true
  | SETMCAR (MCONS _, e) | SETMCDR (MCONS _, e) -> true
  | SETMCAR (_, e) | SETMCDR (_, e) -> false
  | ADD (e0, e1) -> (all_pure e0) && (all_pure e1)
  | SUB (e0, e1) -> (all_pure e0) && (all_pure e1)
  | MUL (e0, e1) -> (all_pure e0) && (all_pure e1)
  | EQ (e0, e1) -> (all_pure e0) && (all_pure e1)
  | LT (e0, e1) -> (all_pure e0) && (all_pure e1)
  | GT (e0, e1) -> (all_pure e0) && (all_pure e1)
  | CONS (e0, e1) -> (all_pure e0) && (all_pure e1)
  | MCONS (e0, e1) -> (all_pure e0) && (all_pure e1)
  | CAR p -> (all_pure p)
  | CDR p -> (all_pure p)
  | MCAR p -> (all_pure p)
  | MCDR p -> (all_pure p)
  | IF (b, e0, e1) -> (all_pure b) && (all_pure e0) && (all_pure e1)
  | LET (b_list, exp) | LETREC (b_list, exp) -> 
      (all_pure exp) &&
      (List.fold_left (fun x (_, e) -> x && (all_pure e)) true b_list)
  | APP (e, elist) ->
      (all_pure e) && 
      (List.fold_left (fun x y -> x && (all_pure y)) true elist)
  | RAISE exc -> (all_pure exc)
  | LAMBDA (_, exp) -> (all_pure exp)
  | HANDLERS (hdl_list, exp) -> 
      (all_pure exp) && 
      (List.fold_left (fun x (e0, e1) -> x && (all_pure e0) && (all_pure e1)) true hdl_list)

and eval_cnt (exp: exp_t) env hndl : value_t =
  if !cnt > 300000 then
    raise (RUNTIME_EXCEPTION "too deep")
  else
  (match exp with
  | CONST (CINT n) -> INT n
  | CONST (CTRUE) -> BOOL true
  | CONST (CFALSE) -> BOOL false
  | CONST (CNULL) -> NULL
  | VAR v -> (look_up v env)
  | ADD (e0, e1) ->
      (binary_eval
        (rev_3 ((eval_cnt e1 env hndl ), (eval_cnt e0 env hndl ), '+')))
  | SUB (e0, e1) -> 
      (binary_eval
        (rev_3 ((eval_cnt e1 env hndl ), (eval_cnt e0 env hndl ), '-')))
  | MUL (e0, e1) -> 
      (binary_eval
        (rev_3 ((eval_cnt e1 env hndl ), (eval_cnt e0 env hndl ), '*')))
  | EQ (e0, e1) -> 
      (binary_eval
        (rev_3 ((eval_cnt e1 env hndl ), (eval_cnt e0 env hndl ), '=')))
  | LT (e0, e1) -> 
      (binary_eval
        (rev_3 ((eval_cnt e1 env hndl ), (eval_cnt e0 env hndl ), '<')))
  | GT (e0, e1) -> 
      (binary_eval
        (rev_3 ((eval_cnt e1 env hndl ), (eval_cnt e0 env hndl ), '>')))
  | CONS (e0, e1) -> 
      (binary_eval
        (rev_3 ((eval_cnt e1 env hndl ), (eval_cnt e0 env hndl ), 'p')))
  | MCONS (e0, e1) -> 
      (binary_eval
        (rev_3 ((eval_cnt e1 env hndl ), (eval_cnt e0 env hndl ), 'm')))
  | CAR p -> 
      begin match (eval_cnt p env hndl) with
      | PAIR (el, _) -> el
      | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_pair)
      end
  | CDR p -> 
      begin match (eval_cnt p env hndl) with
      | PAIR (_, el) -> el
      | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_pair)
      end
  | MCAR p -> 
      begin match (eval_cnt p env hndl ) with
      | MPAIR (el, _) -> !el
      | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
      end
  | MCDR p -> 
      begin match (eval_cnt p env hndl ) with
      | MPAIR (_, er) -> !er
      | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
      end
  | SETMCAR (e, el_new) ->
      begin match (eval_cnt e env hndl) with
      | MPAIR (el, _)  ->
          let _ = el:= (eval_cnt el_new env hndl ) in
          VOID
      | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
      end
  | SETMCDR (e, er_new) ->
      begin match (eval_cnt e env hndl ) with
      | MPAIR (_, er)  ->
          let _ = er:= (eval_cnt er_new env hndl ) in
          VOID
      | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
      end
  | IF (b, e0, e1) -> 
      begin match (eval_cnt b env hndl ) with
      | BOOL true -> (eval_cnt e0 env hndl)
      | BOOL false -> (eval_cnt e1 env hndl )
      | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_bool)
      end
  | LET (blist, exp) -> 
        let ht = Hashtbl.create (List.length blist) in
        let add_to_env = 
          (fun (v, e) -> 
            if (Hashtbl.mem ht v ) && (Hashtbl.find ht v <> UNDEF) then
              raise (RUNTIME_EXCEPTION "already defined")
            else
              Hashtbl.add ht v (eval_cnt e env hndl )) in
        let _ = List.iter add_to_env blist in
        (eval_cnt exp (ht::env) hndl )
  | LETREC (blist, exp) -> 
        let ht = Hashtbl.create (List.length blist) in
        let _ = List.iter (fun (v, _) -> Hashtbl.add ht v UNDEF) blist in
        let add_to_env_rec = 
          (fun (v, e) -> 
            if (Hashtbl.mem ht v ) && (Hashtbl.find ht v <> UNDEF) then
              raise (RUNTIME_EXCEPTION "already defined")
            else
              Hashtbl.add ht v (eval_cnt e (ht::env) hndl )) in
        let _ = List.iter add_to_env_rec blist in
        (eval_cnt exp (ht::env) hndl )
  (*FIXME: memoize lambda also?*)
  | APP (LAMBDA (vlist, exp), elist) ->
      let _ = (cnt := (!cnt + 1)) in
      if has_dup vlist then
        raise (RUNTIME_EXCEPTION "duplicate argument name")
      else
      (let ht = Hashtbl.create (List.length elist) in
      let add_to_env = 
        (fun v e ->
          Hashtbl.add ht v (eval_cnt e env hndl )) in
      let _ = 
        try (let l = List.iter2 add_to_env vlist elist in l) with 
        | Invalid_argument "List.iter2" -> 
            raise (RUNTIME_EXCEPTION e_msg_operand) in
      (eval_cnt exp (ht::env) hndl ) )
  | APP (e, elist) ->
      let _ = (cnt := (!cnt + 1)) in
      let ht = Hashtbl.create (List.length elist) in
      let f = (eval_cnt e env hndl ) in
      let add_to_env =
        (fun v e -> Hashtbl.add ht v (eval_cnt e env hndl )) in
      begin match f with
      | CLOS (vlist, exp, en) -> 
          let _ = 
            try (let l = List.iter2 add_to_env vlist elist in l) with 
            | Invalid_argument "List.iter2" -> 
                raise (RUNTIME_EXCEPTION e_msg_operand) in
          (eval_cnt exp (ht::en) hndl )
      | _ -> raise (RUNTIME_EXCEPTION e_msg_need_proc)
      end
  | RAISE excptn ->  (*lookup handlers environment*)
      (dummy_exn_handler excptn env hndl)
  | HANDLERS (hdl_list, exp) ->
      begin
        try 
          let add_to_hndl_env = 
            (fun (p, e) -> 
              ((eval_cnt p env hndl ), (eval_cnt e env hndl ))) in
          let h_list = 
            List.map add_to_hndl_env hdl_list in
          (eval_cnt exp env (h_list::hndl)) 
        with EXCEPTION_HANDLER a -> a
      end
  | LAMBDA (vlist, exp) ->
      if has_dup vlist then
        raise (RUNTIME_EXCEPTION "duplicate argument name")
      else
        CLOS (vlist, exp, env))

and dummy_exn_handler excptn env hndls=
  let ht = Hashtbl.create 1 in
  match hndls with 
  | [] -> raise UNCAUGHT_EXCEPTION
  | []::tl -> (dummy_exn_handler excptn env tl )
  | ((((CLOS ([v], e0, en)), (CLOS ([x], e1, _))))::tail) :: tl ->
      (*closures in handlers only have one parameter*)
      let _ = Hashtbl.add ht v (eval_cnt excptn env hndls) in
      if ( (eval_cnt e0 (ht::en) tl) = BOOL true) then
        raise (EXCEPTION_HANDLER (eval_cnt e1 (ht::en) tl))
      else
        dummy_exn_handler excptn env (tail::tl)
  | _ ->  raise (RUNTIME_EXCEPTION "with-handlers expect proc. with 1 argument")
