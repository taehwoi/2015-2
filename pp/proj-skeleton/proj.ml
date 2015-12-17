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
  | MPAIR of (value_t * value_t)
  | VOID	       
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

let e_msg_need = "expected"
let e_msg_need_pair = "pair" ^ e_msg_need
let e_msg_need_mpair = "mutable" ^ e_msg_need_pair
let e_msg_need_bool = "boolean" ^ e_msg_need
let e_msg_need_proc = "procedure" ^ e_msg_need
let e_msg_non_int = " of non-ints not allowed"
let e_msg_undef = "variable undefined"

let debug exp exp_string =
  let _ = print_string "input: " in
  let _ = print_endline exp_string in
  let _ = print_string "exp: " in
  print_endline (exp_to_string exp)

let rec myeval (exp_string: string): value_t =
  let lexbuf = Lexing.from_string exp_string in
  let lexer () = Lexer.token lexbuf in
  let exp = Parser.parse lexer in

  let env = [] in 
  let hndl_env = [] in
  let table = Hashtbl.create 100 in

  try (eval exp env hndl_env 0 table) with 
  | EXCEPTION_HANDLER a -> a

and eval (exp: exp_t) env hndl m tbl: value_t =
    match exp with
    | CONST (CINT n) -> INT n
    | CONST (CTRUE) -> BOOL true
    | CONST (CFALSE) -> BOOL false
    | CONST (CNULL) -> VOID
    | VAR v -> (look_up v env)
    | ADD (e0, e1) -> 
        (binary_eval ('+', (eval e0 env hndl m tbl), (eval e1 env hndl m tbl)))
    | SUB (e0, e1) -> 
        (binary_eval ('-', (eval e0 env hndl m tbl), (eval e1 env hndl m tbl)))
    | MUL (e0, e1) -> 
        (binary_eval ('*', (eval e0 env hndl m tbl), (eval e1 env hndl m tbl)))
    | EQ (e0, e1) -> 
        (binary_eval ('=', (eval e0 env hndl m tbl), (eval e1 env hndl m tbl)))
    | LT (e0, e1) -> 
        (binary_eval ('<', (eval e0 env hndl m tbl), (eval e1 env hndl m tbl)))
    | GT (e0, e1) -> 
        (binary_eval ('>', (eval e0 env hndl m tbl), (eval e1 env hndl m tbl)))
    | CONS (e0, e1) -> 
        (binary_eval ('p', (eval e0 env hndl m tbl), (eval e1 env hndl m tbl)))
    | MCONS (e0, e1) -> 
        (binary_eval ('m', (eval e0 env hndl m tbl), (eval e1 env hndl m tbl)))
    | CAR p -> 
        begin match (eval p env hndl m tbl) with
        | PAIR (el, _) -> el
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_pair)
        end
    | CDR p -> 
        begin match (eval p env hndl m tbl) with
        | PAIR (_, el) -> el
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_pair)
        end
    | MCAR p -> 
        begin match (eval p env hndl m tbl) with
        | MPAIR (el, _) -> el
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end
    | MCDR p -> 
        begin match (eval p env hndl m tbl) with
        | MPAIR (_, el) -> el
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end
    | SETMCAR (VAR v, el_new) ->
        begin match (look_up v env) with
        | MPAIR (_, el)  -> 
            let _ = 
              (set_var v (MPAIR ((eval el_new env hndl m tbl), el)) env) in 
            VOID
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end
    | SETMCDR (VAR v, el_new) ->
        begin match (look_up v env) with
        | MPAIR (el, _)  -> 
            let _ = 
              (set_var v (MPAIR (el, (eval el_new env hndl m tbl))) env) in 
            VOID
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end
    | SETMCDR (MCONS _, _) | SETMCAR (MCONS _, _) -> VOID
    | IF (b, e0, e1) -> 
        begin match (eval b env hndl m tbl) with
        | BOOL true -> (eval e0 env hndl m tbl)
        | BOOL false -> (eval e1 env hndl m tbl)
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_bool)
        end
    | LET (blist, exp) -> 
          let ht = Hashtbl.create (List.length blist) in
          let add_to_env = 
            (fun (v, e) -> Hashtbl.add ht v (eval e env hndl m tbl)) in
          let _ = List.iter add_to_env blist in
          (eval exp (ht::env) hndl m tbl)
    | LETREC (blist, exp) -> 
          let ht = Hashtbl.create (List.length blist) in
          let add_to_env_rec = 
            (fun (v, e) -> Hashtbl.add ht v (eval e (ht::env) hndl m tbl)) in
          let _ = List.iter add_to_env_rec blist in
          (eval exp (ht::env) hndl m tbl)
    | APP (LAMBDA (vlist, exp), elist) ->
        let ht = Hashtbl.create (List.length elist) in
        let add_to_env = 
          (fun v e -> Hashtbl.add ht v (eval e env hndl m tbl)) in
        let _ = List.iter2 add_to_env vlist elist in
        (eval exp (ht::env) hndl m tbl)
    | APP (VAR x, elist) ->
        let ht = Hashtbl.create (List.length elist) in
        let f = (look_up x env) in
        let add_to_env =
          (fun v e -> Hashtbl.add ht v (eval e env hndl m tbl)) in
        begin match (f, m) with
        | CLOS (vlist, exp, en), 0 -> 
            let _ = List.iter2 add_to_env vlist elist in
            (eval exp (ht::en) hndl m tbl)
        (*FIXME*)
        | CLOS (vlist, exp, en), 1 -> 
            let _ = List.iter2 add_to_env vlist elist in
            let arg_list = List.map 
              (fun e -> (eval e env hndl 1 tbl)) elist in
            let f_memo = (x, arg_list) in
            if (Hashtbl.mem tbl f_memo) then
              (Hashtbl.find tbl f_memo)
            else
              let _ = 
                (Hashtbl.add tbl f_memo (eval exp (ht::en) hndl 1 tbl)) in
              (Hashtbl.find tbl f_memo)
        | _ -> raise (RUNTIME_EXCEPTION e_msg_need_proc)
        end
    | RAISE excp ->  (*lookup handlers environment*)
        (exception_handler excp env hndl m tbl)
    | HANDLERS (hdl_list, exp) ->
        let add_to_hndl_env = 
          (fun (p, e) -> ((eval p env hndl m tbl), (eval e env hndl m tbl))) in
        let h_list = 
          List.map add_to_hndl_env hdl_list in
        (eval exp env h_list m tbl)
    | LAMBDA (vlist, exp) -> CLOS (vlist, exp, env)
    | APP (_, elist) ->
        raise (RUNTIME_EXCEPTION e_msg_need_proc)
    | _ -> raise NOT_IMPLEMENTED

and binary_eval exp =
  match exp with
  | ('+', (INT a), (INT b)) ->  INT (a + b) 
  | ('-', (INT a), (INT b)) ->  INT (a - b) 
  | ('*', (INT a), (INT b)) ->  INT (a * b) 
  | ('=', (INT a), (INT b)) ->  BOOL (a = b) 
  | ('<', (INT a), (INT b)) ->  BOOL (a < b) 
  | ('>', (INT a), (INT b)) ->  BOOL (a > b) 
  | ('p', a, b) ->  PAIR (a , b) 
  | ('m', a, b) ->  MPAIR (a , b) 
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
        Hashtbl.find ht v
      else
        look_up v tl

and set_var v value env =
  match env with 
  | [] -> raise (RUNTIME_EXCEPTION e_msg_undef)
  | ht::tl -> 
      if (Hashtbl.mem ht v) then
        Hashtbl.replace ht v value
      else
        set_var v value tl

(*raise appropriate exception to be caught at the top*)
and exception_handler e env hndls m tbl=
  let ht = Hashtbl.create 1 in
  match hndls with 
  | [] -> raise UNCAUGHT_EXCEPTION
  | ((CLOS ([v], e0, en)), (CLOS (_, e1, _))) :: tl ->
      (*closures in handlers only have one parameter*)
      let _ = Hashtbl.add ht v (eval e env hndls m tbl) in
      if ( (eval e0 (ht::en) hndls m tbl) = BOOL true) then
        raise (EXCEPTION_HANDLER (eval e1 (ht::en) hndls m tbl))
      else
        exception_handler e env tl m tbl
  | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_proc)


let rec myeval_memo (exp_string: string): value_t =
  let lexbuf = Lexing.from_string exp_string in
  let lexer () = Lexer.token lexbuf in
  let exp = Parser.parse lexer in

  let table = Hashtbl.create 100 in
  let _ = debug exp exp_string in

  try
  (if (pure exp) then  (*check for purity*)
    (eval exp [] [] 1 table)
  else
    (myeval exp_string)) with
  | EXCEPTION_HANDLER a -> a

(*TODO*)
and pure (exp) : bool =
  true


  (*test like this: *)
(*let exp1 = "((lambda (x y z) (+ x y)) 3 4 5)"*)
(*let exp1 = "(let ((f (lambda (x y) (+ x y)))) (f 3 4))"*)
(*let exp1 = "(f 1 2 (+ x 1))"*)
(*let exp1 = "(letrec ((f (lambda (x n) (if (= x 0) n (f (- x 1) (+ n x)) )))) (f 99 0))"*)
(*let exp1 = "(let ((x (mcons 3 5))) x)"*)
(*let exp1 = "((lambda (x y z w) (+ x y)) (+ 1 1) 4 5 6)"*)
let exp1 = "(letrec ((fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))) ))) )) (fib 80))"
let v = myeval_memo exp1
let _ = print_endline (value_to_string v)
