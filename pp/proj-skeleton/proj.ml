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
  (*| CLOS_MEM of ... *)
  | PAIR of (value_t * value_t)
  | MPAIR of (value_t * value_t)
  | VOID	       
exception EXCEPTION_HANDLER of value_t

let rev = function
  (a, b) -> (b, a)

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

let debug exp exp_string =
  let _ = print_string "input: " in
  let _ = print_endline exp_string in
  let _ = print_string "exp: " in
  print_endline (exp_to_string exp)

let rec myeval (exp_string: string): value_t =
  let lexbuf = Lexing.from_string exp_string in
  let lexer () = Lexer.token lexbuf in
  let exp = Parser.parse lexer in

  let _ = debug exp exp_string in
  let env = [] in let hndl_env = [] in

  try
    (eval exp env hndl_env)
  with EXCEPTION_HANDLER a -> a

and eval (exp: exp_t) env hndl: value_t =
    match exp with
    | CONST (CINT n) -> INT n
    | CONST (CTRUE) -> BOOL true
    | CONST (CFALSE) -> BOOL false
    | CONST (CNULL) -> VOID
    | VAR v -> (look_up v env)
    | ADD (e0, e1) -> (binary_eval ('+', (eval e0 env hndl), (eval e1 env hndl)))
    | SUB (e0, e1) -> (binary_eval ('-', (eval e0 env hndl), (eval e1 env hndl)))
    | MUL (e0, e1) -> (binary_eval ('*', (eval e0 env hndl), (eval e1 env hndl)))
    | EQ (e0, e1) -> (binary_eval ('=', (eval e0 env hndl), (eval e1 env hndl)))
    | LT (e0, e1) -> (binary_eval ('<', (eval e0 env hndl), (eval e1 env hndl)))
    | GT (e0, e1) -> (binary_eval ('>', (eval e0 env hndl), (eval e1 env hndl)))
    | CONS (e0, e1) -> (binary_eval ('p', (eval e0 env hndl), (eval e1 env hndl)))
    | MCONS (e0, e1) -> (binary_eval ('m', (eval e0 env hndl), (eval e1 env hndl)))
    | CAR p -> 
        (match (eval p env hndl) with
        | PAIR (el, _) -> el
        | _ ->  raise (RUNTIME_EXCEPTION "pair expected"))
    | CDR p -> 
        (match (eval p env hndl) with
        | PAIR (_, el) -> el
        | _ ->  raise (RUNTIME_EXCEPTION "pair expected"))
    | MCAR p -> 
        (match (eval p env hndl) with
        | MPAIR (el, _) -> el
        | _ ->  raise (RUNTIME_EXCEPTION "mutable pair expected"))
    | MCDR p -> 
        (match (eval p env hndl) with
        | MPAIR (_, el) -> el
        | _ ->  raise (RUNTIME_EXCEPTION "mutable pair expected"))
    | SETMCAR (VAR v, el_new) ->
        (match (look_up v env) with
        | MPAIR (_, el)  -> 
            let _ = (set_var v (MPAIR ((eval el_new env hndl), el)) env) in VOID
        | _ ->  raise (RUNTIME_EXCEPTION "mutable pair expected"))
    | SETMCDR (VAR v, el_new) ->
        (match (look_up v env) with
        | MPAIR (el, _)  -> 
            let _ = (set_var v (MPAIR (el, (eval el_new env hndl))) env) in VOID
        | _ ->  raise (RUNTIME_EXCEPTION "mutable pair expected"))
    | SETMCDR (MCONS (_, _), _) | SETMCAR (MCONS (_, _), _)->
        VOID
    | IF (b, e0, e1) -> 
        (match (eval b env hndl) with
        | BOOL true -> (eval e0 env hndl)
        | BOOL false -> (eval e1 env hndl)
        | _ ->  raise (RUNTIME_EXCEPTION "boolean expected"))
    | LET (blist, e) -> 
        (let ht = Hashtbl.create (List.length blist) in
        let _ = List.iter (fun (v, e) -> Hashtbl.add ht v (eval e env hndl)) blist in
        (eval e (ht::env) hndl))
    | LETREC (blist, e) -> 
        (let ht = Hashtbl.create (List.length blist) in
        let _ = List.iter (fun (v, e) -> Hashtbl.add ht v (eval e (ht::env) hndl)) blist in
        (eval e (ht::env) hndl))
    | APP (e, elist) ->
        begin
          let ht = Hashtbl.create (List.length elist) in
          match e with 
          | LAMBDA (vlist, e) -> 
              let _ = List.iter2 
                (fun v e -> Hashtbl.add ht v (eval e env hndl)) vlist elist in
              (eval e (ht::env) hndl)
          | VAR x -> 
              let f = (look_up x env) in
              (match f with
              | CLOS (vlist, e, en) -> 
                  let _ = List.iter2
                    (fun v e -> Hashtbl.add ht v (eval e env hndl)) vlist elist in
                  (eval e (ht::en) hndl)
              | _ -> raise (RUNTIME_EXCEPTION "procedure expected"))
          | _ -> raise (RUNTIME_EXCEPTION "procedure expected")
        end
    | RAISE e ->  (*lookup handlers environment*)
        (exception_handler e env hndl)
    | HANDLERS (hdl_list, e) ->
        (*FIXME: change order to avoid side effect?*)
        let h_list = List.map (fun (p, e) -> (rev ((eval e env hndl), (eval p env hndl)))) hdl_list in
        (eval e env h_list)
    | LAMBDA (vlist, e) -> CLOS (vlist, e, env)
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
  | '+', _, _ ->  raise (RUNTIME_EXCEPTION "addition of non-ints not allowed" ) 
  | '-', _, _ ->  raise (RUNTIME_EXCEPTION "subtraction of non-ints not allowed" ) 
  | '*', _, _ ->  raise (RUNTIME_EXCEPTION "multiplication of non-ints not allowed" ) 
  | ('=' | '<' | '>'), _, _ ->  raise (RUNTIME_EXCEPTION "comparison of non-ints not allowed" ) 
  | _ -> raise NOT_IMPLEMENTED

and look_up v env =
  match env with 
  | [] -> raise (RUNTIME_EXCEPTION "variable undefined")
  | ht::tl -> 
      if (Hashtbl.mem ht v) then
        Hashtbl.find ht v
      else
        look_up v tl

and set_var v value env =
  match env with 
  | [] -> raise (RUNTIME_EXCEPTION "variable undefined")
  | ht::tl -> 
      if (Hashtbl.mem ht v) then
        Hashtbl.replace ht v value
      else
        set_var v value tl

and exception_handler e env hndls =
  let ht = Hashtbl.create 1 in
  match hndls with 
  | [] -> raise UNCAUGHT_EXCEPTION
  | ((CLOS (v0, e0, en0)), (CLOS (v1, e1, en1)))::tl ->
      let _ = List.iter (fun v -> Hashtbl.add ht v (eval e env hndls)) v0 in
      if ( (eval e0 (ht::en0) hndls) = BOOL true) then
        raise (EXCEPTION_HANDLER (eval e1 (ht::en1) hndls))
      else
        exception_handler e env tl
  | _ ->  raise (RUNTIME_EXCEPTION "handlers should have procedures")




(*let myeval_memo (exp_string: string): value_t =*)

  (*test like this: *)
(*let exp1 = "(let ((x (lambda (x) (+ x 1)))) ((lambda (x) (+ x 1)) 3))"*)
(*let exp1 = "(letrec ((f (lambda (x) (if (= x 0) 0 (+ x (f (- x 1)))) ))) (f 100))"*)
(*let exp1 = "(letrec ((f (lambda (x) (if (= x 0) 0 (+ x (f (- x 1))))) )) (f 999999))"*)
let exp1 ="(let ((a 3))
          (with-handlers (((lambda (x) (= x 5)) (lambda (x) a))) (let ((a 5)) (raise (+ 3 3)))))"
let v = myeval exp1
let _ = print_endline (value_to_string v)
