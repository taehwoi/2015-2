open Syntax
open Lexer
open Hashtbl

exception RUNTIME_EXCEPTION of string
exception NOT_IMPLEMENTED
exception UNCAUGHT_EXCEPTION

(* this is for testing the lexer and the parser *)

type value_t = 
  | INT of int
  | BOOL of bool
  | NULL
  (*| CLOS of ...*)
  (*| CLOS_MEM of ... *)
  | PAIR of (value_t * value_t)
  (*| MPAIR of ...*)
  | VOID	       

(*type env_t = *)
  (*Hashtbl of (var_t * exp_t) list*)

(* If value_to_string does not work well for your code, *)
(*  adjust this function manually to make it work *)
(* Content of mpair is hidden when printing *)
let rec value_to_string (v:value_t): string =
  match v with
  | INT n -> string_of_int n
  | BOOL b -> if b then "#t" else "#f"
  | NULL -> "'()"
  (*| CLOS _ -> "#<procedure>"*)
  (*| CLOS_MEM _ -> "#<procedure-memo>"*)
  | PAIR (a, b) -> "(" ^ (value_to_string a) ^ " . " ^ (value_to_string b) ^ ")"
  (*| MPAIR (a, b) -> "(mcons ? ?)"*)
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

  (eval exp []) (*empty list -> empty env*)

and eval (exp: exp_t) env: value_t =
    match exp with
    | CONST (CINT n) -> INT n
    | CONST (CTRUE) -> BOOL true
    | CONST (CFALSE) -> BOOL false
    | CONST (CNULL) -> VOID
    | VAR v -> (look_up v env) (*FIXME: use look_up env*)
    | ADD (e0, e1) -> (binary_eval ('+', (eval e0 env), (eval e1 env)))
    | SUB (e0, e1) -> (binary_eval ('-', (eval e0 env), (eval e1 env)))
    | MUL (e0, e1) -> (binary_eval ('*', (eval e0 env), (eval e1 env)))
    | EQ (e0, e1) -> (binary_eval ('=', (eval e0 env), (eval e1 env)))
    | LT (e0, e1) -> (binary_eval ('<', (eval e0 env), (eval e1 env)))
    | GT (e0, e1) -> (binary_eval ('>', (eval e0 env), (eval e1 env)))
    | CONS (e0, e1) -> (binary_eval ('p', (eval e0 env), (eval e1 env)))
    | CAR p -> 
        (match p with
        | CONS (e, _) -> (eval e env)
        | _ ->  raise (RUNTIME_EXCEPTION "pair expected"))
    | CDR p -> 
        (match p with
        | CONS (_, e) -> (eval e env)
        | _ ->  raise (RUNTIME_EXCEPTION "pair expected"))
    | IF (b, e0, e1) -> 
        (match (eval b env) with
        | BOOL true -> (eval e0 env)
        | BOOL false -> (eval e1 env)
        | _ ->  raise (RUNTIME_EXCEPTION "boolean expected"))
        (*TODO: check how let & letrec works -> evaluate ATM : current implementation
          or evaluate when variable is called: have to remember where the value was *)
    | LET (blist, e) -> 
        begin 
          let ht = Hashtbl.create (List.length blist) in
          let _ = List.iter (function (v, e) -> Hashtbl.add ht v (eval e env)) blist in
          (eval e (ht::env)) (*wrong?*)
        end
    | LETREC (blist, e) -> 
        begin
          let ht = Hashtbl.create (List.length blist) in
          let _ = List.iter (function (v, e) -> Hashtbl.add ht v (eval e (ht::env))) blist in
          (eval e (ht::env)) (*wrong?*)
        end
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



(*let myeval_memo (exp_string: string): value_t =*)

  (*test like this: *)
let exp1 = "(letrec ((x 4) (z 3) (y (+ x z))) (let ((x 3) (y 4)) y))"
let v = myeval exp1
let _ = print_endline (value_to_string v)
