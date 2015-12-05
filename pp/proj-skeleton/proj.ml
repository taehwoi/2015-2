open Syntax
open Lexer

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


let myeval (exp_string: string): value_t =
  let lexbuf = Lexing.from_string exp_string in
  let lexer () = Lexer.token lexbuf in
  let exp = Parser.parse lexer in
  let _ = print_string "input: " in
  let _ = print_endline exp_string in
  let _ = print_string "exp: " in
  let _ = print_endline (exp_to_string exp) in

  let rec eval (exp: exp_t) : value_t =
    match exp with
    | CONST (CINT n) -> INT n
    | CONST (CTRUE) -> BOOL true
    | CONST (CFALSE) -> BOOL false
    | CONST (CNULL) -> VOID
    | ADD (e0, e1) -> (arith_eval '+' ((eval e0), (eval e1)))
    | SUB (e0, e1) -> (arith_eval '-' ((eval e0), (eval e1)))
    | MUL (e0, e1) -> (arith_eval '*' ((eval e0), (eval e1)))
    | EQ (e0, e1) -> (arith_eval '=' ((eval e0), (eval e1)))
    | LT (e0, e1) -> (arith_eval '<' ((eval e0), (eval e1)))
    | GT (e0, e1) -> (arith_eval '>' ((eval e0), (eval e1)))
    (*include this in arith_eval?*)
    | CONS (e0, e1) -> PAIR ((eval e0), (eval e1))
    (*FIXME: don't nest matches?*)
    | CAR p -> 
        (match p with
        | CONS (e, _) -> (eval e)
        | _ ->  raise (RUNTIME_EXCEPTION "pair expected"))
    | CDR p -> 
        (match p with
        | CONS (_, e) -> (eval e)
        | _ ->  raise (RUNTIME_EXCEPTION "pair expected"))
    | IF (b, e0, e1) -> 
        (match (eval b) with
        | BOOL true -> (eval e0)
        | BOOL false -> (eval e1)
        | _ ->  raise (RUNTIME_EXCEPTION "boolean expected"))
    | _ -> raise NOT_IMPLEMENTED

and arith_eval op vals=
  match op with
  | '+' -> 
      (match vals with 
      | ((INT a), (INT b)) -> INT (a + b) 
      | _ -> raise (RUNTIME_EXCEPTION "+ with not-int" ) )
  | '-' -> 
      (match vals with 
      | ((INT a), (INT b)) -> INT (a - b) 
      | _ -> raise (RUNTIME_EXCEPTION "- with not-int" ) )
  | '*' -> 
      (match vals with 
      | ((INT a), (INT b)) -> INT (a * b) 
      | _ -> raise (RUNTIME_EXCEPTION "* with not-int" ) )
  | '=' -> 
      (match vals with 
      | ((INT a), (INT b)) -> BOOL (a = b) 
      | _ -> raise (RUNTIME_EXCEPTION "comparison of non-int not allowed" ) )
  | '<' -> 
      (match vals with 
      | ((INT a), (INT b)) -> BOOL (a < b) 
      | _ -> raise (RUNTIME_EXCEPTION "comparison of non-int not allowed" ) )
  | '>' -> 
      (match vals with 
      | ((INT a), (INT b)) -> BOOL (a > b) 
      | _ -> raise (RUNTIME_EXCEPTION "comparison of non-int not allowed" ) )
  | _ -> raise NOT_IMPLEMENTED

in eval exp

(*let myeval_memo (exp_string: string): value_t =*)

  (*test like this: *)
let exp1 = "(if (= 3 3) (cons 3 5) (cons 4 6))"
let v = myeval exp1
let _ = print_endline (value_to_string v)
