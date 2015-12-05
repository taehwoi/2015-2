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
  (*| PAIR of ...*)
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
  (*| PAIR (a, b) -> "(cons " ^ (value_to_string a) ^ " " ^ (value_to_string b) ^ ")"*)
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
    | ADD (e0, e1) -> INT (arith_eval ('+', (eval e0), (eval e1)))
    | SUB (e0, e1) -> INT (arith_eval ('-', (eval e0), (eval e1)))
    | MUL (e0, e1) -> INT (arith_eval ('*', (eval e0), (eval e1)))

and arith_eval exp =
  match exp with
  | ('+', (INT v0), (INT v1)) -> (v0 + v1)
  | ('-', (INT v0), (INT v1)) -> (v0 - v1)
  | ('*', (INT v0), (INT v1)) -> (v0 - v1)

in eval exp

(*let myeval_memo (exp_string: string): value_t =*)

  (*test like this: *)
let exp1 = "(- (+ 1 2) 2)"
let v = myeval exp1
let _ = print_endline (value_to_string v)
