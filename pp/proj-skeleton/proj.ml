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

  let env = [] in 
  let hndl_env = [] in
  let table = Hashtbl.create 100 in

  (eval exp env hndl_env false table) 

and eval (exp: exp_t) env hndl to_mem tbl: value_t =
    match exp with
    | CONST (CINT n) -> INT n
    | CONST (CTRUE) -> BOOL true
    | CONST (CFALSE) -> BOOL false
    | CONST (CNULL) -> NULL
    | VAR v -> (look_up v env)
    | ADD (e0, e1) ->
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl), (eval e0 env hndl to_mem tbl), '+')))
    | SUB (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl), (eval e0 env hndl to_mem tbl), '-')))
    | MUL (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl), (eval e0 env hndl to_mem tbl), '*')))
    | EQ (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl), (eval e0 env hndl to_mem tbl), '=')))
    | LT (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl), (eval e0 env hndl to_mem tbl), '<')))
    | GT (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl), (eval e0 env hndl to_mem tbl), '>')))
    | CONS (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl), (eval e0 env hndl to_mem tbl), 'p')))
    | MCONS (e0, e1) -> 
        (binary_eval 
          (rev_3 ((eval e1 env hndl to_mem tbl), (eval e0 env hndl to_mem tbl), 'm')))
    | CAR p -> 
        begin match (eval p env hndl to_mem tbl) with
        | PAIR (el, _) -> el
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_pair)
        end
    | CDR p -> 
        begin match (eval p env hndl to_mem tbl) with
        | PAIR (_, el) -> el
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_pair)
        end
    | MCAR p -> 
        begin match (eval p env hndl to_mem tbl) with
        | MPAIR (el, _) -> !el
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end
    | MCDR p -> 
        begin match (eval p env hndl to_mem tbl) with
        | MPAIR (_, er) -> !er
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end
    | SETMCAR (e, el_new) ->
        begin match (eval e env hndl to_mem tbl) with
        | MPAIR (el, _)  ->
            let _ = el:= (eval el_new env hndl to_mem tbl) in
            VOID
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end
    | SETMCDR (e, er_new) ->
        begin match (eval e env hndl to_mem tbl) with
        | MPAIR (_, er)  ->
            let _ = er:= (eval er_new env hndl to_mem tbl) in
            VOID
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_mpair)
        end
    | IF (b, e0, e1) -> 
        begin match (eval b env hndl to_mem tbl) with
        | BOOL true -> (eval e0 env hndl to_mem tbl)
        | BOOL false -> (eval e1 env hndl to_mem tbl)
        | _ ->  raise (RUNTIME_EXCEPTION e_msg_need_bool)
        end
    | LET (blist, exp) -> 
          let ht = Hashtbl.create (List.length blist) in
          let add_to_env = 
            (fun (v, e) -> 
              if (Hashtbl.mem ht v ) && (Hashtbl.find ht v <> UNDEF) then
                raise (RUNTIME_EXCEPTION "already defined")
              else
                Hashtbl.add ht v (eval e env hndl to_mem tbl)) in
          let _ = List.iter add_to_env blist in
          (eval exp (ht::env) hndl to_mem tbl)
    | LETREC (blist, exp) -> 
          let ht = Hashtbl.create (List.length blist) in
          let _ = List.iter (fun (v, _) -> Hashtbl.add ht v UNDEF) blist in
          let add_to_env_rec = 
            (fun (v, e) -> 
              if (Hashtbl.mem ht v ) && (Hashtbl.find ht v <> UNDEF) then
                raise (RUNTIME_EXCEPTION "already defined")
              else
                Hashtbl.add ht v (eval e (ht::env) hndl to_mem tbl)) in
          let _ = List.iter add_to_env_rec blist in
          (eval exp (ht::env) hndl to_mem tbl)
    (*FIXME: memoize lambda also?*)
    | APP (LAMBDA (vlist, exp), elist) ->
        if has_dup vlist then
          raise (RUNTIME_EXCEPTION "duplicate argument name")
        else
        (let ht = Hashtbl.create (List.length elist) in
        let add_to_env = 
          (fun v e ->
            Hashtbl.add ht v (eval e env hndl to_mem tbl)) in
        let _ = 
          try (let l = List.iter2 add_to_env vlist elist in l) with 
          | Invalid_argument "List.iter2" -> 
              raise (RUNTIME_EXCEPTION e_msg_operand) in
        (eval exp (ht::env) hndl to_mem tbl) )
    | APP (e, elist) ->
        let ht = Hashtbl.create (List.length elist) in
        let f = (eval e env hndl to_mem tbl) in
        let add_to_env =
          (fun v e -> Hashtbl.add ht v (eval e env hndl to_mem tbl)) in
        begin match (f, to_mem) with
        | CLOS (vlist, exp, en), false -> 
            let _ = 
              try (let l = List.iter2 add_to_env vlist elist in l) with 
              | Invalid_argument "List.iter2" -> 
                  raise (RUNTIME_EXCEPTION e_msg_operand) in
            (eval exp (ht::en) hndl to_mem tbl)
        | CLOS (vlist, exp, en), true -> 
            let _ = 
              try (let l = List.iter2 add_to_env vlist elist in l) with 
              | Invalid_argument "List.iter2" -> 
                  raise (RUNTIME_EXCEPTION e_msg_operand) in
            let arg_list = List.map 
              (fun e -> (eval e env hndl true tbl)) elist in
            let f_memo = (f, arg_list) in
            if (Hashtbl.mem tbl f_memo) then
              (Hashtbl.find tbl f_memo)
            else
              let _ = 
                (Hashtbl.add tbl f_memo (eval exp (ht::en) hndl true tbl)) in
              (Hashtbl.find tbl f_memo)
        | _ -> raise (RUNTIME_EXCEPTION e_msg_need_proc)
        end
    | RAISE excptn ->  (*lookup handlers environment*)
        (exception_handler excptn env hndl to_mem tbl)
    | HANDLERS (hdl_list, exp) ->
        begin
          try 
            let add_to_hndl_env = 
              (fun (p, e) -> 
                ((eval p env hndl to_mem tbl), (eval e env hndl to_mem tbl))) in
            let h_list = 
              List.map add_to_hndl_env hdl_list in
            (eval exp env (h_list::hndl) to_mem tbl) 
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
and exception_handler excptn env hndls to_mem tbl=
  let ht = Hashtbl.create 1 in
  match hndls with 
  | [] -> raise UNCAUGHT_EXCEPTION
  | []::tl -> (exception_handler excptn env tl to_mem tbl)
  | ((((CLOS ([v], e0, en)), (CLOS ([x], e1, _))))::tail) :: tl ->
      (*closures in handlers only have one parameter*)
      let _ = Hashtbl.add ht v (eval excptn env hndls to_mem tbl) in
      if ( (eval e0 (ht::en) tl to_mem tbl) = BOOL true) then
        raise (EXCEPTION_HANDLER (eval e1 (ht::en) tl to_mem tbl))
      else
        exception_handler excptn env (tail::tl) to_mem tbl
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
  let _ = debug exp exp_string in

  if (can_memo exp) then (*check for purity*)
    (eval exp [] [] true table)
  else
    (eval exp [] [] false table)

(*checks if a function is referentially transparent*)
and can_memo exp : bool =

  (*1st net: a function can be memoized if everything is pure*)
  if (all_pure exp) then
    true
  (*2nd net: a function can be memoized if mutables don't matter to results*)
  else if true then
    false
  else
    false

and all_pure exp : bool =
  match exp with
  | CONST _ -> true
  | VAR _ -> true
  | SETMCAR (_, _) | SETMCDR (_, _) -> false
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

  (*test like this: *)
let exp1 = "(cons 3 5)"
let v = myeval exp1
let _ = print_endline (value_to_string v)
