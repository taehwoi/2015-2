(* throw this exception with a proper message if you meet a parsing error. *)
exception PARSE_ERROR of string
module Syn= Syntax

type token =
  | INT of int
  | ID of string
  | PLUS
  | MINUS
  | TIMES
  | LPAREN
  | RPAREN
  | TRUE
  | FALSE
  | NULL
  | IF
  | CONS
  | CAR
  | CDR
  | LAMBDA
  | LET
  | LETREC
  | EQ
  | LT
  | GT
  | MCONS
  | MCAR
  | MCDR
  | SETMCAR
  | SETMCDR
  | RAISE
  | HANDLERS
  | EOF

(* you can use this for debugging *)
let token_printer = function
  | INT n -> string_of_int n
  | ID id -> id
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | LPAREN -> "("
  | RPAREN -> ")"
  | TRUE -> "#t"
  | FALSE -> "#f"
  | NULL -> "'()"
  | IF -> "if"
  | CONS -> "cons"
  | CAR -> "car"
  | CDR -> "cdr"
  | LAMBDA -> "lambda"
  | LET -> "let"
  | LETREC -> "letrec"
  | EQ -> "="
  | LT -> "<"
  | GT -> ">"
  | MCONS -> "mcons"
  | MCAR -> "mcar"
  | MCDR -> "mcdr"
  | SETMCAR -> "set-mcar!"
  | SETMCDR -> "set-mcdr!"
  | RAISE -> "raise"
  | HANDLERS -> "with-handlers"
  | EOF -> "eof"

(* You can get a token by calling `lexer` function like this example: *)
(* let token = lexer () in ... *)
let rev = function
  (a, b) -> (b, a)

let rev_3 = function
  (a, b, c) -> (c, b, a)

let rec parse (lexer: unit -> token): Syn.exp_t =
  let exp = parse_helper lexer in
  if lexer() = EOF then
    exp
  else
    raise (PARSE_ERROR "trailing characters")

and parse_helper lexer =
  let token = lexer () in
  match token with 
  | INT n -> Syn.CONST (Syn.CINT n)
  | ID v -> Syn.VAR v
  | TRUE -> Syn.CONST Syn.CTRUE
  | FALSE -> Syn.CONST Syn.CFALSE
  | NULL -> Syn.CONST Syn.CNULL
  | LPAREN -> 
      let exp = 
        let token = lexer () in
        (*let _ = print_endline (token_printer token) in*)
        match token  with
        | LPAREN -> 
            begin
              let exp = 
              match lexer () with
              | LAMBDA -> 
                  let vl = (var_to_list lexer []) in
                  let proc = (Syn.LAMBDA (rev ((parse_helper lexer), vl))) in
                  let _ = lexer () in (*exhaust right parenthesis*)
                  Syn.APP (rev ( (exp_to_list lexer [] (List.length vl)), proc))
              | _ -> raise (PARSE_ERROR "expect a procedure") in
              exp
            end
        | PLUS -> Syn.ADD (rev ((parse_helper lexer), (parse_helper lexer))) 
        | MINUS -> Syn.SUB (rev ((parse_helper lexer), (parse_helper lexer))) 
        | TIMES -> Syn.MUL (rev ((parse_helper lexer), (parse_helper lexer))) 
        | IF -> Syn.IF (rev_3 ((parse_helper lexer), (parse_helper lexer), (parse_helper lexer)))
        | CONS -> Syn.CONS (rev ((parse_helper lexer), (parse_helper lexer))) 
        | MCONS -> Syn.MCONS (rev ((parse_helper lexer), (parse_helper lexer))) 
        | CAR -> Syn.CAR (parse_helper lexer) 
        | CDR -> Syn.CDR (parse_helper lexer) 
        | LAMBDA ->
            Syn.LAMBDA (rev ((parse_helper lexer), (var_to_list lexer [])))
        | LET -> 
            Syn.LET (rev ((parse_helper lexer), (bind_to_list lexer [] 0)))
        | LETREC -> 
            Syn.LETREC (rev ((parse_helper lexer), (bind_to_list lexer [] 0)))
        | EQ -> Syn.EQ (rev ((parse_helper lexer), (parse_helper lexer)))
        | LT -> Syn.LT (rev ((parse_helper lexer), (parse_helper lexer)))
        | GT -> Syn.GT (rev ((parse_helper lexer), (parse_helper lexer)))
        | MCAR -> Syn.MCAR (parse_helper lexer)
        | MCDR -> Syn.MCDR (parse_helper lexer)
        | SETMCAR -> Syn.SETMCAR (rev ( (parse_helper lexer), (parse_helper lexer) ))
        | SETMCDR -> Syn.SETMCDR (rev ( (parse_helper lexer), (parse_helper lexer) ))
        | INT _ | TRUE | FALSE -> 
            raise ( PARSE_ERROR ("expected a procedure") )
        (*FIXME*)
        | ID x ->
            (*FIXME*)
            Syn.APP (rev ( (exp_to_list lexer [] 1), Syn.VAR x))
        | RAISE -> Syn.RAISE (parse_helper lexer)
        | HANDLERS -> 
            Syn.HANDLERS (rev ((parse_helper lexer), (hdls_to_list lexer [])))
        | RPAREN -> raise (PARSE_ERROR "empty parenthesis")
        | _ -> Syn.VAR "END" in
      let p = lexer () in
      if p <> RPAREN then
        raise (PARSE_ERROR "unmatched parenthesis") 
      else 
        exp
  | RPAREN -> raise (PARSE_ERROR  "need more operand")
  | EOF -> raise (PARSE_ERROR  "premature eof")
  | _ ->  raise (PARSE_ERROR  ((token_printer token)^" is not a part of the syntax"))


and var_to_list (lexer: unit -> token) (vl: Syntax.var_t list): Syntax.var_t list =
  let token = lexer () in
  match token with
  | LPAREN -> (var_to_list lexer vl)
  | ID v -> (var_to_list lexer (vl@[v]))
  | RPAREN -> vl
  | _ -> raise (PARSE_ERROR "not a variable")

and bind_to_list (lexer: unit -> token) (bl: Syntax.binding_t list) cnt :Syntax.binding_t list =
  let token = lexer () in
  match token with
  | LPAREN -> (bind_to_list lexer bl (cnt+1))
  | ID v -> 
      (bind_to_list lexer (bl@[(v,parse_helper lexer)]) cnt)
  | RPAREN -> 
      if (cnt = 1) then (*consume one more paren*)
        bl
      else 
        (bind_to_list lexer bl (cnt-1))
  | _ -> raise (PARSE_ERROR "not a variable")

  (*parse exps for the number of variables in expression*)
and exp_to_list (lexer: unit -> token) (el: Syntax.exp_t list) cnt :Syntax.exp_t list =
  if cnt = 0 then el 
  else (exp_to_list lexer (el@[parse_helper lexer]) (cnt - 1))

and exp_to_list2 (lexer: unit -> token) (el: Syn.exp_t list) cnt :Syntax.exp_t list =
  if cnt = 0 then el 
  else (exp_to_list lexer (el@[parse_helper lexer]) (cnt - 1))

and hdls_to_list lexer hl :(Syn.exp_t * Syn.exp_t) list =
  let token = lexer () in
  match token with
  | LPAREN -> 
      (hdls_to_list lexer (hl@[(rev ((parse_helper lexer), (parse_helper lexer)))]))
  | RPAREN -> 
      hl
  | _ -> raise (PARSE_ERROR "expected a procedure")

