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

let unmatched_paren = "unmatched parenthesis"
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
      let tok = lexer () in
        begin match tok with
        | ID x ->
            Syn.APP (rev ( (exp_to_list lexer []), Syn.VAR x))
        | LPAREN -> 
            let proc =
              let t = lexer () in
              match t with
              | LAMBDA ->
                  (parse_LAM lexer)
              | LET | LETREC -> 
                  (parse_LETS t lexer)
              | CAR | CDR | MCAR | MCDR ->
                  (parse_UNARY t lexer)
              | IF ->
                  (parse_IF lexer)
              | _ -> raise (PARSE_ERROR "expect a procedure") in
            Syn.APP (proc, (exp_to_list lexer []))
        | PLUS | MINUS | TIMES | CONS | MCONS | EQ | LT | GT ->
            (parse_BINOP tok lexer)
        | SETMCAR | SETMCDR ->
            (parse_BINOP tok lexer)
        | CAR | CDR | MCAR | MCDR | RAISE ->
            (parse_UNARY tok lexer)
        | IF ->
           (parse_IF lexer)
        | LAMBDA ->
            (parse_LAM lexer) 
        | LET | LETREC -> 
            (parse_LETS tok lexer)
        | HANDLERS -> 
            (parse_HNDL lexer)
        | INT _ | TRUE | FALSE ->
            raise ( PARSE_ERROR ("expected a procedure") )
        | RPAREN -> raise (PARSE_ERROR "empty parenthesis")
        | _ ->  raise (PARSE_ERROR
                ((token_printer tok)^" is not a part of the syntax"))
        end
  | RPAREN -> raise (PARSE_ERROR  "missing operand")
  | EOF -> raise (PARSE_ERROR  "premature eof")
  | _ ->  raise (PARSE_ERROR  ((token_printer token)^" is not a part of the syntax"))

and parse_BINOP op lexer =
  let exp = 
    match op with
    | PLUS -> (fun x -> Syn.ADD x)
    | MINUS -> (fun x -> Syn.SUB x)
    | TIMES -> (fun x -> Syn.MUL x)
    | CONS -> (fun x -> Syn.CONS x)
    | MCONS -> (fun x -> Syn.MCONS x)
    | EQ -> (fun x -> Syn.EQ x)
    | LT -> (fun x -> Syn.LT x)
    | GT -> (fun x -> Syn.GT x)
    | SETMCAR -> (fun x -> Syn.SETMCAR x)
    | SETMCDR -> (fun x -> Syn.SETMCDR x)
    | _ -> raise (PARSE_ERROR "undefined BINOP") in

  let ret = (exp (rev ((parse_helper lexer), (parse_helper lexer)))) in
  (match_pair ret lexer)

and parse_UNARY op lexer =
  let exp = 
    match op with
    | CAR -> (fun x -> Syn.CAR x)
    | CDR -> (fun x -> Syn.CDR x)
    | MCAR -> (fun x -> Syn.MCAR x)
    | MCDR -> (fun x -> Syn.MCDR x)
    | RAISE -> (fun x -> Syn.RAISE x)
    | _ -> raise (PARSE_ERROR "undefined UNARY") in

  let ret = exp (parse_helper lexer) in
  (match_pair ret lexer)

and parse_LETS op lexer =
  let exp = 
    match op with
    | LET -> (fun x -> Syn.LET x)
    | LETREC -> (fun x -> Syn.LETREC x)
    | _ -> raise (PARSE_ERROR "undefined LET") in

    let ret = exp (rev ((parse_helper lexer), (bind_to_list lexer [] 0))) in
  (match_pair ret lexer)

and parse_LAM lexer =
  let exp = 
    Syn.LAMBDA (rev ((parse_helper lexer), (var_to_list lexer []))) in
  (match_pair exp lexer)

and parse_IF lexer =
  let exp = 
    Syn.IF (rev_3 ((parse_helper lexer), (parse_helper lexer), (parse_helper lexer))) in
  (match_pair exp lexer)

and parse_HNDL lexer =
  let exp = 
    Syn.HANDLERS (rev ((parse_helper lexer), (hndls_to_list lexer []))) in
  (match_pair exp lexer)

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

and exp_to_list (lexer: unit -> token) (el: Syntax.exp_t list) :Syntax.exp_t list =
  let exp =  
    try (parse_helper lexer) with
    | PARSE_ERROR ("missing operand") -> Syn.CONST Syn.CNULL in
  match exp with
  | Syn.CONST Syn.CNULL -> (List.rev el)
  | _ -> (exp_to_list lexer (exp::el))

and hndls_to_list lexer hl :(Syn.exp_t * Syn.exp_t) list =
  let _ = lexer () in (*exhaust one left paren*)
  let token = lexer () in
  match token with
  | LPAREN ->
      (hndls_to_list lexer (hl@[(rev ((parse_helper lexer), (parse_helper lexer)))]))
  | RPAREN ->
      hl
  | _ -> raise (PARSE_ERROR "expected a procedure")

and match_pair exp lexer =
  if (lexer () = RPAREN) then
    exp
  else
    raise (PARSE_ERROR unmatched_paren)
