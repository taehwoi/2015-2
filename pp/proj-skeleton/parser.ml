(* throw this exception with a proper message if you meet a parsing error. *)
exception PARSE_ERROR of string

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
  (*| LETREC*)
  | EQ
  | LT
  | GT
  (*| MCONS*)
  (*| MCAR*)
  (*| MCDR*)
  (*| SETMCAR*)
  (*| SETMCDR*)
  (*| RAISE*)
  (*| HANDLERS*)
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
  (*| LETREC -> "letrec"*)
  | EQ -> "="
  | LT -> "<"
  | GT -> ">"
  (*| MCONS -> "mcons"*)
  (*| MCAR -> "mcar"*)
  (*| MCDR -> "mcdr"*)
  (*| SETMCAR -> "set-mcar!"*)
  (*| SETMCDR -> "set-mcdr!"*)
  (*| RAISE -> "raise"*)
  (*| HANDLERS -> "with-handlers"*)
  | EOF -> "eof"

(* You can get a token by calling `lexer` function like this example: *)
(* let token = lexer () in ... *)
let rev = function
  (a, b) -> (b, a)

let rev_3 = function
  (a, b, c) -> (c, b, a)

(*FIXME: tuples are evaluated from right, watch out for sideeffects*)
let rec parse (lexer: unit -> token): Syntax.exp_t =
  let token = lexer () in
  let _ = print_endline (token_printer token ) in
  match token with 
  | INT n -> CONST (CINT n)
  | ID v -> VAR v
  | PLUS -> (ADD (rev ((parse lexer), (parse lexer))))
  | MINUS -> (SUB (rev ((parse lexer), (parse lexer))))
  | TIMES -> (MUL (rev ((parse lexer), (parse lexer))))
  | LPAREN -> (parse lexer)
  | RPAREN -> (parse lexer)
  | TRUE -> CONST CTRUE
  | FALSE -> CONST CFALSE
  | NULL -> CONST CNULL
  | IF -> IF (rev_3 ((parse lexer), (parse lexer), (parse lexer)))
  | CONS -> CONS (rev ((parse lexer), (parse lexer)))
  | CAR -> CAR (parse lexer)
  | CDR -> CDR (parse lexer)
  | LAMBDA ->
      LAMBDA (rev ((parse lexer), (var_to_list lexer [])))
  | LET -> 
      let _ = lexer () in
      let _ = lexer () in
      LET (rev ((parse lexer), (bind_to_list lexer [])))
  | EQ -> EQ (rev ((parse lexer), (parse lexer)))
  | LT -> LT (rev ((parse lexer), (parse lexer)))
  | GT -> GT (rev ((parse lexer), (parse lexer)))
  | EOF -> VAR "end"
and var_to_list (lexer: unit -> token) (vl: Syntax.var_t list): Syntax.var_t list =
  let token = lexer () in
  match token with
  | LPAREN -> (var_to_list lexer vl)
  | ID v -> (var_to_list lexer (vl@[v]))
  | RPAREN -> vl
  | _ -> raise (PARSE_ERROR "not a variable")
and bind_to_list (lexer: unit -> token) (bl: Syntax.binding_t list):Syntax.binding_t list =
  let token = lexer () in
  let _ = print_string "at b_to_l: " in
  let _ = print_endline (token_printer token ) in
  match token with
  | LPAREN -> []
  | ID v -> (bind_to_list lexer (bl@[(v,parse lexer)]))
  | RPAREN -> 
      if (next_token lexer) = RPAREN 
      then 
        let _ = print_string "end" in
        bl
      else (bind_to_list lexer bl)
  | _ -> raise (PARSE_ERROR "not a variable")
and next_token lexer: token =
  let a = lexer () in 
  let _ = print_endline (token_printer a ) in
  a
