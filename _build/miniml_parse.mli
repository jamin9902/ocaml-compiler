type token =
  | EOF
  | OPEN
  | CLOSE
  | LET
  | DOT
  | IN
  | REC
  | NEG
  | PLUS
  | MINUS
  | TIMES
  | LESSTHAN
  | EQUALS
  | IF
  | THEN
  | ELSE
  | FUNCTION
  | RAISE
  | ID of (string)
  | INT of (int)
  | FLOAT of (float)
  | STRING of (string)
  | BIGNUM of (Bignum.BN.bignum)
  | TRUE
  | FALSE
  | CONCAT

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
