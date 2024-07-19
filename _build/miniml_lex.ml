# 7 "miniml_lex.mll"
 
  open Printf ;;
  open Miniml_parse ;; (* need access to parser's token definitions *)
  open Bignum ;;

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table = 
    create_hashtable 8 [
		       ("if", IF);
		       ("in", IN);
		       ("then", THEN);
		       ("else", ELSE);
		       ("let", LET);
		       ("raise", RAISE);
		       ("rec", REC);
		       ("true", TRUE);
		       ("false", FALSE);
		       ("lambda", FUNCTION);
		       ("fun", FUNCTION);
		       ("function", FUNCTION)
		     ]
		     
  let sym_table = 
    create_hashtable 8 [
		       ("=", EQUALS);
		       ("<", LESSTHAN);
		       (".", DOT);
		       ("->", DOT);
		       (";;", EOF);
		       ("~", NEG);
		       ("+", PLUS);
		       ("-", MINUS);
		       ("*", TIMES);
		       ("(", OPEN);
		       (")", CLOSE);
		       (* symbol for string concatenation *)
		       ("^", CONCAT)
		     ]

# 46 "miniml_lex.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\246\255\247\255\248\255\002\000\001\000\030\000\251\255\
    \031\000\109\000\121\000\131\000\141\000\250\255\003\000\029\000\
    \083\000";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\008\000\008\000\004\000\255\255\
    \003\000\008\000\000\000\001\000\002\000\255\255\255\255\255\255\
    \006\000";
  Lexing.lex_default =
   "\002\000\000\000\000\000\000\000\015\000\014\000\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\014\000\015\000\
    \015\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\005\000\013\000\000\000\013\000\000\000\255\255\
    \007\000\007\000\006\000\006\000\000\000\006\000\006\000\000\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\000\000\006\000\006\000\006\000\006\000\000\000\
    \000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\006\000\000\000\006\000\006\000\000\000\000\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\006\000\006\000\006\000\006\000\255\255\006\000\000\000\
    \000\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\004\000\006\000\000\000\006\000\016\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\016\000\000\000\006\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\011\000\
    \000\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\255\255\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\004\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\005\000\255\255\014\000\255\255\015\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \006\000\006\000\255\255\006\000\006\000\255\255\255\255\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\006\000\006\000\006\000\006\000\016\000\000\000\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\006\000\255\255\000\000\004\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\015\000\255\255\006\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\010\000\
    \255\255\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \016\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\005\000\004\000\014\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\015\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\016\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 62 "miniml_lex.mll"
              inum
# 184 "miniml_lex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 63 "miniml_lex.mll"
   ( let num = int_of_string inum in
	  INT num
	)
# 190 "miniml_lex.ml"

  | 1 ->
let
# 66 "miniml_lex.mll"
                         fnum
# 196 "miniml_lex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 67 "miniml_lex.mll"
   ( let num = float_of_string fnum in
  	  FLOAT num
  	)
# 202 "miniml_lex.ml"

  | 2 ->
let
# 70 "miniml_lex.mll"
                    bigN
# 208 "miniml_lex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 71 "miniml_lex.mll"
   (
  		let bnum = Bignum.BN.fromString (String.sub bigN 1 (String.length bigN - 1)) in
  		BIGNUM bnum
  	)
# 215 "miniml_lex.ml"

  | 3 ->
let
# 75 "miniml_lex.mll"
          word
# 221 "miniml_lex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 76 "miniml_lex.mll"
   ( try
	    let token = Hashtbl.find keyword_table word in
	    token 
	  with Not_found ->
	    ID word
	)
# 230 "miniml_lex.ml"

  | 4 ->
let
# 82 "miniml_lex.mll"
           symbol
# 236 "miniml_lex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 83 "miniml_lex.mll"
 ( try
	    let token = Hashtbl.find sym_table symbol in
	    token
	  with Not_found ->
	    token lexbuf
	)
# 245 "miniml_lex.ml"

  | 5 ->
let
# 89 "miniml_lex.mll"
               str
# 251 "miniml_lex.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 90 "miniml_lex.mll"
   (
   		STRING (String.sub str 1 (String.length str - 2))
  	)
# 257 "miniml_lex.ml"

  | 6 ->
# 93 "miniml_lex.mll"
                      ( token lexbuf )
# 262 "miniml_lex.ml"

  | 7 ->
# 94 "miniml_lex.mll"
                    ( token lexbuf )
# 267 "miniml_lex.ml"

  | 8 ->
let
# 95 "miniml_lex.mll"
         c
# 273 "miniml_lex.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 96 "miniml_lex.mll"
   ( printf "Unrecognized character: %c\n" c;
	  token lexbuf
	)
# 279 "miniml_lex.ml"

  | 9 ->
# 100 "miniml_lex.mll"
        ( raise End_of_file )
# 284 "miniml_lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

