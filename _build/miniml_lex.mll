(* 
			 CS 51 Final Project
		      MiniML -- Lexical Analyzer
			     Spring 2017
*)

{
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
}

let digit = ['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let sym = ['(' ')'] | (['+' '-' '*' '.' '=' '~' ';' '<' '>' '^']+)
(* regexp fpr strings 
 * starts and with a double quotation mark with anything else in between
 *)
let strings = ['"'] [^ '"']* ['"']



rule token = parse
  | digit+ as inum
  	{ let num = int_of_string inum in
	  INT num
	}
  | digit+ '.' digit* as fnum
  	{ let num = float_of_string fnum in
  	  FLOAT num
  	}
  | ['B'] digit+ as bigN
  	{
  		let bnum = Bignum.BN.fromString (String.sub bigN 1 (String.length bigN - 1)) in
  		BIGNUM bnum
  	}
  | id as word
  	{ try
	    let token = Hashtbl.find keyword_table word in
	    token 
	  with Not_found ->
	    ID word
	}
  | sym as symbol
	{ try
	    let token = Hashtbl.find sym_table symbol in
	    token
	  with Not_found ->
	    token lexbuf
	}
  | strings as str
  	{
   		STRING (String.sub str 1 (String.length str - 2))
  	}
  | '{' [^ '\n']* '}'	{ token lexbuf }    (* skip one-line comments *)
  | [' ' '\t' '\n']	{ token lexbuf }    (* skip whitespace *)
  | _ as c                                  (* warn and skip unrecognized characters *)
  	{ printf "Unrecognized character: %c\n" c;
	  token lexbuf
	}
  | eof
        { raise End_of_file }
