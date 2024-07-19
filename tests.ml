(* 
       CS 51 Final Project
         Test Program
         Spring 2017
*)

module Ev = Evaluation ;;
module MP = Miniml_parse ;;
module ML = Miniml_lex ;;
module Ex = Expr ;;

open Printf ;;

(* str_to_exp: string -> expr
   Returns the expression specified by the string using the Miniml
   parser. *)
let str_to_exp (str: string) : Ex.expr =
  let lexbuf = Lexing.from_string str in
  let exp = MP.input ML.token lexbuf in
  exp
;;

(* sample for freevars *)
let exp_for_freevars = [ str_to_exp "let x = 3 in let y = x in f x y;;"; 
						 str_to_exp "let x = x in let y = x in f x y;;";
						 str_to_exp "let x = y in let y = x in f x y;;"; 
						 str_to_exp "let x = y in let y = x in f x y;;";
						 str_to_exp "let x = fun y -> x in x;;"; 
						 str_to_exp  "let rec x = fun y -> x in x;;" ] ;;

let free_vars = [Ex.vars_of_list ["f"]; Ex.vars_of_list ["f";"x"]; Ex.vars_of_list ["f";"y"];
				Ex.vars_of_list ["f";"y"]; Ex.vars_of_list ["x"]; Ex.vars_of_list []];;

(* sample for testing to_abstract_string *)
let exps = ["1;;"; "true;;"; "5.1;;"; "~1;;"; "~1.;;"; "cs51;;"; "\"CS51\";;"; 
			 "38 + 4;;"; "3 * 17;;"; "53 - 1;;"; "\"This\" ^ \" is \" \"CS51\";;"; 
			 "38. + 4.;;"; "3. * 17.;;"; "53. - 1.;;"; "if 1 < 2 then 51 else 42;;"; 
			 "let x = 2017 in x + 1;;"; "1 = 3;;"; "3 = 3;;"; "1 < 3;;"; "3 < 1;;"; 
			 "2.1 = 2.11;;"; "2.1 < 2.11;;"; "let f = fun x -> x in f f 3;;";
			 "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4;;";
			 "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3;;";
			] ;;


let string_check = ["Num(1)"; "Bool(true)"; "Float(5.1)"; "Unop(~-, Num(1))";
					"Unop(~-, Float(1.))"; "Var(cs51)"; "String(CS51)"; 
					"Binop(+, Num(38), Num(4))"; "Binop(*, Num(3), Num(17))";
					"Binop(-, Num(53), Num(1))"; 
					"Concat(String(This), App(String( is ), String(CS51)))";
					"Binop(+, Float(38.), Float(4.))"; "Binop(*, Float(3.), " ^ 
					"Float(17.))";
					"Binop(-, Float(53.), Float(1.))"; 
					"Conditional(Binop(<, Num(1), Num(2)), Num(51), Num(42))";
					"Let(x, Num(2017), Binop(+, Var(x), Num(1)))";
					"Binop(=, Num(1), Num(3))"; "Binop(=, Num(3), Num(3))";
					"Binop(<, Num(1), Num(3))"; "Binop(<, Num(3), Num(1))";
					"Binop(=, Float(2.1), Float(2.11))"; "Binop(<, Float(2.1)," ^ 
					" Float(2.11))";
					"Let(f, Fun(x, Var(x)), App(App(Var(f), Var(f)), Num(3)))";
					"Letrec(f, Fun(x, Conditional(Binop(=, Var(x), Num(0)), " ^ 
					"Num(1), Binop(*, Var(x), App(Var(f), Binop(-, Var(x), " ^
					"Num(1)))))), App(Var(f), Num(4)))";
					"Let(x, Num(1), Let(f, Fun(y, Binop(+, Var(x), Var(y)))," ^ 
					" Let(x, Num(2), App(Var(f), Num(3)))))"
					] ;;

(* sample for testing evaluators *)
let exps2 = ["1;;"; "true;;"; "5.1;;"; "~1;;"; "~1.;;"; "\"CS51\";;"; "38 + 4;;"; "3 * 17;;"; 
			"53 - 1;;"; "38. + 4.;;"; "3. * 17.;;"; 
			 "53. - 1.;;"; "if 1 < 2 then 51 else 42;;"; "let x = 2017 in x + 1;;";
			 "1 = 3;;"; "3 = 3;;"; "1 < 3;;"; "3 < 1;;"; "2.1 = 2.11;;"; "2.1 < 2.11;;";
			 "let f = fun x -> x in f f 3;;";
			 "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4;;";
			 "let x = 1 in let f = fun y -> x + y in let x = 2 in f 3;;";
			] ;;

(* for lexical scope *)
let value_check1 = [Ex.Num(1); Ex.Bool(true); Ex.Float(5.1); Ex.Num(-1);
					Ex.Float(-1.); Ex.String("CS51"); 
					Ex.Num(42); Ex.Num(51); Ex.Num(52); 
					Ex.Float(42.); Ex.Float(51.); Ex.Float(52.); Ex.Num(51);
					Ex.Num(2018); Ex.Bool(false); Ex.Bool(true); Ex.Bool(true);
					Ex.Bool(false); Ex.Bool(false); Ex.Bool(true); Ex.Num(3);
					Ex.Num(24); Ex.Num(4)
					] ;;

(* for dynamic scope *)
let value_check2 = [Ex.Num(1); Ex.Bool(true); Ex.Float(5.1); Ex.Num(-1);
					Ex.Float(-1.); Ex.String("CS51"); 
					Ex.Num(42); Ex.Num(51); Ex.Num(52); 
					Ex.Float(42.); Ex.Float(51.); Ex.Float(52.); Ex.Num(51);
					Ex.Num(2018); Ex.Bool(false); Ex.Bool(true); Ex.Bool(true);
					Ex.Bool(false); Ex.Bool(false); Ex.Bool(true); Ex.Num(3);
					Ex.Num(24); Ex.Num(5)
					] ;;



(* function to test substitution *)
let test_subst (code : string) (var : Ex.varid) (value : Ex.expr) (check : Ex.expr) : bool =
	Ex.subst var value (str_to_exp code) = check
;;

(* function to help test the evaluators *)
let check (code : string) (check : Ex.expr) (evaluator : Ex.expr-> Ev.Env.env -> Ex.expr) : bool =
	let env = Ev.Env.create () in
	let exp = str_to_exp code in
	let res = evaluator exp env in
	res = check
;;

(* function to help test to_abstract_string *)
let check_to_abstract_string (code : string) (check : string) : bool =
	let exp = str_to_exp code in
	Ex.exp_to_abstract_string exp = check
;;

(* function to test abstract string *)
let test_to_abstract_string =
	List.fold_left2 (fun a code check -> a && check_to_abstract_string code check);;

(* function to test evaluators *)
let test_evals func =
	List.fold_left2 (fun a code checks -> a && check code checks func);;


(* test for free_vars *)
let _ = assert(List.fold_left2 (fun a x y -> a && Ex.same_vars (Ex.free_vars x) y) true exp_for_freevars free_vars);;
printf "free_vars tested\n";;

(* test for substitution *)
let _ = assert(test_subst "fun x -> x + x;;" "x" (Num 3) (Ex.Fun ("x", Ex.Binop 
	(Ex.Plus, Ex.Var "x", Ex.Var "x"))));;
let _ = assert(test_subst "fun x -> y + x;;" "y" (Num 3) (Ex.Fun ("x", Ex.Binop 
	(Ex.Plus, Num 3, Ex.Var "x"))));;
let _ = assert(test_subst "let x = y * y in x + x;;" "x" (Num 3) 
							(Ex.Let ("x", Ex.Binop (Ex.Times, Ex.Var "y", Ex.Var "y"), 
								Ex.Binop (Ex.Plus, Ex.Var "x", Ex.Var "x"))));;
let _ = assert(test_subst "let x = y * y in x + x;;" "y" (Num 3) 
							(Ex.Let ("x", Ex.Binop (Ex.Times, Ex.Num 3, Ex.Num 3), 
								Ex.Binop (Ex.Plus, Ex.Var "x", Ex.Var "x"))));;
printf "subst tested\n";;

(* test for to_abstract_string *)
let _ = assert(test_to_abstract_string true exps string_check);;

(* test for evaluators *)
let _ = assert(test_evals (fun a b -> Ev.val_to_e (Ev.eval_l a b)) true exps2 value_check1);;
printf "eval_l tested\n";;

let _ = assert(test_evals (fun a b -> Ev.val_to_e (Ev.eval_s a b)) true exps2 value_check1);;
printf "eval_s tested\n";;

let _ = assert(test_evals (fun a b -> Ev.val_to_e (Ev.eval_d a b)) true exps2 value_check2);;
printf "eval_d tested\n";;

