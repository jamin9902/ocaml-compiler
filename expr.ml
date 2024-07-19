(* 
       CS 51 Final Project
      MiniML -- Expressions
           Spring 2017
*)

(* Abstract syntax of MiniML expressions *)
open Bignum ;;

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;
      
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | String of string                     (* strings *)
  | Bignum of BN.bignum                  (* bignums *)      
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Concat of expr * expr                (* string concatenation *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(* Sets of varids *)
module SS = Set.Make (struct
           type t = varid
           let compare = String.compare
         end ) ;;

type varidset = SS.t ;;

(* Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal ;;

(* Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;
  
(* Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> SS.singleton v                        
  | Num _ | Bool _ | Float _ | String _ | Bignum _ -> SS.empty                                                 
  | Unop (_, e) -> free_vars e                 
  | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2) 
  | Concat (e1, e2) -> SS.union (free_vars e1) (free_vars e2)       
  | Conditional (e1, e2, e3) ->  SS.union (SS.union (free_vars e1) 
                                                    (free_vars e2)) 
                                          (free_vars e3)
  | Fun (v, exp) -> SS.remove v (free_vars exp)                  
  | Let (v, e1, e2) -> SS.union (SS.remove v (free_vars e2)) (free_vars e1)         
  | Letrec (v, e1, e2) -> SS.union (SS.remove v (free_vars e1)) 
                                   (SS.remove v (free_vars e2))           
  | Raise | Unassigned -> SS.empty                         
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
;;
  

(* Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let new_varname =
  let ctr = ref 0 in
    fun () ->
      let v = "x" ^ string_of_int (!ctr) in
      ctr := !ctr + 1;
      v ;;
  
(* Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let rec subst' (exp : expr) = 
    match exp with 
    | Var x -> if x = var_name then repl else exp                     
    | Num _ | Float _ | Bool _ | String _ | Bignum _ -> exp               
    | Unop (u, e) -> Unop (u, subst' e)      
    | Binop (b, e1, e2) -> Binop (b, subst' e1, subst' e2) 
    | Concat (e1, e2) -> Concat(subst' e1, subst' e2)  
    | Conditional (e1, e2, e3) -> Conditional (subst' e1, subst' e2, subst' e3)
    | Fun (v, e) -> if v = var_name then
                      exp
                    else 
                      if SS.mem v (free_vars repl) then
                        let new_v = new_varname () in
                        Fun (new_v, subst' (subst v (Var new_v) e))
                      else
                        Fun(v, subst' e)
    | Let (v, e1, e2) ->  if v = var_name then
                            Let (v, subst' e1, e2)
                          else 
                            if SS.mem v (free_vars repl) then
                              let new_v = new_varname () in
                              Let (new_v, subst' e1, subst' (subst v (Var new_v) e2))
                            else
                              Let (v, subst' e1, subst' e2)
    | Letrec (v, e1, e2) -> if v = var_name then
                              exp
                            else
                              if SS.mem v (free_vars repl) then
                                let new_v = new_varname () in
                                Letrec (new_v, subst' (subst v (Var new_v) e1), 
                                        subst' (subst v (Var new_v) e2))
                              else 
                                Letrec (v, subst' e1, subst' e2)
    | Raise | Unassigned -> exp
    | App (e1, e2) -> App (subst' e1, subst' e2)
  in
  subst' exp ;;
 
(* helper functions to convert unops and binops to strings *)   
let string_of_unop (unp : unop) : string = 
  match unp with
  Negate -> "~-"
;;

let string_of_binop (bnp : binop) : string = 
  match bnp with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Equals -> "="
  | LessThan -> "<"
;;

(* storing space as a variable so it can be used easily later *)
let space = " "

(* exp_to_string -- Returns a string representation of the expr *)
let rec exp_to_string (exp : expr) : string =
  match exp with
  | Var x -> x                        
  | Num n -> string_of_int n
  | Float f -> string_of_float f 
  | String s -> "\"" ^ s ^ "\"" 
  | Bignum b -> "B" ^ BN.toString b                     
  | Bool b -> string_of_bool b                       
  | Unop (u, e) -> string_of_unop u ^ exp_to_string e      
  | Binop (b, e1, e2) -> "(" ^ exp_to_string e1 ^ space ^ string_of_binop b ^
                         space ^ exp_to_string e2 ^ ")" 
  | Concat (e1, e2) -> exp_to_string e1 ^ space ^ "^" ^ space ^ exp_to_string e2    
  | Conditional (e1, e2, e3) -> "if " ^ exp_to_string e1 ^
                                space ^ "then " ^ exp_to_string e2 ^
                                space ^ "else " ^ exp_to_string e3
  | Fun (v, e) -> "fun " ^ v ^ " -> " ^ exp_to_string e             
  | Let (v, e1, e2) -> "let " ^ v ^ " = " ^ exp_to_string e1 ^ " in " ^
                        exp_to_string e2       
  | Letrec (v, e1, e2) -> "let rec " ^ v ^ " = " ^ exp_to_string e1 ^ " in " ^
                        exp_to_string e2         
  | Raise -> "Raise"                               
  | Unassigned -> "Unassigned"                         
  | App (e1, e2) -> exp_to_string e1 ^ space ^ exp_to_string e2
   ;;


(* storing common elements as variables *)
let joiner : string = ", ";;
let closer : string = ")"

(* exp_to_abstract_string: Returns a string representation of the abstract
   syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^ closer                        
  | Num n -> "Num(" ^ string_of_int n ^ closer 
  | Float f -> "Float(" ^ string_of_float f ^ closer 
  | String s -> "String(" ^ s ^ closer 
  | Bignum b -> "Bignum(" ^ BN.toString b ^ closer                    
  | Bool b -> "Bool(" ^ string_of_bool b ^ closer                    
  | Unop (u, e) -> "Unop(" ^ string_of_unop u ^ joiner  ^ 
                    exp_to_abstract_string e ^ closer         
  | Binop (b, e1, e2) -> "Binop(" ^ string_of_binop b ^ joiner ^ 
                          exp_to_abstract_string e1 ^ joiner ^
                          exp_to_abstract_string e2 ^ closer 
  | Concat (e1, e2) -> "Concat(" ^ exp_to_abstract_string e1 ^ joiner ^ 
                        exp_to_abstract_string e2 ^ closer
  | Conditional (e1, e2, e3) -> "Conditional(" ^ exp_to_abstract_string e1 ^
                                joiner ^ exp_to_abstract_string e2 ^
                                joiner ^ exp_to_abstract_string e3 ^ closer
  | Fun (v, e) -> "Fun(" ^ v ^ joiner ^ exp_to_abstract_string e ^ closer                
  | Let (v, e1, e2) -> "Let(" ^ v ^ joiner ^ exp_to_abstract_string e1 ^ 
                       joiner ^ exp_to_abstract_string e2 ^ closer      
  | Letrec (v, e1, e2) -> "Letrec(" ^ v ^ joiner ^ exp_to_abstract_string e1 ^ 
                          joiner ^ exp_to_abstract_string e2 ^ closer        
  | Raise -> "Raise"                               
  | Unassigned -> "Unassigned"                          
  | App (e1, e2) -> "App(" ^ exp_to_abstract_string e1 ^ joiner ^ 
                     exp_to_abstract_string e2 ^ closer
;;
