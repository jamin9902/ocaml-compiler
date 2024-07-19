(* Bignum module to implement calculations in Bignum *)

(* signature
 * only the functions that are needed are made available *)
module type Bignum = sig
    type bignum = {neg: bool; coeffs: int list} ;;
    val negate : bignum -> bignum
    val equal : bignum -> bignum -> bool
    val less : bignum -> bignum -> bool
    val fromString : string -> bignum
    val toString : bignum -> string
    val plus :  bignum -> bignum -> bignum
    val times : bignum -> bignum -> bignum
  end

(* Implementation as module BN *)
module BN : Bignum =
  struct
    type bignum = {neg: bool; coeffs: int list} ;;


    (* The base for representing bignums. *)
    let base  = 1000 ;;
      
    (* Negation *)
    let negate (b : bignum) : bignum =
      if b.coeffs = [] then b else {neg = not b.neg; coeffs = b.coeffs}
    ;;

    (* Comparing bignums *)  

    (* check if two bignums are equal by comparing element by element *)  
    let equal (b1 : bignum) (b2 : bignum) : bool =
      b1 = b2       
    ;;


    (* helper function to compare coefficient lists.
    Check lengths of lists first and provide conditions.
    If length equal compare elements and return true or false for first inequal 
    values 
    *)
    let compare (comparator1 : int -> int -> bool) 
          (comparator2 : int list -> int list -> bool)
          (coeffs1 : int list) 
          (coeffs2 : int list) : bool =
      let len1, len2 = List.length coeffs1, List.length coeffs2 in 
      if comparator1 len1 len2
        then true
      else if comparator1 len2 len1
        then false  
      else comparator2 coeffs1 coeffs2
    ;;

    (* check if first bignum is less than second by providing appropriate operator 
      to compare
     *)
    let less (b1 : bignum) (b2 : bignum) : bool =
      match b1.neg, b2.neg with
      | true, true -> compare (>) (>) b1.coeffs b2.coeffs
      | false, false -> compare (<) (<) b1.coeffs b2.coeffs
      | _, _ -> b1.neg
    ;;

    let greater (b1 : bignum) (b2 : bignum) : bool =
      less b2 b1
    ;;


    (* Helpful functions *)
    (* convert an int to bignum by recursively dividing the int by base *)
    let fromInt (n : int) : bignum =
      let rec make_list (number : int) = 
        match number with 
        | 0 -> []
        | number -> make_list (number / base) @ [number mod base]
      in 
      { neg = n < 0; coeffs = make_list (abs(n))}
    ;;

    (* stripzeroes -- Removes zero coefficients from the beginning of the
       coefficients part of a bignum representation *)
    let rec stripzeroes (b : int list) : int list =
      match b with
      | 0 :: t -> stripzeroes t
      | _ -> b ;;

           
    (* explode -- Splits a string into a list of its characters. *)
    let rec explode (s : string) : char list =
      let len = String.length s in
      if len = 0 then []
      else s.[0] :: explode (String.sub s 1 (len - 1)) ;;

    (* implode -- Condenses a list of characters into a string. *)
    let rec implode (cs : char list) : string =
      match cs with
      | [] -> ""
      | c :: t -> String.make 1 c ^ implode t ;;

    (* split -- Returns a pair (first n elements of lst, rest of elements
       of lst) *)
    let rec split lst n =
      if n = 0 then ([], lst)
      else match lst with
      | [] -> ([], [])
      | h :: t -> let (lst1, lst2) = split t (n - 1) in
                  (h :: lst1, lst2) ;;

    (* intlog -- Returns the floor of the base 10 log of an integer *)
    let intlog (base : int) : int =
      int_of_float (log10 (float_of_int base)) ;;

    (* fromString -- Converts a string representing an integer to a
       bignum. Assumes the base is a power of 10. *)
    let fromString (s : string) : bignum =
      let rec fromString_rec (cs : char list) : int list =
        if cs = [] then [] else
        let (chars_to_convert, rest) = split cs (intlog base) in
        let string_to_convert = implode (List.rev chars_to_convert) in
        int_of_string string_to_convert :: fromString_rec rest
      in
      match explode s with
      | [] -> fromInt 0
      | h :: t ->
          if h = '-' || h = '~' then
            {neg = true; coeffs = (List.rev (fromString_rec (List.rev t)))}
          else {neg = false;
                coeffs = (stripzeroes (List.rev (fromString_rec (List.rev (h :: t)))))}

    (* toString -- Converts a bignum to its string representation.
       Returns a string beginning with ~ for negative integers. Assumes
       the base is a power of 10. *)
    let toString (b : bignum) : string =
      let rec pad_with_zeroes_left (s : string) (len : int) =
        if String.length s >= len then s
        else "0" ^ pad_with_zeroes_left s (len - 1) in
      let rec stripstrzeroes (s : string) (c : char) =
        if String.length s = 0 then
          "0"
        else if String.get s 0 = '0' then
          stripstrzeroes (String.sub s 1 (String.length s - 1)) c
        else s in
      let rec coeffs_to_string (coeffs : int list) : string =
        match coeffs with
        | [] -> ""
        | h :: t -> pad_with_zeroes_left (string_of_int h) (intlog base)
                    ^ coeffs_to_string t in
      let stripped = stripzeroes b.coeffs in
      if List.length stripped = 0 then "0"
      else let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
           if b.neg then "~" ^ from_coeffs else from_coeffs ;;

    (*......................................................................
    Arithmetic functions
    ......................................................................*)

    (* plus_pos -- Returns a bignum representing b1 + b2.  Assumes that b1
       + b2 > 0. *)
    let plus_pos (b1 : bignum) (b2 : bignum) : bignum =
      let pair_from_carry (carry : int) =
        if carry = 0 then (false, [])
        else if carry = 1 then (false, [1])
        else (true, [1])
      in
      let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry : int)
        : bool * int list =
        match (coeffs1, coeffs2) with
        | ([], []) -> pair_from_carry carry
        | ([], _) ->
            if carry = 0 then (neg2, coeffs2)
            else plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
        | (_, []) ->
            if carry = 0 then (neg1, coeffs1)
            else plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
        | (h1 :: t1, h2 :: t2) ->
            let (sign1, sign2) =
                ((if neg1 then -1 else 1), (if neg2 then -1 else 1)) in
            let result = h1 * sign1 + h2 * sign2 + carry in
            if result < 0 then
              let (negres, coeffsres) =
                  plus_with_carry (neg1, t1) (neg2, t2) (-1)
              in (negres, result + base :: coeffsres)
            else if result >= base then
              let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
              in (negres, result - base :: coeffsres)
            else
              let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
              in (negres, result :: coeffsres)
      in
      let (negres, coeffsres) =
          plus_with_carry (b1.neg, List.rev b1.coeffs)
                          (b2.neg, List.rev b2.coeffs)
                          0
      in {neg = negres; coeffs = stripzeroes (List.rev coeffsres)} ;;

    let plus (b1 : bignum) (b2 : bignum) : bignum =
      if b1.neg <> b2.neg && equal (negate b1) b2 then {neg =  false; coeffs = []}
      else if b1.neg && b2.neg then negate (plus_pos (negate b1) (negate b2))
      else if b1.neg then if greater (negate b1) b2  
                  then negate (plus_pos (negate b1) (negate b2))
                  else  plus_pos b1 b2
      else if b2.neg then if less b1 (negate b2) 
                  then negate (plus_pos (negate b1) (negate b2))
                  else  plus_pos b1 b2
      else plus_pos b1 b2 
    ;;

    (*......................................................................
    Times
    ......................................................................*)
    (* return the qutient and remainder when interger is divided by base *)
    let div_mod (m : int) =
      (m / base, m mod base)
    ;;

    (* multiply a list representing coefficients by an int and return a list 
    representing the coefficients of the product *)
    let mult_list (multiplier : int) (lst : int list) : int list =
      let rec mult_rev_list (multiplier : int) 
                  (lst : int list) 
                  (carry : int) : int list =
        match lst with
        | [] -> let quotient, remainder =  div_mod carry in
            if quotient <> 0 
              then remainder :: (mult_rev_list multiplier [] quotient)
            else [remainder]
        | hd :: tl -> let quotient, remainder =  
                        div_mod (multiplier * hd + carry) in
                remainder :: (mult_rev_list multiplier tl quotient)
      in
      List.rev (mult_rev_list multiplier (List.rev lst) 0)
    ;;

    (* return a list of the products of the elements of a coefficient list
    with a second list *)
    let rec_mult (lst1 : int list) (lst2 : int list) : int list list =
      let rec rec_mult_rev (lst1 : int list) (lst2 : int list) : int list list =
        match lst1 with
        | [] -> []
        | hd :: tl -> (mult_list hd lst2) :: rec_mult_rev tl lst2
      in
      List.rev (rec_mult_rev (List.rev lst1) lst2)
    ;;

    (* adds a 0 at the end of list for every power of base *)
    let rec add_shift (shift_val : int) : int list =
      match shift_val with 
      | 0 -> []
      | shift_val -> [0] @ add_shift (shift_val - 1)
    ;;

    (* multiply the elements of the result of rec_mult with proper power of base 
    by adding appropriate shifts and return a list of proper bignums *)
    let rec mult_base_sum (lst : int list list) : bignum list = 
      match lst with 
      | [] -> []
      | hd :: tl -> {neg = false; coeffs = hd @ add_shift (List.length tl)} ::
                            mult_base_sum tl 
    ;;

    (* utilize helper functions to sum the results of mult_base_sum and return 
    result with appropriate sign *)
    let times (b1 : bignum) (b2 : bignum) : bignum =
      let prod_list = mult_base_sum (rec_mult b1.coeffs b2.coeffs)
      in
      let unsigned_result = List.fold_left (fun acc x -> plus acc x) 
                         {neg = false; coeffs = []} 
                         prod_list
      in 
      if b1.neg = b2.neg then unsigned_result
      else negate (unsigned_result)
    ;;
  end
;;