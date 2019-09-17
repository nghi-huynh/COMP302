(* TODO: Correct these tests for the double function. *)
let double_tests = [ 
  (0,0);
  (1,2);
  (3,6);
  
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec double (n:int):int = match n with
  |0 -> 0
  |n -> 2 + double (n - 1);; 

(* TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
let fact_tests = [
  (* Your test cases go here.
     Remember that the outputs of fact should be *floating-point* numbers.
  *)
  (0,1.0);
  (1,1.0);
  (3,6.0);
  (4,24.0);
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n:int) : float = match n with
  | 0 -> 1.0
  | _ -> float_of_int n *. fact (n - 1) ;;

(* TODO: Write a good set of tests for max_factor. *)
let max_factor_tests = [
  (7,1);
  (6,3);
  (12,6);
  (4,2);
  (15,5);
]

(* TODO: Implement max_factor. *)
let max_factor n =
  let rec f n m=match n mod m with
    |0 -> m
    |_ -> f n (m-1) in
  f n (n-1) ;;
(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [ (0,1);(1,1);(2,2);(3,3);(4,5);(5,8)];;

(* TODO: Implement a tail-recursive helper fib_aux. *)
let fib_aux n =
  let rec f n a b=
    match n with
    |0  -> a
    |1  -> b
    |_  -> f (n-1) a b + f(n-2) a b
  in
  f n 1 1
;;  

(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n  =  fib_aux n;;

