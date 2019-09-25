exception NotImplemented

let fact n =
  let rec factorial n =
    if n = 0 then 1
    else  n * factorial (n - 1)
  in
  if n <= 0 then 1 else factorial n

let binom (n, k) =
  if n < k then 0.0
  else float (fact n) /. (float (fact k) *. float (fact (n - k)))

let dist_black n x (marblesTotal, marblesDrawn) =
  (binom (n, x) *. binom (marblesTotal - n, marblesDrawn - x))
  /. (binom (marblesTotal, marblesDrawn))

let rec tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n-1) ((f n)::acc)
  in
  tab n []

let max_in_list l =
  let rec max_in_list' pos l =
    match l with
    | [] -> assert false
    | [h]  -> (pos, h)
    | h::t ->
      let (q, mx) = max_in_list' (pos + 1) t in
      if h < mx then (q, mx)
      else (pos, h)
  in
  let (pos, _) = max_in_list' 0 l in
  pos
  
  (* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (* Remember: your test cases should have this form:
     ((f, n), output)
     The following test case asserts that:
       tabulate (fun x -> x) (-1)
     should have the output []
  *)
  (((fun x -> x), 0), [0]);
  (((fun x ->x+1),1),[1;2])
  
]

(* TODO: Implement dist_table. *)
let dist_table (marblesTotal, marblesDrawn) x = 
  tabulate (fun y->dist_black y x (marblesTotal,marblesDrawn)) marblesTotal
;;

(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list =
  [([],true);([[1.2;3.2];[2.3;1.]],false);([[]],true)]
;;
(* TODO: Implement is_empty. *)
let is_empty matrix =
  List.for_all (fun x->x=[]) matrix
;;
(*List.for_all (fun x-> x=[]) [[]];;*)
(* TODO: Implement dist_matrix. *)
let dist_matrix (total, drawn) resultList =
  List.map (fun x-> dist_table (total,drawn) x) resultList
;;

let rec transpose xss =
  match xss with
  | [] -> []
  | []::_ -> []
  | _ -> List.map List.hd xss :: transpose (List.map List.tl xss);;
(* TODO: Implement combined_dist_table. *)
let combined_dist_table matrix = 
  List.map (List.fold_left (fun x->fun y->y*.x) 1.) (transpose matrix)
;;


(* Once you have implemented all the above functions, you can
   use this function to compute the maximum likelihood.
   You can try it on the given example by running:
     max_likelihood (6, 3) [2; 0; 1]
*)
let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
       (dist_matrix (total, drawn) resultList))
;;

