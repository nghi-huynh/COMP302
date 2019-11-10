
(* SAT problem with exception-based backtracking *)

(** Logical formulas with variables. *)
type exp
  = Or of exp * exp
  | Neg of exp
  | Var of int

let ( +++ ) x y = Or (x, y)
let ( *** ) x y = Neg (Neg x +++ Neg y) (* de Morgan's law *)
let ( ^=> ) x y = Neg x +++ y

(** Evaluate a logical formula in a complete environment. *)
let rec eval env = function
  | Or (e1, e2) -> eval env e1 || eval env e2
  | Neg e1 -> not (eval env e1)
  | Var k -> List.nth env k

let rec lookup env k = match env, k with
  | x :: _, 0 -> Some x
  | _ :: xs, k -> lookup xs (k-1)
  | _ -> None

(** A notion of partial evaluation.
    An expression fails to evaluate (returns None) if its evaluation depends on unknown variables.

    Notice that (x OR y) is satisfiable in the (partial) environment
    {x = true}! Our partial evaluator takes this into account:
    if *either* side of an OR is definitely true, then the whole OR is definitely true.
    If *both* sides are maybe true or definitely false, then the whole
    OR is maybe true or definitely false, respectively.
    
 *)
let rec eval_maybe env = function
  | Or (e1, e2) ->
     begin match eval_maybe env e1 with
     | Some true -> Some true
     | _ -> eval_maybe env e2
     end
  | Neg e1 ->
     begin match eval_maybe env e1 with
     | Some b -> Some (not b)
     | None -> None
     end
  | Var k -> lookup env k
     
exception Unsatisfiable

(** sat finds a satisfying assignment `env` for the `k` variables in
    the logical formula `phi` by greedily assigning true to variables
    sequentially and backtracking when the formula becomes
    unsatisfiable.
    It does so by accumulating new assignments in the environment
    `env`.
 *)
let rec sat env phi k =
  match eval_maybe env phi with
  | Some true -> env (* then env is a satisfying assignment *)
  | Some false -> (* env contains a bad assignment *)
     raise Unsatisfiable
  | None -> (* not sure if env is satisfying yet *)
     (* there must be more variables left to assign *)
     assert (k > 0);
     try
       sat (env @ [true]) phi (k-1)
     with
     | Unsatisfiable ->
        sat (env @ [false]) phi (k-1)
