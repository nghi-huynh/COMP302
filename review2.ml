type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

type dir = L | R
type path = dir list

(** Find all paths in the tree using naive recursion. *)
let rec all_paths t : path list =
  assert false

(** Find all paths in the tree using tail recursion with continuations. *)
let rec all_paths_tr t k =
  assert false

(** Return a function that simulates f but increments an internal counter,
    which can be read by using the second returned function of type unit -> int.
 *)
let monitor (f : 'a -> 'b) : ('a -> 'b) * (unit -> int) =
  assert false

(** Logical formulas. *)
type exp =
  | Disj of exp * exp
  | Neg of exp
  | Var of int

(** Shorthand for variables. *)
let v k = Var k

(* OR operator *)
let ( +++ ) e1 e2 = Disj (e1, e2)

(* AND operator *)
let ( *** ) e1 e2 = Neg (Neg e1 +++ Neg e2)

(* IMPLIES operator *)
let ( ^=> ) e1 e2 = Neg e1 +++ e2                      

(** Lookup a variable assignment in the environment env. *)
let rec lookup env k = match env, k with
  | x :: _, 0 -> Some x
  | _ :: xs, k -> lookup xs (k-1)
  | _ -> None

(** Lookup a variable, but throw an exception if it is not assigned. *)
let lookup' env k =
  match lookup env k with
  | Some x -> x
  | _ -> failwith "variable out of bounds"

(** Evaluates a logical formula with a complete assignment of variables. *)
let rec eval env = function
  | Disj (e1, e2) -> eval env e1 || eval env e2
  | Neg e1 -> not (eval env e1)
  | Var k -> lookup' env k

(** Idea: partially evaluate an expression.
    Return None if the evaluation _crucially_ depends on unassigned
    variables. This means "don't know".
 *)
let rec eval_maybe env = assert false

exception Unsatsifiable

(** Build up a satisfying assignment of variables progressively. *)
let rec sat env phi k = assert false
