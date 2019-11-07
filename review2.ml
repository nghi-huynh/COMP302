
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

(* PROBLEM 3: SAT SOLVING *)

(* The Satisfiability Problem (SAT) is the problem of deciding whether
   a (propositional) logical formula is _satisfiable_, i.e. does there
   exist an assignment of truth values for the variables in the formula
   that makes it true?

   We represent logical formulas with the following type:
 *)

(** Logical formulas. *)
type exp =
  | Disj of exp * exp
  | Neg of exp
  | Var of int

(* Notice that we only define disjunctions (logical OR) and negations
   (logical NOT) as logical connectives.
   This is fine, because all other connectives one might care about
   (e.g. conjunction and implication) can be defined in terms of
   these.

   We now define some shorthands / syntactic sugar for the connectives.
 *)

(** Shorthand for variables. *)
let v k = Var k

(* OR operator *)
let ( +++ ) e1 e2 = Disj (e1, e2)

(* AND operator *)
let ( *** ) e1 e2 = Neg (Neg e1 +++ Neg e2)

(* IMPLIES operator *)
let ( ^=> ) e1 e2 = Neg e1 +++ e2                      

(* Here are some example formulas. *)

let ex1 =
  v 0 *** v 1  ^=> v 0
(* (a & b) => a *)

let ex2 =
  v 0 *** Neg (v 1)
(* a & ~a *)

let ex3 =
  v 0 +++ v 1 *** Neg (v 1)
(* a | (b & ~b) *)

(* Notice that we represent variables as _numbers_.
   They are represented by their index into a list that we call the
   _environment_ (usually called `env` in the code)

   When we _evaluate_ a formula in an environment, we recursively
   evaluate the subparts and recombine.
   e.g. eval env (v 0 &&& v 1)
   should evaluate v 0 and v 1 and then take the _boolean_ AND (&&).
   To evaluate `v 0`, we need to look up the 0th element of `env`.
   env : bool list, so the lookup produces a bool, and we're done.

   We need to implement this lookup operation.
 *)

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

(* Finally, we can implement a *complete* evaluator,
   which needs an environment `env` containing a value for *every*
   variable.
   I call such an `env` a *complete* environment.
 *)

(** Evaluates a logical formula with a complete assignment of variables. *)
let rec eval env = function
  | Disj (e1, e2) -> eval env e1 || eval env e2
  | Neg e1 -> not (eval env e1)
  | Var k -> lookup' env k

(* The idea for our SAT solver is to work not with complete
   environments, but with *partial* environments.
   A partial environment only assigns values to the first `n`
   variables.
   We can partially evaluate a formula when we have a partial
   environment by allowing our evaluator to answer in three ways:
   - The formula is _definitely_ true
   - The formula is _definitely_ false
   - We don't know if it's true or false
   The third option occurs when trying to evaluate a variable that
   does not have a value.
   We can represent these three possibilities with the type:
   bool option
   It gets propagated upwards in certain cases.
   For example, given the environment [true] (that is, v 0 = true),
   let's evaluate (v 1 +++ v 0)
   v 1 is unknown so we get None in the first recursive call.
   Now this isn't enough to conclude that the whole OR is unknown,
   because perhaps the right subexpression (v 0) is definitely true.
   So we must evaluate the right subexpression.
   We find that it is definitely true, so the whole OR must be
   definitely true, *even though* we didn't know anything about v 1.
 *)

(** Idea: partially evaluate an expression.
    Return None if the evaluation _crucially_ depends on unassigned
    variables. This means "don't know".
 *)
let rec eval_maybe (env : bool list) (phi : exp) : bool option =
  assert false

exception Unsatsifiable

(* Now we're ready to build the solver.
   `sat env phi k` is given a formula `phi` with `k` unassigned
   variables in it.
   It checks if the partial environment `env` is satisfying by using
   `eval_maybe`.
   If it is definitely true, `env` is satisfying so we're done.
   If it is definitely false, `env` must have a bad choice, so we need
   to backtrack.
   If it is unknown, we can make one more assignment, decrement k, and
   keep going.
 *)

(** Build up a satisfying assignment of variables progressively. *)
let rec sat env phi k = assert false
