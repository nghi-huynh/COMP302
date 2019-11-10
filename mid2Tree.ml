(* Examples for reviewing for midterm 2.
   Author: Jacob Thomas Errington
 *)

(** Binary trees. *)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(** Directions in a binary tree: at a node we can go left or right. *)
type dir = L | R

(** A path in a tree is a list of directions to follow at each node. *)
type path = dir list

let empty_path = []

(** Checks that a path is legit. *)
let rec validate t p = match t, p with
  | Empty, [] -> true
  | Node (l, _, _), L :: p -> validate l p
  | Node (_, _, r), R :: p -> validate r p
  | _ -> false
       
(** Forms a list of all paths in the tree, from left to right. *)
let rec all_paths t : path list =
  match t with
  | Empty -> [empty_path]
  | Node (l, _, r) ->
     let ps_l = List.map (fun p -> L :: p) (all_paths l) in
     let ps_r = List.map (fun p -> R :: p) (all_paths r) in
     ps_l @ ps_r

let rec all_paths_tr t (k : path list -> 'r) : 'r =
  match t with
  | Empty -> k [empty_path]
  | Node (l, _, r) ->
     all_paths_tr l
       (fun ps_l ->
         let ps_l = List.map (fun p -> L :: p) ps_l in
         all_paths_tr r
           (fun ps_r ->
             let ps_r = List.map (fun p -> R :: p) ps_r in
             k (ps_l @ ps_r)))

(* We want to add logging to a sensitive part of our application. *)
let with_logging name f =
  fun x ->
  print_string ("entering " ^ name ^ "\n");
  let y = f x in
  print_string ("exiting " ^ name ^ "\n");
  y
    
(** We want to monitor how many times `f` is called.
    Given f : 'a -> 'b,
    with_monitor f = (r, f')
    such that r : unit -> int
    and tells us how many times f' has been called
    and f' : 'a -> 'b
    executes f and updates the internal counter.
 *)
let with_monitor f =
  let counter = ref 0 in
  let r () = !counter in
  let f' x =
    incr counter;
    f x
  in
  ( r, f' )
