exception NotImplemented

(* The type of ternary trees. *)
type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree * 'a tree

(* An example ternary tree. *)
let t = Node (
    1,
    Node (2, Empty, Empty, Empty),
    Node (3, Empty, Empty, Empty),
    Node (4, Empty, Empty, Empty)
  )

(* A function for constructing leaves of a tree.
   You may find this useful for writing cleaner test cases.
*)
let leaf v = Node (v, Empty, Empty, Empty)

(* Here are some functions that you may find useful for writing your tests.
   They have been modified so that the grader will print, e.g.,
   "(fun x -> x)", instead of just "<fun>".
*)
let identity = fun x -> x
let add1 = fun x -> x + 1

(* The type of linked lists. *)
type 'a llist =
  | Nil
  | Cons of (float * 'a) * 'a lcell * 'a lcell
and 'a lcell = ('a llist) ref

(* Constructing singleton linked lists, i.e. a linked list with just one
   element in it.
*)
let singleton (init: float * 'a): 'a lcell ref =
  let l = ref (Cons (init, ref Nil, ref Nil)) in
  let front = ref l in
  front

(* Converting linked lists to regular OCaml lists. *)
let rec list_of_lcell lcell =
  match !lcell with
  | Nil -> []
  | Cons (d, _, next) -> d :: list_of_lcell next

(* Comparing floating-point numbers. You might find this helpful when writing
   predicates for remove.
*)
let close x y = abs_float (x -. y) < 0.0001
  
  (* TODO: Write some test cases for map_tree. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let map_tree_tests: (((int -> int) * int tree) * int tree) list = [
  (* Remember: your test cases should have this form:
     ((f, t), output)
     The following test case asserts that:
       map_tree identity t
     should have the output
       t
  *)
  ((identity, t), t); ((add1,t),Node(2,Node(3,Empty,Empty,Empty),Node(4,Empty,Empty,Empty),Node(5,Empty,Empty,Empty)));
  ((identity, Node(2,Empty,Empty,Empty)), Node(2,Empty,Empty,Empty));
  ((identity, Empty), Empty)
];;


(* TODO: Implement map_tree. *)
let rec map_tree f t =
  match t with
  |Empty-> Empty
  |Node(a,x,y,z)->Node(f a, map_tree f x, map_tree f y, map_tree f z)
;;

(* TODO: Implement delete_data. *)
let delete_data t =
  map_tree (fun (x,y)-> x) t
;;

(* TODO: Write some test cases for fold_tree. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let fold_tree_tests:
  (((int * int * int * int -> int) * int * int tree) * int) list =
  [
  (* Remember: your test cases should have this form:
     ((f, e, t), output))
     Where:
     - f is a function of type int * int * int * int -> int
     - e has type int
     - t is a tree of type int tree
     - output has type int.
    *)
    (((fun _ -> 0), 3, t), 0);(((fun (x,y,z,t)->x+y+z+t),2,leaf 3),9);
    (((fun (x,y,z,t)->x+y+z+t),2,Empty),2);
    (((fun (x,y,z,t)->x+y+z+t),2,Node(2,leaf 1,Empty,Empty)),13)
  ]
;;

(* TODO: Implement fold_tree. *)
let rec fold_tree f e t =
  match t with
  |Empty->e
  |Node(a,x,y,z)->
      f (a,fold_tree f e x, fold_tree f e y, fold_tree f e z)
;;
(* TODO: Write some test cases for size. *)
let size_tests: (int tree * int) list = [
  (t,4);(Empty,0)
]
;;

(* TODO: Implement size. *)
let size t = 
  fold_tree (fun (_,l,m,r)->1+l+m+r) 0 t 
;; 
size t;;
(*size t;;*)
(* TODO: Write some test cases for reflect. *)
let reflect_tests: (int tree * int tree) list = [
  (t,Node(1,leaf 4,leaf 3, leaf 2));(Empty,Empty)
]
;;

(* TODO: Implement reflect. *)
let reflect t = 
  fold_tree (fun (a,l,m,r)->Node(a,r,m,l)) Empty t 
;; 
reflect t;;
(*reflect t;;*)
(* TODO: Write some test cases for postorder. *)
let postorder_tests: (int tree * int list) list = [
  (t,[2;3;4;1]);(Empty, [])
]
;;

(* TODO: Implement postorder. *)
let postorder t =
  fold_tree (fun (a,l,m,r)->l@m@r@[a]) [] t
;;
postorder t;;
(* TODO: Implement add_head. *)

let head=singleton (2.2,"a");;
list_of_lcell !head;;
let add_head (x:float*'a) (head:'a lcell ref): unit =
  match !head with
  |{contents = Nil} -> head := {contents = Cons(x,ref Nil, ref Nil)}
  |{contents = Cons ((_,_),c,_)}-> 
      c := Cons(x,ref Nil, !head );
      head := c 
;;
let head=singleton (2.2,"a");;
add_head (1.2,"b") head; add_head (3.3,"c") head;;
head;;
list_of_lcell !head;; 

(* TODO: Implement remove. *)
let remove (p:float->bool) (head:'a lcell ref):unit =
  let rec remove' ll =
    match ll with
    |{contents =Nil}-> ll := !ll
    |{contents =Cons((a,_),c,d)}-> 
        if p a then 
          match (c,d) with 
          |({contents=Nil},{contents=Nil})-> 
              ll := !c
          |({contents=Cons((a',b'),c',d')},{contents=Nil})->(*last element*) 
              c := Cons((a',b'),c', d); 
              ll := !c
          |({contents=Nil},{contents=Cons((a',b'),c',d')})-> (*head*) 
              d := Cons((a',b'), ref Nil, d'); 
              head := d 
              (*remove' ll*)
          |({contents=Cons((x,y),z,t)},{contents=Cons((a',b'),c',d')})-> 
              c := Cons((x,y),z,d); 
              d := Cons((a',b'),c,d'); 
              ll := !d
              (*remove' ll*)
        else 
          remove' d 
  in
  remove' !head
;;
add_head (4.4,"t") head;;
let x=singleton (2.3, "a");;
head;;
add_head (3.3,"a") x;;
remove (fun x -> x  < 5.4) head;;
head;;
list_of_lcell !head;;

