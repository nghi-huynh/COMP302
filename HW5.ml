exception NotImplemented
exception Fail

(* Interface for our representations of metrics. *)
module type METRIC =
sig
  type t

  val unit: t
  val plus: t -> t -> t
  val prod: float -> t -> t
  val toString: t -> string
  val toFloat: t -> float
  val fromFloat: float -> t
end

(* A simple representation of the Hour unit. Note that the type t is abstract.
   Ideally we would use an implementation of the METRIC type to write this
   module.
   The Hour module is hard-coded here for grading purposes.
*)
module Hour:
  sig
    type t
    val toFloat: t -> float
    val fromFloat: float -> t
  end
= struct
  type t = float

  let toFloat x = x
  let fromFloat x = x
end

(* Interface for a module computing speeds using a fixed metric. *)
module type SPEED =
sig
  type s
  type distance

  val speed: distance -> Hour.t -> s
  val average: s list -> s
  val toFloat: s -> float
  val speed_as_float: distance -> float -> float
  val average_as_float: float list -> float
end

(* The type of graphs. *)
type 'a graph = {
  nodes: 'a list;
  edges: ('a * 'a) list
}
  
(* TODO: Implement the functions in this module.
   Do not change the type signature!
*)
module Float: (METRIC with type t = float) =
struct
  type t = float

  (* TODO: Update this value. *)
  let unit = 1.

  (* TODO: Implement the following functions. *)
  let plus = (+.)
  let prod = ( *. )
  let toString = string_of_float
  let toFloat x = x
  let fromFloat x = x
end
;;


(* TODO: Use the module Float to create different representations for:
   Meter, KM, Feet, and Miles.
   Uncomment the following and replace the "???" with your solution.
*)

module Meter = (Float: METRIC);;
module KM = (Float: METRIC);;
module Feet = (Float: METRIC);;
module Miles = (Float: METRIC);;

(* TODO: Implement the functor Speed. *)
module Speed (M: METRIC): (SPEED with type distance = M.t) =
struct
  type s = float
  type distance = M.t

  (* TODO: Implement the following functions. *)
  let speed (m: distance) (h: Hour.t) =
    (M.toFloat m) /. (Hour.toFloat h)

  let average s =
    (List.fold_left (+.) 0. s)/.(float_of_int (List.length s))

  let toFloat s = s
    

  (* You should not modify this code: it is here for testing purposes. *)
  let speed_as_float m h = toFloat (speed m (Hour.fromFloat h))
  let average_as_float s = toFloat (average s)
end
;;
(*(List.fold_left (fun x y->(x+.y)) 0. [1.;2.;3.])/.3.;;*)

(* TODO: Use the functor Speed to create modules for computing miles per hour
   and kilometers per hour. Uncomment the following and replace the "???"
   with your solution.
*)

module MilesPerHour = Speed (Miles);;
module KMPerHour = Speed (KM);;

(* Do not remove this line from your code: we need it for testing. *)
module TestModule = Speed (Float)

(* TODO: Write some tests for neighbours. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * string list) list = [
  (({nodes=["A";"B";"C"]; edges=[("A","C");("C","B")]},"A"),["C"]);
  (({nodes=["A";"B";"C"]; edges=[("C","A");("C","B")]},"A"),[]);
  (({nodes=["B";"A"]; edges=[("A","A");("A","B")]},"A"),["A";"B"])
]
;;

(* TODO: Implement neighbours. *)
let neighbours g vertex:'a list =
  let m= List.filter (fun (x,_)-> x=vertex ) g.edges in
  List.map (fun (_,y)-> y) m
;;
let g={nodes=["A";"B";"C"]; edges=[("A","C");("C","B")]};;
let m=List.filter (fun (x,_)-> x="A" ) g.edges;;
List.map (fun (_,y)->y) m;;

g.edges;;

(* TODO: Implement find_path. *)
let a={nodes = ["Vancouver"; "New York"; "Boston"; "Houston"; "Atlanta"];
       edges =
         [("Boston", "Atlanta"); ("New York", "Atlanta"); ("New York", "Boston");
          ("Houston", "Atlanta"); ("Vancouver", "Atlanta");
          ("Atlanta", "Houston"); ("Boston", "New York"); ("Atlanta", "New York");
          ("Boston", "Houston"); ("Vancouver", "New York")]}
;;
a.nodes;;
a.edges;;
neighbours a "Vancouver";;
neighbours a "Atlanta";;
neighbours a "New York";;
neighbours a "Boston";;
neighbours a "Houston";;

let find_path g a b =
  let rec aux_node node visited= 
    match (node,visited) with
    |(_,[])-> false
    |(a,x::xs)-> a=x || aux_node a xs
            (*List.mem node visited*)
  and aux_list nodes visited =
    match nodes with
    |[]->raise Fail
    |x::xs->if aux_node x visited then aux_list xs visited
        else if (x=b) then [x]
        else try aux_list xs (x::visited)
          with Fail -> x::aux_list (neighbours g x) (x::visited)
  in
  aux_list [a] [] 
;;

(* TODO: Implement find_path'. *)
let find_path' g a b =
  let rec aux_node node visited fc sc =
    match (node,visited) with
    |(_,[])-> fc false
    |(a,x::xs)->if a=x then sc
        else aux_node a xs (fun _-> aux_node a xs fc sc ) sc
  and aux_list nodes visited fc sc =
    match nodes with
    |[]-> fc (raise Fail)
    |x::xs-> if aux_node x visited (fun _-> false) (true) then aux_list xs visited fc sc
        else if (x=b) then [x]
        else (*aux_list xs (x::visited) (fun _-> x::aux_list (neighbours g x) (x::visited) fc sc) sc*) 
          try aux_list xs (x::visited) fc sc 
          with Fail-> 
            fc (find_path g a b)
            
  in
  aux_list [a] [] (fun x -> x) []
;;
List.mem "NO" a.nodes;; 

find_path a "Vancouver" "Atlanta";;
find_path' a "Vancouver" "Atlanta" ;;
find_path a "Vancouver" "Boston";; 
find_path' a "Vancouver" "Boston" ;;
