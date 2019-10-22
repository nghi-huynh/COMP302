(*Lecture 12*)
module type STACK = sig
  type el(*abstract type*)
  type t(*stack*)
      (*declaring , decorating using val( decoration), whereas let is defining*)
  val push: el->t->t
  val pop: t->(t*el) option
  val empty: t   
end
;;
  (*interface implementing*)
module Stack : (STACK with type el=int)= struct
  (*still need to re-define it*)
  type el = int
  type t =el list
      
      (*implement push pop and empty*)
  let empty = []
  let push e s= e::s
  let pop s: (t*el) option=
    match s with
    |[]->None
    |e::s'->Some (s',e)
end
;;

Stack.push 3 Stack.empty;;

Stack.(push 3 empty);;
(*locally import the module stack*)
let open Stack in push 3 empty;;

(*global import*)
open Stack;;
push 3 empty;;

let s=empty;;
let s=push 3 s;;
pop s;;
(*make module more generic*)
(*introduce functor*)
(*appear inside is decoration, only say the type cant give implementation*)
module type TYPE= sig
  type t
end 
;; 
    
(*concrete module, can implement*)
module ListStack (T: TYPE): (STACK with type el=T.t) =
  (*functor (T: TYPE)->struct*)
struct
  type el=T.t
  type t= el list
        
  let push e s=e::s
  let pop s=match s with
    |[]->None
    |e::s-> Some(s,e)
  let empty=[]
end
;;
module Char=
struct 
  type t=char 
end
;;
module CharStack=
  ListStack (Char)(*(struct type t=char end) *)
;;

(*implement set*)
type comparison=Less|Equal|Greater
;;

module type ORDERED=sig
  type t
  val compare: t->t-> comparison
end
;;

module IntLt: (ORDERED with type t=int)=struct
  type t=int
  let compare n1 n2=
    match compare n1 n2 with(*from standard library*)
    |0-> Equal
    |(-1)->Less
    |1->Greater
    |_-> failwith "compare impossible"
end
;;

module ListLt: ORDERED->ORDERED=
  functor (Elt: ORDERED)->
  struct
    type t=Elt.t list
    let rec compare l1 l2=match l1,l2 with
      |[],[]->Equal
      |[],_->Less
      |_,[]->Greater
      |x::xs, y::ys->
          begin 
            match Elt.compare x y with
            |Less->Less
            |Greater->Greater
            |Equal->compare xs ys
          end
  end
;;

module type SET=sig
  type element
  type t
  val empty:t
  val add: element->t->t
  val member: element->t->bool
end
;;
(*module Set (Elt:ORDERED): (SET with type element=Elt.t)=struct
   type element=Elt.t
   type t=element list
   let rec add e = function
     |[]->[e]
     |x::s->
         match Elt.compare e x with
         |Less| Equal->e::x::s
         |Greater->x::add e s 
      
 end
 ;;*)
let point = (ref 4, ref 3);;
let point' = point;;
let component_x (x,y) = x;;
let component_y (x,y) = y;;

let shift point =
  let x = component_x point in
  let y = component_y point in
  x := !x + 1 ; y := !y + 1
;;

let add p1 p2 =
  let x1 = component_x p1 in
  let y1 = component_y p1 in
  let x2 = component_x p2 in
  let y2 = component_y p2 in
  x1 := !x2 + !x1 ; y1:= !y2 + !y1
;;
shift point; add point point'; point'

