(*Lecture 8*)

type 'a tree=
  |Empty
  |Node of 'a tree*'a*'a tree 
;;

(*retreiving the value associated with a given key*)
let rec lookup (k:'a) (t: ('a*'b) tree) :'b option=
  match t with
  | Empty -> (*raise KeyNotFound*) None
  | Node (l,(k',v),r)->
      if k=k' then
        Some v
      else
      if k<k' then
        lookup k l
      else
        lookup k r
;;

(*insert a key to the tree*)
let rec insert (k,v: 'a*'b) (t:('a*'b) tree): ('a*'b) tree=
  match t with
  |Empty-> Node (Empty, (k,v), Empty)
  |Node (l, (k',v'),r)->
      if k=k' then
        Node (l, (k,v), r)
      else
      if k<k' then
        (*let l'=insert (k,v) l in Node (l',(k',v'),r)*)
        Node (insert (k,v) l, (k',v'), r)
      else
        Node (l,(k',v'),insert (k,v) r)
;;
(*Theorem: for all t: ('a*'b) tree, k:'a,v:'b, lookup k (insert (k,v) t)=Some v*)

             
let rec filter p = function
  | [] -> []
  | h :: t -> 
      if p h then h :: filter p t 
      else filter p t
;;
let rec series (<=>) f (a,b) inc acc=
  if a>=b then
    acc
  else
    series (<=>) f (inc a, b) 
      inc (f a <=> acc)
;;

let rec integral f n (a,b)=(*n: number of rectangles*)
  let dx=(b-.a)/.n in
  series (+.) (fun x ->dx*.f x) 
    (a,b)
    (fun x-> x+.dx)
    0.0
;;
let curry f=fun a b-> f (a,b);; 
let uncurry f=fun (a,b)->f a b;;
let flip (f:'a->'b->'c):'b->'a->'c=fun y x-> f x y;;
let id (x:'a):'a=x;;

uncurry (flip (curry id));;
flip (curry id);;
let swap: 'a*'b->'b*'a=
  uncurry (flip (curry id)) 
;;
let deriv (f,e)=fun x->(f (x+.e)-.f x)/.e;;
curry deriv;;
curry deriv (fun x->x*.x) 0.0001;;
deriv ((fun x->x*.x),0.0001) 1.;;

let anti_d f= curry (integral f 0.);;

List.fold_left;;
List.map;;
List.filter;;
List.map2;;
List.for_all;;
List.fold_right;;

(*let diag f l=
   let f acc=
     if f (List.tl acc) then
      
     else
      
   in*)
    (* List.filter (List.hd l) f l;;*)
let l=[[1;0;0];[0;1;0];[0;0;1]];;
let rec diagonal m=
  match m with
  |[]->true
  |r::rs->
      let all_zero=
        List.for_all (fun x->x=0)
      in
      all_zero (List.tl r)
      && all_zero (List.map List.hd rs)
      && diagonal (List.map List.tl rs)
;;
      
let curry f x y=f (x,y);;
let uncurry f (x,y)=f x y;;

let p=(3,("hello","world")) in
let q=snd p in
let x=fst q in
x
;;
