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

             
