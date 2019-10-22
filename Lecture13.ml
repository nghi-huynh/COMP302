(*Lecture 13*)

let rec append l1 l2=match l1 with
  |[]->l2
  |h::t->
      let l'=append t l2 in 
      h::l'
;;

(*append, tail recursive, using a continuation*)
let rec app_tr (l1: 'a list) (l2: 'a list) (k: 'a list-> 'r): 'r=
  match l1 with
  |[]-> k l2
  |h::t-> app_tr t l2 (fun l'-> k (h::l'))
            
;;
let app' l1 l2=app_tr l1 l2 (fun x->x);;

let len_app l1 l2=
  app_tr l1 l2 (fun l' -> List.length l');;

app_tr [1;2;3] [4;5;6] (List.map (fun x->2*x));;


type 'a tree=
  |Empty
  |Node of 'a tree* 'a* 'a tree
;;

let rec find (p: 'a->bool) (t: 'a tree)=
  match t with
  |Empty-> None
  |Node (l,d,r) when p d-> Some d
  |Node (l,_,r)->
      match find p l with
      |None-> find p r
      |Some d-> Some d
;;

let rec find_tr p t (fail: unit->'r) (succeed: 'a->'r) :'r=
  match t with
  |Empty-> fail ()
  |Node(_,d,_) when p d-> succeed d
  |Node(l,_,r)->
      find_tr p l (fun ()-> find_tr p r fail succeed) succeed
;;

let find_tr' p (t:'a tree):'a option=
  find_tr p t (fun ()-> None) (fun d-> Some d)
;;
(*Theorem find_tr' p t= find p t*)

let elim_option (none: unit->'b) (some: 'a->'b) (o: 'a option)=
  match o with
  |None-> none ()
  |Some x-> some x
;;
(*Theorem: 
  for any functions none: unit ->'b and some:'a->'b
  for any predicate p: 'a-> bool
  for any tree t: 'a tree
  we have
  elim_option none some (find p t)= find_tr p t none some
*)
  
