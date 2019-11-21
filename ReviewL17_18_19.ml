(*Lecture 17,18,19*)
type primop=Equals|LessThan|Plus|Minus|Times|Negate;;

type exp=
  |Bool of bool
  |Int of int
  |If of exp*exp*exp(*if e then e1 else e2*)
  |Primop of primop*exp list(*e1 <op> e2 or <op> e*)
;;

let rec evalOp op arg_list=match op, arg_list with
  |Equals,[Int i;Int i']-> Some (Bool (i=i'))
  |LessThan, [Int i;Int i']->Some (Bool (i<i'))
  |Plus, [Int i;Int i']->Some (Int (i+i'))
  |Minus,[Int i;Int i']->Some (Int (i-i'))
  |Times,[Int i;Int i']->Some (Int (i*i'))
  |Negate,[Int i;Int i']->Some (Int (-i)) 
  |_->None
;;

exception Stuck of string;;

(*eval: exp->exp*)
let rec eval e=match e with
  |Int n->Int n
  |Bool b->Bool b
  |Primop (po, args)->
      begin
        match evalOp po (List.map eval args) with
        |None->raise (Stuck "Unexpected arguments to primitive operation")
        |Some v->v
      end
  |If (e,e1,e2)->
      begin
        match eval e with
        |Bool true->eval e1
        |Bool false->eval e2
        |_->raise (Stuck "Expected boolean value")
      end
;;
         
