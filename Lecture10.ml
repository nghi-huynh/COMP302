(*Lecture 10*)
let x=ref(3+2) in x ;;

type counter=
  {tick: unit->int; reset: unit->unit}
;;
let new_counter ()=(*method*)
  let c=ref 0 in
  {tick=(fun ()-> c := !c + 1; !c);reset=(fun ()->c := 0)}
;;

let c=new_counter ();;
c.tick();;
c.tick();;

c.reset();;
c.tick();;

(*and to make it mutable*)
type 'a rlist=Empty |RCons of 'a*'a reflist and 'a reflist='a rlist ref;;
(*converts an ordinary list into a ref list*)
let rec of_list: 'a list -> 'a reflist=
  function
  |[]-> ref Empty
  |x::xs-> ref (RCons (x,of_list xs))
;;
of_list [1;2;3;4];;

(*rapp: 'a reflist-> 'a reflist ->unit| appends one reflist to another*)
let rec rapp (l1: 'a reflist) (l2: 'a reflist) =match !l1 with
  |Empty-> l1 := !l2
  |RCons (x,xs)->rapp xs l2
;;
let l1=of_list [1;2;3;4];;
let l2=of_list [5;6;7;8];;
rapp l1 l2;;
(*to_list l1;;*)
(*demand when we need l2*)
let rec rapp_gen l1=
  match !l1 with
  |Empty->fun l2-> l1 := !l2
  |RCons (x,xs)->(*let f=rapp_gen xs in
                  fun l2 -> f l2*)
      rapp_gen xs
      
;;
