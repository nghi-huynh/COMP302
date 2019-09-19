(*Lecture 6*)
(*let rec filter p l=
   match l with
   |[]->[]*)
       
       

let rec filter p=
  function (*match the list right away, create a function, introduce a function which match right away*)
  |[]->[] 
      (*if p x then
         x::filter p xs
       else
         filter p xs
*)
  |x::xs when p x->
      x::filter p xs
        (*if use when not (p x), then have to introduce _ to cover all cases*)
  |x::xs -> 
      filter p xs
;;
let is_even x=x mod 2=0;;

filter (fun x->x mod 2=0) [1;2;3;4;5];;

let curry (f: 'a*'b->'c):'a->'b->'c
  =(*fun a b -> f (a,b)*)
  fun a b-> f (a,b)
;;
let uncurry (f:'a->'b->'c):'a*'b->'c=
  fun (x,y) ->f x y
;;
let flip (f:'a->'b->'c):'b->'a->'c
  =fun b a->f a b
;;

let id (x:'a):'a=x;;
(*id also has type 'a*'b->'a*'b*)

let swap : 'a*'b->'b*'a(*use curry, uncurry, flip, id*)
  =  uncurry (flip (curry id)) (*: 'a*'b->'b*'a*) (*weak polymorphic*)
;;
(*id : 'a*'b->'a*'b
  curry id: 'a->'b->'a*'b
  curry id 3 4
    (fun x y->id (x,y)) 3 4
  (fun y->id (3,y)) 4
  (id (3,4))
*)
(*         f             epsilon*)
(*deriv: (float->float)*float->(float->float*)
  (*                            f*)
let deriv (f,e)=
  fun x->
    (f (x+.e)-.f x)/.e
;;

deriv ((fun x->x*.x),0.0001) 1.;;
curry deriv (fun x->x*.x) 0.0001 1.;;

let f'=curry deriv (fun x->x*.x) 0.00001;;

(*power*)
let rec pow k n=
  if k=0 then 
    1
  else
    n*pow (k-1) n
;;

let rec pow_gen k=
  if k=0 then
    fun n -> 1
  else
    let p =pow_gen (k-1) in
    fun n -> n * p n
;;

pow_gen 2 4;;
(*
pow_gen 2
==>
   let p=pow_gen 1
   in
   fun n->n* p n
==>
   let p=fun n->1 in
   fun n-.n*p n
==>
   fun n->n*(fun n->n*(fun n->1) n) n
  

