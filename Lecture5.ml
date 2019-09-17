(*Lecture 5*)

(*Abstract over function*)
(*implement square and cube , exp funct*)
let id x=x;;
let rec sum f (a,b)=
  if a>b then
    0
  else
    f a + sum f (a+1,b)
;;

(*syntax for passing funct to funct*)

(*let f=fun x-> ;;*)

(*sum (fun x-> exp (2,x)) (1,5);;*)

let twice f x =f (f x);;
let double (*x*)=twice (fun x -> x*2) (*x*);;


let  series comb f (a,b) inc acc=
  let rec go a acc=
    if a>b then
      acc
    else
      go (inc a) (comb (f a) acc)
  in
  go a acc
;;

let sum f (a,b)=
  series (fun x y-> x+y)(*or (+)*) f (a,b) (fun x->x+1) 0
;;

series (fun x y->x::y) id (1,5) (fun x ->x+1) [];;


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

let rec map: ('a->'b)->
  'a list->
  'b list=
  fun f l ->
    match l with
    |[]->[]
    |x::xs->
        let y=f x in
        let ys=map f xs in
        y :: ys
          (*  f x :: map f xs*)
;;

map (fun x-> [x;x]) [1;2;3;4];;

(*let rec filter 
    (p:'a->bool)
    (l:'a list)
  :'a list=
  (*filter p xs keeps all elements of xs that satisfy p;
    e.g
      filter (fun x->x mod 2=0) [1;2;3;4;5]
    => [2;4]*)
  fun f l->
     match l with
     |[]->[]
     |x::xs->
         let y=f x in
         let ys= filter f xs in
         y::ys
   ;;*)

