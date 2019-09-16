(*Lecture 4*)

(*2 ways: plain and sugary*)

let l=[];;
let l2=3 :: (4 :: [] );;

let l3=[3;4];;

(*pattern matching on lists*)
let l=[1;2;3;4;5];;
let l=1 :: (2::3::4::5::[]);;
let head (l: 'a list):'a=
  match l with
  | [] -> failwith "uhoh"
            (*or assert false*)
  |x :: xs -> x;;
(*head : 'a list -> 'a*)
let tail l=
  match l with
  | [] -> failwith "uhoh"
            (*or assert false*)
  |x :: xs -> xs;;
(*tail: 'a list -> 'a list*)

let startswith1 l=
  match l with
  |1::_-> true
  |_->false;;

(*recursion on lists*)
let rec length (l:'a list): int=
  match l with 
  |[]->0
  |x::xs->
      let n=length xs in
      n+1;;
let rec length_tr' l acc=
  match l with
  |[]->acc
  |x::xs-> 
      length_tr' xs (1+acc)
;;

let length_tr l=length_tr' l 0;;

(*reverse a list*)
let rec rev l=
  match l with
  |[]->[]
  |x::xs-> rev xs @ [x];;

let rev_tr l=
  let rec go l acc=
    match l with
    |[]->acc
    |x::xs ->go xs (x::acc)
  in
  go l [];;

(*merging 2 lists*)

let rec merge (l1:int list) (l2:int list): int list=
  match (l1,l2) with
  |([],ys)->ys
  |(xs,[])->xs
  |(x::xs,y::ys)->
      if x<=y then x:: merge xs (y::ys)
      else if y<x then y::merge (x::xs) ys
      else assert false;;

(*split a list*)

let rec split (l1:'a list):'a list*'a list=
  match l1 with
  |[] -> ([], [])
  |[x]->([x],[])
  |x1::x2::xs->
      let (xs',ys')=split xs in
      (x1::xs',
       x2::ys');;

(*zip 2 lists*)
let rec zip l1 l2=
  match (l1,l2) with
  |([],l2)->l2
  |(l1,[])->l1
  |(x::xs,y::ys)->
      x::y::zip xs ys;;

zip [2;5] [5;3];;

