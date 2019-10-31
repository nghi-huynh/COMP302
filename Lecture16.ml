(*Lecture 16*)

type 'a susp=Susp of (unit->'a);;
module type LAZY=sig
  type 'a susp
  val force: 'a susp->'a
  val delay: (unit->'a)->'a susp
end 
;;

module Memo0: LAZY=struct
  type 'a susp=Susp of (unit->'a)
  let force (Susp f) =f ()
  let delay f=Susp f
end
;;

(*Memoizing the suspension. Idea: use a reference cell holding an option to store the result once it has been computed*)

module Memo1: LAZY=struct
  type 'a susp =Susp of (unit->'a)
  let delay f=
    let box=ref None in
    Susp 
      begin fun ()->
        match ! box with (*match the content in box, if it is None then call it and update the content; return the x*)
        |None-> let x=f () in
            box :=Some x;
            x
        |Some x->x (*if it has some content, then just return it*)
      end
  let force (Susp f) =f ()
end
;;

module Memo2: LAZY= struct
  (* the suspended computation is inside a ref cell*)
  type 'a susp=Susp of (unit->'a) ref
  let delay f=
    let rec s=ref (*make a ref to store a function then update itself*)
        begin fun ()->
          let x=f () in
          s := (fun ()->x);
          x
        end
    in
    Susp s
    
    
  let force (Susp f)= ! f ()
end
;;
type 'a str={hd: 'a; tl: 'a str susp}
;;
let force (Susp f) = f ()
;;
let rec zeroes : int str=
  {hd=0;tl= Susp (fun ()-> zeroes)}
;;

let rec count (n: int) :int str=
  {hd=n;
   tl=Susp (fun ()-> count (n+1))}
;;
let rec zip_with (f: 'a->'b->'c) (s1:'a str) (s2: 'b str):'c str=
  {hd= f s1.hd s2.hd;
   tl=
     Susp (fun ()->zip_with f (force s1.tl) (force s2.tl))(*use force because of the tl has type Susp, so need to force it to get rid of the susp*)
  }
;;
let rec take n (s: 'a str): 'a list =
  if n <=0 then 
    []
  else
    s.hd :: take (n-1) (force s.tl)
;;
let rec fibs=
  {hd = 0;
   tl = Susp (fun ()->fibs')}
and fibs'=
  {hd = 1; 
   tl = Susp (fun ()-> zip_with (+) fibs fibs')}
;;
let rec sfilter p s=
  let h,t =find_next p s in
  {hd=h;
   tl= Susp (fun  ()-> sfilter p (force t))} 
and find_next p s=
  if p s.hd then
    s.hd, s.tl
  else
    find_next p (force s.tl)
;;
(*n |||k=true'false decides whether n is divisible by k*)
let (|||) n k=(n mod k=0);;
let rec sieve s=
  { hd=s.hd;
    tl=Susp (fun ()->sieve (sfilter (fun x-> not (x ||| s.hd)) (force s.tl)))}
;;
take 100 (sieve (count 2));;
type 'a lazy_list=
  {hd: 'a;
   tl: 'a fin_list susp}
and 'a fin_list= Empty |NonEmpty of 'a lazy_list
;;

let rec take n (s: 'a lazy_list): 'a list=
  if n<=0 then 
    []
  else
    s.hd :: take' (n-1) (force s.tl)
      
and take' n (s: 'a fin_list):'a list=
  match s with
  |Empty->[]
  |NonEmpty s'-> take n s'
;;

let rec append' s1 s2=
  match s1 with
  |Empty->s2
  |NonEmpty s1'->NonEmpty (append s1' s2)
and append s1 s2=
  {hd= s1.hd;
   tl=Susp (fun ()-> append' (force s1.tl) s2)}
;;



open Memo2;;
let horribleComp n=for i=0 to n do () done;n ;;

let x=delay (fun ()->horribleComp 200000000);;
let y=force x+ force x ;;

