(*Lecture 15*)

type 'a susp=Susp of (unit->'a);;
let force (Susp f) = f ()
;;
type 'a str={hd: 'a; tl: 'a str susp}
;;

(*defining value, simulation, dont evaluate inside the fun*)
let rec zeroes : int str=
  {hd=0;tl= Susp (fun ()-> zeroes)}
;;

let rec count (n: int) :int str=
  {hd=n;
   tl=Susp (fun ()-> count (n+1))}
;;

let nats=count 0;;

let rec zip_with (f: 'a->'b->'c) (s1: 'a str) (s2: 'b str): 'c str=
  {hd= f s1.hd s2.hd;
   tl=
     Susp (fun ()->zip_with f (force s1.tl) (force s2.tl))(*use force because of the tl has type Susp, so need to force it to get rid of the susp*)
  }
;;

(*promise to add the hd but will add the tl later on when we demand it*)
let add s1 s2=
  zip_with (+) s1 s2
;;

let rec smap (f: 'a->'b) (s:'a str): 'b str=
  {hd= f s.hd
  ;tl=
     Susp (fun ()->smap f (force s.tl))
  }
;;

smap (fun x->x+1) zeroes;;
let rec take n (s: 'a str): 'a list =
  if n <=0 then 
    []
  else
    s.hd :: take (n-1) (force s.tl)
;;

take 15 (smap  (fun x->x+1) zeroes);;

let add_one =smap (fun x-> x+1);;

