(*Lecture 9*)

(*Review midterm*)
let rec replicate n s =
  if n=0 then
    ""
  else
    s^replicate (n-1) s
;;
(*let rec f x= f (f x) in f "302"*)
 (*still have type string for the whole expression*) 
   
(*Recall variable binding and overshadowing*)
let k=4;;
let k=3 in k*k;;
k;;
(*allocating, comparing, reading, writing reference*)
let pi=ref 3.14;;(*type of references, cell in memory that can be modified*)
                 
let area r=r*.r*. !pi;;(*bang pi to let the reference falls out/dereference the pi*)
area 2.;;
                 
pi := 3.;;(*in case we're in indiana*)
area 2.;;

let pi'=ref !pi;;
pi==pi';;(*physical equal refers to different memory location*)
                                  
pi=pi';;  (*structurally equal*)
(*r1=r2 <-> !r1=!r2*)

type student=
  {name:string;id:string;mutable age:int}
;;

let jake={name="jake";id="secret";age= -1};;

jake.age <- 0;;(*doesnt have a type, only have an effect*)
               
type student'={age: int ref};;

let j={age= ref 1};;
j.age := 5;;

(*define a new type*)
type 'a myref={mutable contents' : 'a};;(*defing general cell *)
                                        
let read {contents' = x} =x;; 
(*read :'a myref -> 'a*)

let write r x: unit=
  r.contents' <- x;;(*get*)
(*pattern matching*)                   
let ref' x={ contents' = x};;
let grades =ref' [95;75;40];;

write grades [100;100;100];;

grades;;

(*<>, physically not equal*)
1 <> 2;;
1 != 2;;

(*imperative programming*)

let imp_fact n=
  let result =ref 1 in
  for i=1 to n do
    result := i * !result
  done;(*binary operator*)
  !result
;;

let counter =ref 0;;
let tick ()=
  counter := !counter + 1;
  !counter
;;

let gen_name prefix t ()=
  let n =t () in
  prefix^string_of_int n
;;
let gen_jake=gen_name "jake" tick ();;(*global variable*)
