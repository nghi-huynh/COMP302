(*Lecture 11*)

exception Domain;;
let fact n=
  if n<0 then
    raise Domain
  else
    let rec f n= 
      if n=0 then 1
      else n*f (n-1)
    in
    f n
;;

let run_fact n=
  try
    fact n(*type return must be the same*)
  with (*instead of catch*)
  | Domain -> 
      print_string "That's not possible";
      1 (*ends program with 0 success 1 error, type of raising Exception, so type alpha*) 
;;

run_fact 2;;
run_fact (-2);;
                            
let run_fact n=
  try
    let k=fact n in(*try to make the try block to be as small as possible, otherwise it will be hard to catch all exception*)
    print_string(string_of_int n^"! = "
                 ^string_of_int k)(*type return must be the same*)
  with (*instead of catch*)
  | Domain -> 
      print_string "That's not possible";
      (*ends program with 0 success 1 error, type of raising Exception, so type alpha*) 
;;
let read_integer ()=(*cant do it on LearnOcaml since it closes right away*)
  let line= read_line () in
  int_of_string line
  
;;
           
let run_fact_user ()=
  (*use user input via read_integer*)
  let n= read_integer () in
  run_fact n
;;
type key=int;;
                    
type 'a btree=
  |Empty
  |Node of 'a btree * (key * 'a) * 'a btree
             
;;
exception NoSuchKey of key;;
(*Finds a key in a BST t, raising NoSuchKey if it isnt there*)
    
(*exception can have any type*)

let rec find t k=match t with
  |Empty-> raise (NoSuchKey k)
  |Node (l,(k',_),_) when k<k'->
      find l k 
  |Node (_,(k',_),r) when k>k'->
      find r k
  |Node (_,(_,x),_) ->
      x
;;                  
             
let find_top fmt t k=
  try
    let v=find t k in
    print_string("Found "^fmt v)
  with
  |NoSuchKey k->
      print_string("Key"^string_of_int k^"not found\n")
;;
exception BoundExceeded;;
let rec bounded_find t k n=match t with
  |_ when n=0-> raise BoundExceeded
  |Empty-> raise (NoSuchKey k)
  |Node (l,(k',_),_) when k<k'->
      find l k 
  |Node (_,(k',_),r) when k>k'->
      find r k
  |Node (_,(_,x),_) ->
      x
;;       

let rec listToString l =match l with
  |[]->""
  |[x]->string_of_int x
  |x::xs-> string_of_int x^", "^ listToString xs
;;

exception Change;;
(*using coins, make change for amt using a greedy algorithm, doesnt necessarily produce optimal answer*)
let rec change coins amt=
  match coins with
  | _ when amt=0->[]
  |[]-> raise Change
  |c::coins' when c<= amt-> 
      begin
        try 
          c::change coins (amt-c)(*coin c is good so let use c to solve the proble*)
        with
        |Change-> change coins' amt
      end
  |_::coins ->
      change coins amt(*coin c is not good, so need to backtrack further*)
;;
change [20;3;2] 24;;
change [20;5] 22;
                             
                    
