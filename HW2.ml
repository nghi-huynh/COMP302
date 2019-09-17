exception NotImplemented

(* The type of binary numbers. *)
type bnum =
  | E
  | Zero of bnum
  | One of bnum

(* The type of propositions. *)
type prop =
  | Atom of string
  | Neg of prop
  | Conj of prop * prop
  | Disj of prop * prop
  | Impl of prop * prop

(* TODO: Write a good set of tests for psum. *)
let psum_tests = [([1;2;1],[1;3;4]);([0;1;4],[0;1;5]);([1],[1]);([],[])];;

(* TODO: Implement psum. *)
let psum l =
  let rec s l acc=
    match l with
    |[]->[]
    |[x]->[acc+x]
    |x::xs->(acc+x):: s xs (acc+x)
  in
  s l 0
;;

(* TODO: Write a good set of tests for intToBin. *)
let intToBin_tests = [(6,Zero (One (One E)));(4, Zero (Zero (One E)));(1, One E);(0,E)];;

(* TODO: Implement intToBin. *)
let rec intToBin (n:int):bnum =
  match (n,n mod 2) with
  |(0,0)->E
  |(1,0)->One E
  |(x,0)->Zero(intToBin (x/2))
  |(x,1)->One( intToBin (x/2))
  |(x,_)-> intToBin (x/2)                                         
;;


(* TODO: Write a good set of tests for binToInt. *)
let binToInt_tests = [(E,0);(Zero (One (One (Zero E))),6);(Zero (Zero (One E)),4);(One E,1)];;

(* TODO: Implement binToInt. *)
let binToInt (b:bnum):int =
  let rec bt b y acc=
    match b with
    |E->truncate y 
    |One (x)->bt x (2.0**(acc)+.y) (acc+.1.0)
    |Zero (x)->bt x y (acc+.1.0)
  in
  bt b 0.0 0.0
;;

(* TODO: Write a good set of tests for nnf. *)
let nnf_tests = [(Conj(Atom "p",Neg(Disj (Atom "q",Atom"r"))),Conj ( Atom "p",Conj(Neg(Atom "q"),Neg(Atom"r"))));
                 (Neg (Neg (Atom"a")),Atom "a");
                 (Impl(Neg (Atom "a"),Neg (Atom "b")),Disj (Atom "a",Neg (Atom "b"))); 
                 (Neg(Disj(Neg(Atom "a"),Neg(Atom "b"))), Conj(Atom "a", Atom "b"));
                 (Neg(Conj(Neg((Neg(Atom "a"))),Atom "b")), Disj(Neg(Atom "a"), Neg(Atom "b"))); 
                 (Neg(Neg(Conj(Atom "a",Neg(Disj( Atom "b",Atom "c"))))),Conj(Atom "a",Conj(Neg(Atom "b"),Neg(Atom "c"))));
                 (Neg(Impl(Neg(Atom "a"),Neg(Atom "b"))),Conj (Neg(Atom "a"),Atom "b"))

                ];;
(* TODO: Implement nnf. *)
let rec nnf p  = 
  match p with 
  |Atom x->Atom x
  |Neg(Neg(x))-> nnf x
  |Neg(x)->Neg(nnf x) 
  |Disj(x,y)->Disj(nnf x,nnf y)
  |Conj(x,y)->Conj(nnf x, nnf y)
  |Impl(x,y)->Disj(Neg(nnf x),nnf y) 
;;
