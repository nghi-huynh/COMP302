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
psum [1;2;3;4;5];;
(* TODO: Write a good set of tests for intToBin. *)
let intToBin_tests = [(6,Zero (One (One E)));(4, Zero (Zero (One E)));(1, One E);(0,E)];;

(* TODO: Implement intToBin. *)
let rec intToBin n =
  raise NotImplemented

(* TODO: Write a good set of tests for binToInt. *)
let binToInt_tests = [(E,0);(Zero (One (One (Zero E))),6);(Zero (Zero (One E)),4);(One E,1)];;

(* TODO: Implement binToInt. *)
let rec binToInt b =
  raise NotImplemented

(* TODO: Write a good set of tests for nnf. *)
let nnf_tests = [(Conj(Atom "p",Neg(Disj (Atom "q",Atom"r"))),Conj ( Atom "p",Conj(Neg(Atom "q"),Neg(Atom"r"))));
                 (Neg (Neg (Atom"a")),Atom "a");
                 (Impl(Neg (Atom "a"),Neg (Atom "b")),Disj (Atom "a",Neg (Atom "b"))); 
                 (Neg(Disj(Neg(Atom "a"),Neg(Atom "b"))), Conj(Atom "a", Atom "b"));
                 (Neg(Conj(Neg((Neg(Atom "a"))),Atom "b")), Disj(Neg(Atom "a"), Neg(Atom "b"))); 
                ];;
(* TODO: Implement nnf. *)
let rec nnf p =
  raise NotImplemented
