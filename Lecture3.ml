(*Lecture 3*) 
let p =(3,("hello","world")) in (*tuple*)
let q =snd p in
let x=fst q in
x;;

type suit= (*define new type suit consists of 4 constructors*)
  |Clubs
  |Spades
  |Hearts
  |Diamonds;;

let dom (s1: suit) (s2: suit) : bool=match (s1,s2) with
  | (Spades, Spades ) -> failwith " Wrong suit "
  | (Spades, _) -> true
  | (Hearts, a2) -> not (a2= Spades) (*a2 refers to value in s2*)
  | (Diamonds, (Diamonds | Clubs))-> true
  | (Clubs, Clubs)-> true
  | _ -> false;; (*raise all possible cases otherwise it will raise mistmatch error*)

                 

type rank = One | Two |Three | Four |Five |Six |Seven |Eight |Nine |Ten| Ace |King |Queen |Jade;;

type card= rank*suit;;(*not tuple, instead it's of type rank and suit*)
                      
type hand =(*recursive types/ inductive types b/c they are described inductively*)
  | Empty
  | Hand of card*hand;;

(*function extract s h computes a hand containing all cards from h of suit s*)
let rec extract (s : suit) (h: hand) :hand=match h with
  |Empty -> Empty
  |Hand ((r,s'),h') when s =s' -> Hand ((r,s'), extract s h') 
  |Hand (_,h') -> extract s h';; (* can replace with if s=s' then _ else _*)
(*tail recursive since it doesnt compute anything else beside the recursive call*)
 
(*finds the first card in hand of the specified rank and returns its corresponding suit*)                                
let rec find (r,h) : card option=match h with
  |Empty -> None (*handle exception by type card option*)
  |Hand ((r',s),h') -> 
      if r=r' then
        Some (r',s)(*card option*)
      else(*if rank is not equal then not the card looking for*)
        find (r,h') ;;(*perform recursive call but no further computation -> tail recursive*)
          
          (*is this tail recursive?*)
        
let rec count h (*acc*)=
  match h with
  |Empty -> 0 (*acc*)
  |Hand (_, h') -> (*acc+1*)1 + count h';; (*not tail recursion b/c it follows by a computation*)


let hand0 :hand =Empty;;
let hand1: hand =Hand ((Ace, Hearts),Empty);;
let hand2: hand =Hand ((Queen, Diamonds), hand1);;
let hand5       =Hand ((Ace,Spades),
                       Hand((Ten, Diamonds),
                            Hand((Seven, Clubs),
                                 Hand((Queen, Spades),
                                      Hand((Eight, Clubs), Empty)))));;

extract Diamonds hand5;;

find (Queen,hand5);; (*hits on the first Queen*)
                     
count hand5;;
