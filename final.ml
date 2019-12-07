(* ********************************************************************************************************************** *)
(* High order - croupier for simplified roulette *)

(* We have a simplified roulette, where we have only two colours that
   we can bet but if zero comes out, everyone loses *)

type colour = Red | Black        (* The two colours we can bet on *)

type result = colour option      (* The result of a run, it could be one of the colours or no colour if zero came up *)

type amt = int
type bet = amt * colour          (* The bet amount and to what colour *)

type id = string
type player = id * amt * bet option

(* It is simple to see who won *)
let compute (am, col : bet) : result -> int = function
  | None -> 0
  | Some col' -> if col = col' then am * 2 else 0

(*
Solve all these questions without using recursion or pattern
matching on lists, but instead just use the HO functions we saw
in class.
 *)

let players = [ ("Aliya", 1000, Some (400 , Red)) ;
              ("Jerome", 800, Some (240 , Black)) ;
              ("Mo" ,    900, Some (200, Black)) ;
              ("Andrea", 950, Some ((100 , Red)))]

(* Q1: given a list of players  compute the new amounts each player has and set their bets to None *)

(* Q2: given a list of bets and a result compute a list of winning players with their bets *)

(* Q3: given a list of bets and a result compute how much money the casino needs to pay back *)


(* Q4: given a list of bets and a result decide if nobody won *)

(* Q4.b: decide if everybody won *)

(* Q5: given a list of bets and a result compute if someone won *)

(* Q6: given a list of bets return the highest winning *)

(* Level-up (a bit more complicated) *)

(* Q7: given a list of bets and a result compute the balance for the casino, how much it made *)

(* Ninja level  *)

(* Q8: Can you sort the results by the amount they made? *)

                                 
(* ---------------------------------------------------- *)
(* Revisiting Exceptions and Continuations              *)
(* ---------------------------------------------------- *)
(* ---------------------------------------------------- *)
exception Change

(* change: : int list -> int -> int list *)
let rec change coins amt = 
  match coins with
  | _ when amt = 0 -> []
  | [] -> raise Change
  | c :: cs when c > amt ->
     change cs amt
  | c :: cs ->
     try
       c :: change cs (amt - c)
     with
       Change -> change cs amt

let change_top coins amt =
  try
    Some (change coins amt)
  with
    Change -> None


(* Giving change with continuations *)
let rec cchange (coins: int list) (amt:int)
          (sc: int list -> 'r) (fc: unit -> 'r) =
  match coins with
  | _ when amt = 0 -> sc []
  | [] -> fc ()
  | c :: cs when c > amt ->
     cchange cs amt sc fc
  | c :: cs ->
     cchange cs (amt - c)
       (fun ch -> sc (c :: ch))
       (fun () -> cchange cs amt sc fc)


let cchange_top coins amt = 
  cchange coins amt (fun ch -> Some ch) (fun () -> None)

(* Here is the behavior of change_top : 

# change_top [5 ; 2] 3;;
- : int list option = None
# change_top [5 ; 2] 8;;
- : int list option = Some [2; 2; 2; 2]
# change_top [25;10;5;2] 43;;
- : int list option = Some [25; 10; 2; 2; 2; 2]
# change_top [25;10;5;2] 44;;
- : int list option = Some [25; 10; 5; 2; 2]
# change_top [25;10;2] 44;;
Return the following change: 
- : int list option = Some [10; 10; 10; 10; 2; 2]
# change_top [25;10;2] 43;;
- : int list option = Some [25; 10; 2; 2; 2; 2]
# change_top [25;10;2] 23;;
- : int list option = None
# 
*)

  
(* ---------------------------------------------------- *)
(* Lazy Programming                                     *)
(* ---------------------------------------------------- *)
(* ---------------------------------------------------- *)
(* Suspended computation : we can suspend computation
   by wrapping it in a closure. *)
type 'a susp = Susp of (unit -> 'a)

(* delay: *)
let delay f = Susp(f)

(* force: *)
let force (Susp f) = f ()

(* ---------------------------------------------------- *)
(* Define an infinite stream via observations we can make 
   about it.
*)

type 'a str = {hd: 'a  ; tl : ('a str) susp} 

let rec str_ones = {hd = 1 ; tl = Susp (fun () -> str_ones)}

let rec numsFrom n = 
{hd = n ; 
 tl = Susp (fun () -> numsFrom (n+1))}

let nats = numsFrom 0

(* ---------------------------------------------------- *)
(* Inspect a stream up to n elements 

*)
let rec take_str n s = match n with 
  | 0 -> []
  | n -> s.hd :: take_str (n-1) (force s.tl)

(* ------------------------------------------------------- *)
(* Catalan Numbers *)

(* C_0     = 1
   C_(n+1) = 2(2n + 1)/(n+2) * C_n *)

let catalan =
  let rec go n cn =
    { hd = cn
    ; tl =
        Susp
          begin fun () ->
          let cn1 = 2 * (2 * n + 1) * cn / (n + 2) in
          go (n + 1) cn1
          end
    }
  in
  go 0 1
       
(* ---------------------------------------------------- *)
(* Objects and references                               *)
(* ---------------------------------------------------- *)

(** The state of a door. *)
type state = Open | Closed

(** Raised when a door cannot be passed. *)
exception Impassable

(** Raised when an invalid state change is requested,
    viz. open an already open door or close an already closed door.
 *)
exception InvalidState

type door =
  { set_state : state -> unit
  ; pass : unit -> bool
  }

(* Write a function make_door () to generate an object of type `door`.

   The set_state function should adjust the state of the door.  It
   should raise `InvalidState` if the door is already in the requested
   state.

   The function `pass` should return true if and only if one can pass
   through the door, i.e. if the door is open.

   The door should be initially closed. *)

(* make_door : unit -> door *) 
let make_lock () =
  let s = ref Closed in
  { set_state =
      (fun s' -> if !s = s' then raise InvalidState else s := s')
  ; pass = fun () -> !s = Open
  }

(* ---------------------------------------------------- *)
(* ---------------------------------------------------- *)
(* INDUCTION PROOF *)
(*
let rec size t = match t with 
  | Leaf -> 0 
  | Node (l, x, r) -> x + size l + size r

let rec size_acc t acc = match t with 
  | Leaf -> acc
  | Node (l, x, r) -> size_acc l (x + size_acc  r acc)  

(*
THEOREM:  size t + acc = size_acc t acc 

*)
*)






(* ---------------------------------------------------- *)
(* THANK YOU - IT WAS A LOT OF FUN!                     *)
(* ---------------------------------------------------- *)

