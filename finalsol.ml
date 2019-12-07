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

(* Q1:  given a list of players  compute the new amounts each player has and set their bets to None *)

let compute_all_results l r =
  List.map
    (function
     | (id, amt, Some b) ->
        let win = compute b r in
        (id, amt + win, None )
     | (id, amt, None) -> (id, amt, None))
    l

(* Q2: given a list of bets and a result compute a list of winning players with their bets *)
let compute_winners l r =
  List.filter
    (function
     | (id, amt, Some b) -> compute b r > 0
     | (id, amt, None) -> false)
    l

(* Q3: given a list of bets and a result compute how much money the casino needs to pay back *)
let casino_payout l r =
  List.fold_right
    (fun player acc ->
      match player with
      | (_ , _ , Some b) -> (compute b r) + acc
      | (_ , _ , None) -> acc)
    l 0

(* Q4: given a list of bets and a result compute if nobody won *)
let nobody_wins l r =
  List.for_all
    (function
     | (_, _, Some b) -> (compute b r) = 0
     | (_, _, None) -> true)
    l

(* Q4: everybody won !*)

(* Q5: given a list of bets and a result compute if someone won *)
let some_winners l r =
  List.exists
    (function
     | (_ , _ , Some b) -> compute b r > 0
     | ( _ , _ , None) -> false)
    l

(* Q6: given a list of bets return the highest amount *)

(* Returns 0 in case no bets are placed. *)
let highest bs =
  List.fold_right (fun (a, _) acc -> if a > acc then a else acc) bs 0

(* Level-up (a bit more complicated) *)

(* Q7: given a list of bets and a result compute the balance for the casino, how much it made *)

(* Ninja level  *)

(* Q8: Can you sort the results by the amount they made? *)
