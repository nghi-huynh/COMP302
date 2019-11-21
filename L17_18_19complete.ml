(*Lecture 17,18,19*)
let counter = ref 0
let freshVar x =
  incr counter;
  x ^ string_of_int !counter

module Exp = struct
  type primop = Equals | LessThan | Plus | Times | Negate

  type tp = IntTy | BoolTy | ArrowTy of tp * tp

  let rec typ_to_string t = match t with 
    | IntTy -> "Int"
    | BoolTy -> "Bool"
    | ArrowTy (t1, t2) -> "(" ^ typ_to_string t1 ^ " -> " ^ typ_to_string t2 ^ ")"
            
  type name = string

  type exp =
    | Int of int                      (* 0 | 1 | 2 | ... *)
    | Bool of bool                    (* true | false *)
    | If of exp * exp * exp           (* if e then e1 else e2 *)
    | Primop of primop * exp list     (* e1 <op> e2  or  <op> e *)
    | Fun of tp * name * exp
    | Var of name
    | Apply of exp * exp
    | Let of dec * exp
  and dec =
    | Val of name * exp

  type subst = exp * name
end        

module Eval = struct 
  open Exp
     
  exception Stuck of string 
  let fail msg = raise (Stuck msg)

  let delete x = List.filter (fun x' -> x' <> x)

  let union vs1 vs2 =
    List.fold_right
      (fun x acc -> if List.mem x vs2 then acc else x :: acc)
      vs1
      vs2

  let unions vss = List.fold_left union [] vss

  let rec free_variables = function
    | Int _ -> []
    | Bool _ -> []
    | Primop (_, es) -> unions (List.map free_variables es)
    | If (e, e1, e2) -> unions (List.map free_variables [e; e1; e2])
    | Fun (_, x, e) -> delete x (free_variables e)
    | Var x -> [x]
    | Apply (e1, e2) -> unions (List.map free_variables [e1; e2])
    | Let (Val (x, e), e') -> union (free_variables e) (delete x (free_variables e'))
                   
  let rec subst ((e', x) as s) exp =
    match exp with
    | Var y when x = y -> e'
    | Var _ -> exp
    | Int n -> Int n
    | Bool b -> Bool b
    | Primop (po, args) -> Primop (po, List.map (subst s) args)
    | If (e, e1, e2) ->
        If (subst s e, subst s e1, subst s e2)
    | Let (Val (y, e1), e2) ->
        let e1' = subst s e1 in
        if y = x then
          Let (Val (y, e1'), e2)
        else
          let (y, e2) =
            if List.mem y (free_variables e') then
              rename y e2
            else
              (y, e2)
          in
          Let (Val (y, e1'), subst s e2)
         
    | Fun (_, y, _) when y = x -> exp
    | Fun (ty, y, e) ->
        let (y, e) =
          if List.mem y (free_variables e') then
            rename y e
          else
            (y, e)
        in
        Fun (ty, y, subst s e)

    | Apply (e1, e2) ->
        Apply (subst s e1, subst s e2)
                     
  and rename x e =
    let x' = freshVar x in
    (x', subst (Var x', x) e)

  let evalOp op = match op with
    | (Equals,   [Int i; Int i']) -> Some (Bool (i = i'))
    | (LessThan, [Int i; Int i']) -> Some (Bool (i < i'))
    | (Plus,     [Int i; Int i']) -> Some (Int (i + i'))
    | (Times,    [Int i; Int i']) -> Some (Int (i * i'))
    | (Negate,   [Int i])         -> Some (Int (-i))
    | _                           -> None
                                   
  let rec eval e = match e with 
    | Int _ -> e
    | Bool _ -> e
    | If(e, e1, e2) ->
        begin match eval e with
          | Bool true -> eval e1
          | Bool false -> eval e2
          | _ -> raise (Stuck "guard is not a bool")
        end
    (* primitive operations +, -, *, <, = *)
    | Primop (po, args) ->
        let argvalues = List.map eval args in
        begin match evalOp (po, argvalues) with
          | None -> raise (Stuck "Bad arguments to primitive operation")
          | Some v -> v
        end
    | Var x -> fail ("Free variable in program: " ^ x)
    | Apply (e1, e2) ->
        begin match eval e1 with
          | Fun (_, x, e) ->
              let v = eval e2 in
              eval (subst (v, x) e)
          | _ ->
              fail "Application LHS not a function"
        end
    | Fun (ty, x, e) -> Fun (ty, x, e)
    | Let (Val (x, e), e') ->
        let v = eval e in
        eval (subst (v, x) e')
end

module Types = struct
  open Exp
           
  exception TypeError of string

  let fail message = raise (TypeError message)
  let mismatch exp act =
    "Type mismatch: expected " ^ typ_to_string exp ^ " got " ^ typ_to_string act

  (** Context Gamma ::= . | Gamma, x : T *)
  type ctx = Empty | Snoc of ctx * (name * tp)

  let extend ctx b = Snoc (ctx, b)

  (** Lookup Gamma x = Some T if x can be found in Gamma.
      Prioritizes more recent bindings, so overshadowing is automatic.
  *)
  let rec lookup ctx x =
    match ctx with
    | Empty -> None
    | Snoc (_, (x', t)) when x = x' -> Some t
    | Snoc (ctx, _) -> lookup ctx x
                   
  (* primopType op = (argTypes, returnType) *)
  let primopType op : tp list * tp =
    match op with
    | Equals -> [ IntTy; IntTy ], BoolTy
    | LessThan -> [ IntTy; IntTy ], BoolTy
    | Plus -> [ IntTy; IntTy ], IntTy
    | Times -> [ IntTy; IntTy ], IntTy
    | Negate -> [ IntTy ], IntTy
                  
  let rec infer ctx e : tp =
    match e with
    | Int _ -> IntTy
    | Bool _ -> BoolTy
    | If (e, e1, e2) ->
        let t_e = infer ctx e in
        if t_e <> BoolTy then
          fail (mismatch BoolTy t_e ^ " for `if` condition");
        let t_1, t_2 = infer ctx e1, infer ctx e2 in
        if t_1 <> t_2 then
          fail (mismatch t_1 t_2 ^ " for second `if` branch");
        t_1
    | Primop (op, args) ->
        let ts = List.map (infer ctx) args in
        let ts_exp, rt = primopType op in

       (* check that the right number of arguments is given;
          otherwise, List.map2 below will crash. *)
        let n_ts, n_ts_exp = List.length ts, List.length ts_exp in
        if n_ts <> n_ts_exp then
          fail ("wrong number of arguments for primitive operation: expected "
                ^ string_of_int n_ts_exp
                ^ " got "
                ^ string_of_int n_ts);
       (* now check each type one at a time, to give a good error message *)
        List.iter2
          begin fun t_exp t ->
            if t_exp <> t then
              fail (mismatch t_exp t ^ " in primitive operation argument")
          end
          ts_exp ts;
        rt
    | Var x ->
        begin match lookup ctx x with
          | Some t -> t
          | None -> fail ("unbound variable: " ^ x)
        end
    | Apply (e1, e2) ->
        begin match infer ctx e1 with
          | ArrowTy (t1, t2) ->
              let t = infer ctx e2 in
              if t <> t1 then
                fail (mismatch t1 t ^ " in function argument");
              t2
          | t ->
              fail ("Type error: expected a function type, got "
                    ^ typ_to_string t ^ " in a function application")
        end
    | Fun (t_a, x, e) ->
        let t_b = infer (extend ctx (x, t_a)) e in
        ArrowTy (t_a, t_b)
    | Let (Val (x, e), e') ->
        let t = infer ctx e in
        infer (extend ctx (x, t)) e'
end 


module E = Exp
let e1 = E.If (E.Primop (E.Equals, [E.Int 3; E.Int 2]), 
               E.Primop (E.Plus, [E.Int 5 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]),
               E.Primop (E.Plus, [E.Int 1 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]))

let e2 = E.If (E.Primop (E.Equals, [E.Int 3; E.Bool true]), 
               E.Primop (E.Plus, [E.Int 5 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]),
               E.Primop (E.Plus, [E.Int 1 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]))

let e3 = E.(Fun (IntTy, "x", Var "x"))
let e4 = E.(Fun (IntTy, "x", Fun (IntTy, "y", Primop (Plus, [Var "x"; Var "y"]))))

let e5 =
  let open E in
  Apply
    ( Apply (e4, Int 4)
    , Int 6
    )

(** Define composition of functions of type IntTy -> IntTy.
    Minilang does not have polymorphism, so we cannot define a general
function composition.
*)
let compose =
  let open E in
  Fun ( ArrowTy (IntTy, IntTy), "f",
        Fun ( ArrowTy (IntTy, IntTy), "g",
              Fun ( IntTy, "x",
                    Apply (Var "f", (Apply (Var "g", Var "x"))))))

let square =
  let open E in
  Fun ( IntTy, "n", Primop (Times, [Var "n"; Var "n"]))

(** Define an infix operator for Apply so we can write applications
    more cleanly.
    This operator is left-associative, so
    f */ x */ y === Apply (Apply (f, x), y)
    which is the usual associativity for function application.
*)
let ( */ ) e1 e2 = E.Apply (e1, e2)

(** Function that calculates x^4 by composing
    the `square` function with itself. *)
let ex8 = 
  let open E in
  Let
    ( Val ("compose", compose),
      Let
        ( Val ("square", square),
          E.Fun ( IntTy, "n", Var "compose" */ Var "square" */ Var "square" */ Var "n" )))
 
          
          
