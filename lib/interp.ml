open Term
open Pp_term

(*
here unified to use closure for both call-by-value and call-by-name

call-by-value: only functions need closure (env)
recursive closure: use FIX, separate from standard closure 
*)
type interpreter = term * env -> value

let rec find (x : string) = function
  | END -> failwith ("Unbound var " ^ x)
  | NEXT (y, t, e) -> if x = y then t else find x e

let rec interp_by_name : interpreter =
 fun (p, e) ->
  match p with
  | VAR x -> (
      match find x e with
      | VINT _ | VFUN _ | VPAIR _ -> find x e (* value *)
      | THUNK (t, e) -> interp_by_name (t, e)
      | _ -> failwith "store impossible values")
  | INT n -> VINT n
  | FUN (x, t) -> VFUN (x, t, e)
  | BOP (p1, op, p2) -> (
      let v1 = interp_by_name (p1, e) in
      let v2 = interp_by_name (p2, e) in
      match (v1, op, v2) with
      | VINT n1, ADD, VINT n2 -> VINT (n1 + n2)
      | VINT n1, MINUS, VINT n2 -> VINT (n1 - n2)
      | VINT n1, MULTI, VINT n2 -> VINT (n1 * n2)
      | VINT n1, DIVIDE, VINT 0 -> failwith "divide by zero"
      | VINT n1, DIVIDE, VINT n2 -> VINT (n1 / n2)
      | _ -> failwith "binary operands not integer")
  | IFZ (p1, p2, p3) -> (
      match interp_by_name (p1, e) with
      | VINT n ->
          if n = 0 then interp_by_name (p2, e) else interp_by_name (p3, e)
      | _ -> failwith "if condition not bool")
  | APP (p1, p2) -> (
      match interp_by_name (p1, e) with
      | VFUN (x, t, e1) -> interp_by_name (t, NEXT (x, THUNK (p2, e), e1))
      | _ -> failwith "not a function in application")
  | LET (x, p1, p2) -> interp_by_name (APP (FUN (x, p2), p1), e)
  | FIX (x, p1) -> interp_by_name (APP (FUN (x, p1), p), e)
  | PAIR (p1, p2) -> THUNK (p, e)
  | FST p -> (
      match interp_by_name (p, e) with
      | THUNK (PAIR (p1, _), e) -> interp_by_name (p1, e)
      | _ -> failwith "fst not pair")
  | SND p -> (
      match interp_by_name (p, e) with
      | THUNK (PAIR (_, p2), e) -> interp_by_name (p2, e)
      | _ -> failwith "snd not pair")
(* | _ -> failwith (Format.asprintf "impossible construct %a" pp_term p) *)

(* for fixed point operator, cannot interp by value *)
(* extended values *)
let rec interp_by_value : interpreter =
 fun (p, e) ->
  match p with
  | VAR x -> (
      match find x e with
      | VINT _ | VFUN _ | VPAIR _ -> find x e (* values *)
      | VFIX (y, t, e) -> interp_by_value (FIX (y, t), e) (* extended values*)
      | _ -> failwith "env not storing values")
  | INT n -> VINT n
  | FUN (x, t) -> VFUN (x, t, e)
  | BOP (p1, op, p2) -> (
      let v1 = interp_by_value (p1, e) in
      let v2 = interp_by_value (p2, e) in
      match (v1, op, v2) with
      | VINT n1, ADD, VINT n2 -> VINT (n1 + n2)
      | VINT n1, MINUS, VINT n2 -> VINT (n1 - n2)
      | VINT n1, MULTI, VINT n2 -> VINT (n1 * n2)
      | VINT n1, DIVIDE, VINT 0 -> failwith "divide by zero"
      | VINT n1, DIVIDE, VINT n2 -> VINT (n1 / n2)
      | _ -> failwith "binary operands not integer")
  | IFZ (p1, p2, p3) -> (
      match interp_by_value (p1, e) with
      | VINT n ->
          if n = 0 then interp_by_value (p2, e) else interp_by_value (p3, e)
      | _ -> failwith "if condition not bool")
  | APP (p1, p2) -> (
      match interp_by_value (p1, e) with
      | VFUN (x, t, e1) ->
          interp_by_value (t, NEXT (x, interp_by_value (p2, e), e1))
      | _ -> failwith "not a function in application")
  | LET (x, p1, p2) -> interp_by_value (APP (FUN (x, p2), p1), e)
  | FIX (x, p1) -> interp_by_value (p1, NEXT (x, VFIX (x, p, e), e))
  | PAIR (p1, p2) -> VPAIR (interp_by_value (p1, e), interp_by_value (p2, e))
  | FST p -> (
      match interp_by_value (p, e) with
      | VPAIR (v1, _) -> v1
      | _ -> failwith "fst not pair")
  | SND p -> (
      match interp_by_value (p, e) with
      | VPAIR (_, v2) -> v2
      | _ -> failwith "snd not pair")

(*
call-by-value interperter with recursive closures
*)
let rec interp_by_value_recur : interpreter =
 fun (p, e) ->
  match p with
  | VAR x -> find x e
  | INT n -> VINT n (* do not store extra env*)
  | FUN (x, t) -> VFUN (x, t, e)
  | BOP (p1, op, p2) -> (
      let v1 = interp_by_value_recur (p1, e) in
      let v2 = interp_by_value_recur (p2, e) in
      match (v1, op, v2) with
      | VINT n1, ADD, VINT n2 -> VINT (n1 + n2)
      | VINT n1, MINUS, VINT n2 -> VINT (n1 - n2)
      | VINT n1, MULTI, VINT n2 -> VINT (n1 * n2)
      | VINT n1, DIVIDE, VINT 0 -> failwith "divide by zero"
      | VINT n1, DIVIDE, VINT n2 -> VINT (n1 / n2)
      | _ -> failwith "binary operands not integer")
  | IFZ (p1, p2, p3) -> (
      match interp_by_value_recur (p1, e) with
      | VINT n ->
          if n = 0 then interp_by_value_recur (p2, e)
          else interp_by_value_recur (p3, e)
      | _ -> failwith "if condition not bool")
  | APP (p1, p2) -> (
      match interp_by_value_recur (p1, e) with
      | VFUN (x, t, e1) ->
          interp_by_value_recur (t, NEXT (x, interp_by_value_recur (p2, e), e1))
      | VFIXFUN (f, x, t, e1) ->
          interp_by_value_recur
            ( t,
              NEXT
                ( x,
                  interp_by_value_recur (p2, e),
                  NEXT (f, VFIXFUN (f, x, t, e1), e1) ) )
      | _ -> failwith "not a function in application")
  | LET (x, p1, p2) -> interp_by_value_recur (APP (FUN (x, p2), p1), e)
  | FIX (f, FUN (x, t)) -> VFIXFUN (f, x, t, e)
  | PAIR (p1, p2) ->
      VPAIR (interp_by_value_recur (p1, e), interp_by_value_recur (p2, e))
  | FST p -> (
      match interp_by_value (p, e) with
      | VPAIR (v1, _) -> v1
      | _ -> failwith "fst not pair")
  | SND p -> (
      match interp_by_value (p, e) with
      | VPAIR (_, v2) -> v2
      | _ -> failwith "snd not pair")
  | FIX _ ->
      failwith (Format.asprintf "impossible construct inside FIX %a" pp_term p)
