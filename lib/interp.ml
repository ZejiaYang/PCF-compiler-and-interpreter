open Term

type env = END | NEXT of string * closure * env
and closure = term * env

(*
here unified to use closure for both call-by-value and call-by-name

call-by-value: only functions need closure (env)
recursive closure: use FIX, separate from standard closure 
*)
type interpreter = closure -> closure

let rec find (x : string) = function
  | END -> failwith ("Unbound var " ^ x)
  | NEXT (y, t, e) -> if x == y then t else find x e

let rec interp_by_name : interpreter =
 fun (p, e) ->
  match p with
  | VAR x ->
      let t, e = find x e in
      interp_by_name (t, e)
  | INT _ -> (p, END) (* do not store extra env*)
  | FUN _ -> (p, e)
  | BOP (p1, op, p2) -> (
      let v1, _ = interp_by_name (p1, e) in
      let v2, _ = interp_by_name (p2, e) in
      let e = END in
      match (v1, op, v2) with
      | INT n1, ADD, INT n2 -> (INT (n1 + n2), e)
      | INT n1, MINUS, INT n2 -> (INT (n1 - n2), e)
      | INT n1, MULTI, INT n2 -> (INT (n1 * n2), e)
      | INT n1, DIVIDE, INT 0 -> failwith "divide by zero"
      | INT n1, DIVIDE, INT n2 -> (INT (n1 / n2), e)
      | _ -> failwith "binary operands not integer")
  | IFZ (p1, p2, p3) -> (
      match interp_by_name (p1, e) with
      | INT n, _ ->
          if n == 0 then interp_by_name (p2, e) else interp_by_name (p3, e)
      | _ -> failwith "if condition not bool")
  | APP (p1, p2) -> (
      match interp_by_name (p1, e) with
      | FUN (x, t), e1 -> interp_by_name (t, NEXT (x, (p2, e), e1))
      | _ -> failwith "not a function in application")
  | LET (x, p1, p2) -> interp_by_name (APP (FUN (x, p2), p1), e)
  | FIX (x, p1) -> interp_by_name (APP (FUN (x, p1), p), e)

(* for fixed point operator, cannot interp by value *)
(* extended values *)
let rec interp_by_value : interpreter =
 fun (p, e) ->
  match p with
  | VAR x -> (
      let y, e = find x e in
      match y with
      | INT _ | FUN _ -> (y, e) (* values *)
      | FIX _ -> interp_by_value (y, e) (* extended values*)
      | _ -> failwith "env not storing values")
  | INT _ -> (p, END) (* do not store extra env*)
  | FUN _ -> (p, e)
  | BOP (p1, op, p2) -> (
      let v1, _ = interp_by_value (p1, e) in
      let v2, _ = interp_by_value (p2, e) in
      let e = END in
      match (v1, op, v2) with
      | INT n1, ADD, INT n2 -> (INT (n1 + n2), e)
      | INT n1, MINUS, INT n2 -> (INT (n1 - n2), e)
      | INT n1, MULTI, INT n2 -> (INT (n1 * n2), e)
      | INT n1, DIVIDE, INT 0 -> failwith "divide by zero"
      | INT n1, DIVIDE, INT n2 -> (INT (n1 / n2), e)
      | _ -> failwith "binary operands not integer")
  | IFZ (p1, p2, p3) -> (
      match interp_by_value (p1, e) with
      | INT n, _ ->
          if n == 0 then interp_by_value (p2, e) else interp_by_value (p3, e)
      | _ -> failwith "if condition not bool")
  | APP (p1, p2) -> (
      match interp_by_value (p1, e) with
      | FUN (x, t), e1 ->
          interp_by_value (t, NEXT (x, interp_by_value (p2, e), e1))
      | _ -> failwith "not a function in application")
  | LET (x, p1, p2) -> interp_by_value (APP (FUN (x, p2), p1), e)
  | FIX (x, p1) -> interp_by_value (p1, NEXT (x, (p, e), e))

(*
call-by-value interperter with recursive closures
*)
let rec interp_by_value_recur : interpreter =
 fun (p, e) ->
  match p with
  | VAR x -> find x e
  | INT _ -> (p, END) (* do not store extra env*)
  | FUN _ -> (p, e)
  | BOP (p1, op, p2) -> (
      let v1, _ = interp_by_value_recur (p1, e) in
      let v2, _ = interp_by_value_recur (p2, e) in
      let e = END in
      match (v1, op, v2) with
      | INT n1, ADD, INT n2 -> (INT (n1 + n2), e)
      | INT n1, MINUS, INT n2 -> (INT (n1 - n2), e)
      | INT n1, MULTI, INT n2 -> (INT (n1 * n2), e)
      | INT n1, DIVIDE, INT 0 -> failwith "divide by zero"
      | INT n1, DIVIDE, INT n2 -> (INT (n1 / n2), e)
      | _ -> failwith "binary operands not integer")
  | IFZ (p1, p2, p3) -> (
      match interp_by_value_recur (p1, e) with
      | INT n, _ ->
          if n == 0 then interp_by_value_recur (p2, e)
          else interp_by_value_recur (p3, e)
      | _ -> failwith "if condition not bool")
  | APP (p1, p2) -> (
      match interp_by_value_recur (p1, e) with
      | FUN (x, t), e1 ->
          interp_by_value_recur (t, NEXT (x, interp_by_value_recur (p2, e), e1))
      | FIX (f, FUN (x, t)), e1 ->
          interp_by_value_recur
            ( t,
              NEXT
                ( x,
                  interp_by_value_recur (p2, e),
                  NEXT (f, interp_by_value_recur (p1, e), e1) ) )
      | _ -> failwith "not a function in application")
  | LET (x, p1, p2) -> interp_by_value_recur (APP (FUN (x, p2), p1), e)
  | FIX (x, p1) -> interp_by_value_recur (p1, NEXT (x, (p, e), e))
