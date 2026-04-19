open Term

type env = END | NEXT of string * thunk * env
and thunk = term * env

type closure = term * env

let rec find (x : string) = function
  | END -> failwith "cannot find x in env"
  | NEXT (y, t, e) -> if x == y then t else find x e

let rec interp_by_name ((p, e) : closure) : closure =
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
