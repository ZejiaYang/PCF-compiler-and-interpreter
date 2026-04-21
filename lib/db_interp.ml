open Db_term

type dbenv = END | NEXT of dbclosure * dbenv
and dbclosure = dbterm * dbenv

type dbinterpreter = dbclosure -> dbclosure

let rec find (i : int) = function
  | END -> failwith ("Unbound var pos" ^ string_of_int i)
  | NEXT (t, e) -> if i == 0 then t else find (i - 1) e

let rec dbinterp_by_name : dbinterpreter =
 fun (p, e) ->
  match p with
  | DBVAR i ->
      let t = find i e in
      dbinterp_by_name t
  | DBINT _ -> (p, END) (* do not store extra env*)
  | DBFUN _ -> (p, e)
  | DBBOP (p1, op, p2) -> (
      let v1, _ = dbinterp_by_name (p1, e) in
      let v2, _ = dbinterp_by_name (p2, e) in
      let e = END in
      match (v1, op, v2) with
      | DBINT n1, ADD, DBINT n2 -> (DBINT (n1 + n2), e)
      | DBINT n1, MINUS, DBINT n2 -> (DBINT (n1 - n2), e)
      | DBINT n1, MULTI, DBINT n2 -> (DBINT (n1 * n2), e)
      | DBINT n1, DIVIDE, DBINT 0 -> failwith "divide by zero"
      | DBINT n1, DIVIDE, DBINT n2 -> (DBINT (n1 / n2), e)
      | _ -> failwith "binary operands not integer")
  | DBIFZ (p1, p2, p3) -> (
      match dbinterp_by_name (p1, e) with
      | DBINT n, _ ->
          if n == 0 then dbinterp_by_name (p2, e) else dbinterp_by_name (p3, e)
      | _ -> failwith "if condition not bool")
  | DBAPP (p1, p2) -> (
      match dbinterp_by_name (p1, e) with
      | DBFUN t, e1 -> dbinterp_by_name (t, NEXT ((p2, e), e1))
      | _ -> failwith "not a function in application")
  | DBLET (p1, p2) -> dbinterp_by_name (DBAPP (DBFUN p2, p1), e)
  | DBFIXFUN p1 -> dbinterp_by_name (DBFUN p1, NEXT ((p, e), e))

(* for fixed point operator, cannot interp by value *)
(* extended values *)
let rec dbinterp_by_value : dbinterpreter =
 fun (p, e) ->
  match p with
  | DBVAR i -> find i e
  | DBINT _ -> (p, END) (* do not store extra env*)
  | DBFUN _ -> (p, e)
  | DBBOP (p1, op, p2) -> (
      let v1, _ = dbinterp_by_value (p1, e) in
      let v2, _ = dbinterp_by_value (p2, e) in
      let e = END in
      match (v1, op, v2) with
      | DBINT n1, ADD, DBINT n2 -> (DBINT (n1 + n2), e)
      | DBINT n1, MINUS, DBINT n2 -> (DBINT (n1 - n2), e)
      | DBINT n1, MULTI, DBINT n2 -> (DBINT (n1 * n2), e)
      | DBINT n1, DIVIDE, DBINT 0 -> failwith "divide by zero"
      | DBINT n1, DIVIDE, DBINT n2 -> (DBINT (n1 / n2), e)
      | _ -> failwith "binary operands not integer")
  | DBIFZ (p1, p2, p3) -> (
      match dbinterp_by_value (p1, e) with
      | DBINT n, _ ->
          if n == 0 then dbinterp_by_value (p2, e) else dbinterp_by_value (p3, e)
      | _ -> failwith "if condition not bool")
  | DBAPP (p1, p2) -> (
      match dbinterp_by_value (p1, e) with
      | DBFUN t, e1 ->
          dbinterp_by_value (t, NEXT (dbinterp_by_value (p2, e), e1))
      | DBFIXFUN t, e1 ->
          dbinterp_by_value
            ( t,
              NEXT
                (dbinterp_by_value (p2, e), NEXT (dbinterp_by_value (p1, e), e1))
            )
      | _ -> failwith "not a function in application")
  | DBLET (p1, p2) -> dbinterp_by_value (DBAPP (DBFUN p2, p1), e)
  | DBFIXFUN p1 -> dbinterp_by_value (DBFUN p1, NEXT ((p, e), e))
