open Db_term

type dbinterpreter = dbterm * dbenv -> dbvalue

let rec find (i : int) = function
  | DBEND -> failwith ("Unbound var pos" ^ string_of_int i)
  | DBNEXT (t, e) -> if i = 0 then t else find (i - 1) e

let rec dbinterp_by_name : dbinterpreter =
 fun (p, e) ->
  match p with
  | DBVAR i -> (
      match find i e with
      | DBTHUNK (t, e) -> dbinterp_by_name (t, e)
      | _ -> find i e)
  | DBINT n -> VDBINT n
  | DBFUN t -> VDBFUN (t, e)
  | DBBOP (p1, op, p2) -> (
      let v1 = dbinterp_by_name (p1, e) in
      let v2 = dbinterp_by_name (p2, e) in
      match (v1, op, v2) with
      | VDBINT n1, ADD, VDBINT n2 -> VDBINT (n1 + n2)
      | VDBINT n1, MINUS, VDBINT n2 -> VDBINT (n1 - n2)
      | VDBINT n1, MULTI, VDBINT n2 -> VDBINT (n1 * n2)
      | VDBINT n1, DIVIDE, VDBINT 0 -> failwith "divide by zero"
      | VDBINT n1, DIVIDE, VDBINT n2 -> VDBINT (n1 / n2)
      | _ -> failwith "binary operands not integer")
  | DBIFZ (p1, p2, p3) -> (
      match dbinterp_by_name (p1, e) with
      | VDBINT n ->
          if n = 0 then dbinterp_by_name (p2, e) else dbinterp_by_name (p3, e)
      | _ -> failwith "if condition not bool")
  | DBAPP (p1, p2) -> (
      match dbinterp_by_name (p1, e) with
      | VDBFUN (t, e1) -> dbinterp_by_name (t, DBNEXT (DBTHUNK (p2, e), e1))
      | _ -> failwith "not a function in application")
  | DBLET (p1, p2) -> dbinterp_by_name (DBAPP (DBFUN p2, p1), e)
  | DBFIXFUN p1 -> dbinterp_by_name (DBFUN p1, DBNEXT (DBTHUNK (p, e), e))
  | DBPAIR (p1, p2) -> DBTHUNK (p, e)
  | DBFST p -> (
      match dbinterp_by_name (p, e) with
      | DBTHUNK (DBPAIR (p1, _), e) -> dbinterp_by_name (p1, e)
      | _ -> failwith "fst not pair")
  | DBSND p -> (
      match dbinterp_by_name (p, e) with
      | DBTHUNK (DBPAIR (_, p2), e) -> dbinterp_by_name (p2, e)
      | _ -> failwith "snd not pair")

(* for fixed point operator, cannot interp by value *)
(* extended values *)
let rec dbinterp_by_value : dbinterpreter =
 fun (p, e) ->
  match p with
  | DBVAR i -> find i e
  | DBINT n -> VDBINT n
  | DBFUN t -> VDBFUN (t, e)
  | DBBOP (p1, op, p2) -> (
      let v1 = dbinterp_by_value (p1, e) in
      let v2 = dbinterp_by_value (p2, e) in
      match (v1, op, v2) with
      | VDBINT n1, ADD, VDBINT n2 -> VDBINT (n1 + n2)
      | VDBINT n1, MINUS, VDBINT n2 -> VDBINT (n1 - n2)
      | VDBINT n1, MULTI, VDBINT n2 -> VDBINT (n1 * n2)
      | VDBINT n1, DIVIDE, VDBINT 0 -> failwith "divide by zero"
      | VDBINT n1, DIVIDE, VDBINT n2 -> VDBINT (n1 / n2)
      | _ -> failwith "binary operands not integer")
  | DBIFZ (p1, p2, p3) -> (
      match dbinterp_by_value (p1, e) with
      | VDBINT n ->
          if n = 0 then dbinterp_by_value (p2, e) else dbinterp_by_value (p3, e)
      | _ -> failwith "if condition not bool")
  | DBAPP (p1, p2) -> (
      match dbinterp_by_value (p1, e) with
      | VDBFUN (t, e1) ->
          dbinterp_by_value (t, DBNEXT (dbinterp_by_value (p2, e), e1))
      | VDBFIXFUN (t, e1) ->
          dbinterp_by_value
            ( t,
              DBNEXT (dbinterp_by_value (p2, e), DBNEXT (VDBFIXFUN (t, e1), e1))
            )
      | _ -> failwith "not a function in application")
  | DBLET (p1, p2) -> dbinterp_by_value (DBAPP (DBFUN p2, p1), e)
  | DBFIXFUN p1 -> VDBFIXFUN (p1, e)
  | DBPAIR (p1, p2) ->
      VDBPAIR (dbinterp_by_value (p1, e), dbinterp_by_value (p2, e))
  | DBFST p -> (
      match dbinterp_by_value (p, e) with
      | VDBPAIR (v1, _) -> v1
      | _ -> failwith "fst not pair")
  | DBSND p -> (
      match dbinterp_by_value (p, e) with
      | VDBPAIR (_, v2) -> v2
      | _ -> failwith "snd not pair")
