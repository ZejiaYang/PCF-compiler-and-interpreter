open Term

type dbterm =
  | DBVAR of int
  | DBFUN of dbterm
  | DBAPP of dbterm * dbterm
  | DBINT of int
  | DBBOP of dbterm * op * dbterm
  | DBIFZ of dbterm * dbterm * dbterm
  | DBFIXFUN of dbterm (*recursive closure*)
  | DBLET of dbterm * dbterm

type var_env = VEND | VNEXT of string * var_env

type dbenv = END | NEXT of dbvalue * dbenv

and dbvalue =
  | VDBINT of int
  | VDBFUN of dbterm * dbenv
  | VDBFIXFUN of dbterm * dbenv
  | DBTHUNK of dbterm * dbenv

let rec find_pos (x : string) (venv : var_env) =
  let rec find_pos_acc x venv pos =
    match venv with
    | VEND -> failwith ("Unbound var " ^ x)
    | VNEXT (y, venv) -> if x == y then pos else find_pos_acc x venv (pos + 1)
  in
  find_pos_acc x venv 0

let rec translate_db (t : term) (venv : var_env) : dbterm =
  match t with
  | VAR x -> DBVAR (find_pos x venv)
  | INT n -> DBINT n
  | FUN (x, t) -> DBFUN (translate_db t (VNEXT (x, venv)))
  | BOP (p1, op, p2) ->
      let db_p1 = translate_db p1 venv in
      let db_p2 = translate_db p2 venv in
      DBBOP (db_p1, op, db_p2)
  | IFZ (p1, p2, p3) ->
      let db_p1 = translate_db p1 venv in
      let db_p2 = translate_db p2 venv in
      let db_p3 = translate_db p3 venv in
      DBIFZ (db_p1, db_p2, db_p3)
  | APP (p1, p2) ->
      let db_p1 = translate_db p1 venv in
      let db_p2 = translate_db p2 venv in
      DBAPP (db_p1, db_p2)
  | LET (x, p1, p2) ->
      let db_p1 = translate_db p1 venv in
      let db_p2 = translate_db p2 (VNEXT (x, venv)) in
      DBLET (db_p1, db_p2)
  | FIX (f, FUN (x, t)) ->
      DBFIXFUN (translate_db t (VNEXT (x, VNEXT (f, venv))))
  | _ ->
      failwith
        "illegal construct, this is a langauge with only functions can be \
         recursively defined"
