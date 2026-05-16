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
  (* -- Pair -- *)
  | DBPAIR of dbterm * dbterm
  | DBFST of dbterm
  | DBSND of dbterm
  (* -- List -- *)
  | DBNIL
  | DBCONS of dbterm * dbterm
  | DBIFNIL of dbterm * dbterm * dbterm
  | DBHD of dbterm
  | DBTL of dbterm
  (* -- Tree --*)
  | DBLEAF of dbterm
  | DBTREE of dbterm * dbterm
  | DBITEM of dbterm
  | DBIFLEAF of dbterm * dbterm * dbterm
  | DBLTREE of dbterm
  | DBRTREE of dbterm
[@@deriving show, eq]

type var_env = VEND | VNEXT of string * var_env

type dbenv = DBEND | DBNEXT of dbvalue * dbenv

and dbvalue =
  | VDBINT of int
  | VDBFUN of dbterm * dbenv
  | VDBFIXFUN of dbterm * dbenv
  | DBTHUNK of dbterm * dbenv
  (* -- Pair -- *)
  | VDBPAIR of dbvalue * dbvalue
  (* -- List -- *)
  | VDBNIL
  | VDBCONS of dbvalue * dbvalue
  (* -- Tree -- *)
  | VDBLEAF of dbvalue
  | VDBTREE of dbvalue * dbvalue
[@@deriving show, eq]

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
  | FIX _ ->
      failwith
        "illegal construct, this is a langauge with only functions can be \
         recursively defined"
  | PAIR (p1, p2) -> DBPAIR (translate_db p1 venv, translate_db p2 venv)
  | FST p -> DBFST (translate_db p venv)
  | SND p -> DBSND (translate_db p venv)
  | NIL -> DBNIL
  | CONS (p, l) -> DBCONS (translate_db p venv, translate_db l venv)
  | IFNIL (p1, p2, p3) ->
      let db_p1 = translate_db p1 venv in
      let db_p2 = translate_db p2 venv in
      let db_p3 = translate_db p3 venv in
      DBIFNIL (db_p1, db_p2, db_p3)
  | HD p -> DBHD (translate_db p venv)
  | TL p -> DBTL (translate_db p venv)
  | LEAF p -> DBLEAF (translate_db p venv)
  | TREE (l, r) -> DBTREE (translate_db l venv, translate_db r venv)
  | ITEM p -> DBITEM (translate_db p venv)
  | IFLEAF (p1, p2, p3) ->
      let db_p1 = translate_db p1 venv in
      let db_p2 = translate_db p2 venv in
      let db_p3 = translate_db p3 venv in
      DBIFLEAF (db_p1, db_p2, db_p3)
  | LTREE p -> DBLTREE (translate_db p venv)
  | RTREE p -> DBRTREE (translate_db p venv)

let rec translate_env (tenv : env) : var_env * dbenv =
  match tenv with
  | END -> (VEND, DBEND)
  | NEXT (x, v, e) ->
      let venv, denv = translate_env e in
      let dbv = translate_db_val v venv in
      (VNEXT (x, venv), DBNEXT (dbv, denv))

and translate_db_val (v : value) (venv : var_env) : dbvalue =
  match v with
  | VINT n -> VDBINT n
  | VFUN (x, t, e) ->
      let v_env_inner, d_env_inner = translate_env e in
      VDBFUN (translate_db t (VNEXT (x, v_env_inner)), d_env_inner)
  | VFIX _ -> failwith "db_term only has recursive functions"
  | VFIXFUN (f, x, t, e) ->
      let v_env_inner, d_env_inner = translate_env e in
      VDBFIXFUN (translate_db t (VNEXT (x, VNEXT (f, v_env_inner))), d_env_inner)
  | THUNK (t, e) ->
      let v_env_inner, d_env_inner = translate_env e in
      DBTHUNK (translate_db t v_env_inner, d_env_inner)
  (* -- Pair -- *)
  | VPAIR (v1, v2) ->
      VDBPAIR (translate_db_val v1 venv, translate_db_val v2 venv)
  (* -- List -- *)
  | VNIL -> VDBNIL
  | VCONS (t, l) -> VDBCONS (translate_db_val t venv, translate_db_val l venv)
  (* -- Tree -- *)
  | VLEAF t -> VDBLEAF (translate_db_val t venv)
  | VTREE (l, r) -> VDBTREE (translate_db_val l venv, translate_db_val r venv)
