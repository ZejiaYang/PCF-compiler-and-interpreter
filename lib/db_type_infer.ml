open Db_term

type dbtype =
  | Nat
  | Fun of dbtype * dbtype
  | Fixfun of dbtype * dbtype
  | Pair of dbtype * dbtype
  | List of dbtype (* empty list *)
  | Tree of dbtype
  | Unknown

type tienv = dbtype list

(* type checker for db *)
(* basic implementation
  1. first inference the type (add de bruijn indices + mutable environment + fail)
  2. replace the variable's type with the inference types
*)

(* type checker for db term*)
(* let rec check_db (env : tienv) (term : dbterm) : dbtype =
  match term with
  | DBVAR i -> List.nth env i
  | DBFUN t -> let Fun (Unknown, Unknown)
  | DBAPP (p1, p2) -> (
      match (check_db env p1, check_db env p2) with
      | Some (Fun (Some p1, p2)), Some p3 | Some (Fixfun (Some p1, p2)), Some p3
        ->
          if p3 = p1 then p2 else failwith "functiona parameter type mismatch"
      | _ -> failwith "app not function ")
  | DBINT _ -> Nat
  | DBBOP (p1, op, p2) -> (
      match (check_db env p1, check_db env p2) with
      | Nat, Nat -> Nat
      | _ -> failwith "illegal binary operation, operand not nat")
  | DBIFZ (p, t1, t2) -> (
      match (check_db env p, check_db env t1, check_db env t2) with
      | Nat, p1, p2 -> if p1 = p2 then p1 else failwith "Ifz condition not nat"
      | _ -> failwith "illegal Ifz construct")
  | DBFIXFUN (a, t) -> Fixfun (a, check_db (a :: env) t)
  | DBLET (a, t, p) ->
      let b = check_db env t in
      let c = check_db (a :: env) p in
      if a = b then c else failwith "let binding type mismatch"
  (* -- Pair -- *)
  | DBPAIR (p1, p2) -> Pair (check_db env p1, check_db env p2)
  | DBFST p -> (
      match check_db env p with
      | Pair (a, b) -> a
      | _ -> failwith "illegal fst construct")
  | DBSND p -> (
      match check_db env p with
      | Pair (a, b) -> b
      | _ -> failwith "illegal fst construct")
  (* -- List -- *)
  | DBNIL -> List None (* polymorphic type *)
  | DBCONS (hd, tl) -> (
      match (check_db env hd, check_db env tl) with
      | a, List None -> a (* differentiate between empty list and not empty!*)
      | a, List (Some b) ->
          if a = b then a else failwith "list element type not consistent "
      | _ -> failwith "illegal cons")
  | DBIFNIL (p, t1, t2) -> (
      match (check_db env p, check_db env t1, check_db env t2) with
      | List _, p1, p2 ->
          if p1 = p2 then p1 else failwith "Ifz condition not nat"
      | _ -> failwith "illegal Ifz construct")
  | DBHD t -> (
      match check_db env t with
      | List None -> failwith "empty list not hd"
      | List (Some a) -> a
      | _ -> failwith "hd not list")
  | DBTL t -> (
      match check_db env t with
      | List None -> failwith "empty list not tl"
      | List (Some a) -> a
      | _ -> failwith "tl not list")
  (* -- Tree -- *)
  | DBLEAF t -> Tree (check_db env t)
  | DBTREE (p1, p2) -> (
      match (check_db env p1, check_db env p2) with
      | Tree a, Tree b ->
          if a == b then Tree a else failwith "Tree branch not same type"
      | _ -> failwith "Tree branch not tree")
  | DBITEM t -> (
      match check_db env t with Tree a -> a | _ -> failwith "Item not tree")
  | DBIFLEAF (p, t1, t2) -> (
      match (check_db env p, check_db env t1, check_db env t2) with
      | Tree _, p1, p2 ->
          if p1 = p2 then p1 else failwith "Ifz condition not nat"
      | _ -> failwith "illegal Ifz construct")
  | DBLTREE t -> (
      match check_db env t with Tree a -> a | _ -> failwith "ltree not tree")
  | DBRTREE t -> (
      match check_db env t with Tree a -> a | _ -> failwith "ltree not tree") *)
