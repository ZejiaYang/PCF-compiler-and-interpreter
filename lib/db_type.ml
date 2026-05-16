open Db_term
open Term

type dbtype =
  | Nat
  | Fun of dbtype * dbtype
  | Fixfun of dbtype * dbtype
  | Pair of dbtype * dbtype
  | List of dbtype option (* empty list *)
  | Tree of dbtype

and tdbterm =
  (* type inference to translate from dbterm to tdbterm *)
  | TDBVAR of int
  | TDBFUN of dbtype * tdbterm
  | TDBAPP of tdbterm * tdbterm
  | TDBINT of int
  | TDBBOP of tdbterm * op * tdbterm
  | TDBIFZ of tdbterm * tdbterm * tdbterm
  | TDBFIXFUN of dbtype * tdbterm (*recursive closure*)
  | TDBLET of dbtype * tdbterm * tdbterm
  (* -- Pair -- *)
  | TDBPAIR of tdbterm * tdbterm
  | TDBFST of tdbterm
  | TDBSND of tdbterm
  (* -- List -- *)
  | TDBNIL
  | TDBCONS of tdbterm * tdbterm
  | TDBIFNIL of tdbterm * tdbterm * tdbterm
  | TDBHD of tdbterm
  | TDBTL of tdbterm
  (* -- Tree --*)
  | TDBLEAF of tdbterm
  | TDBTREE of tdbterm * tdbterm
  | TDBITEM of tdbterm
  | TDBIFLEAF of tdbterm * tdbterm * tdbterm
  | TDBLTREE of tdbterm
  | TDBRTREE of tdbterm
[@@deriving show, eq]

(* function *)
let ( ==> ) a b = Fun (a, b)

(* recursive function *)
let ( =>> ) a b = Fixfun (a, b)

type tenv = dbtype list (* type checker for typed db *)
type typechecker = tenv -> tdbterm -> dbtype

(* type checker for typed db terms *)
let rec check_tdb (env : tenv) (term : tdbterm) : dbtype =
  match term with
  | TDBVAR i -> List.nth env i
  | TDBFUN (a, t) -> Fun (a, check_tdb (a :: env) t)
  | TDBAPP (p1, p2) -> (
      match (check_tdb env p1, check_tdb env p2) with
      | Fun (p1, p2), p3 | Fixfun (p1, p2), p3 ->
          if p3 = p1 then p2 else failwith "functiona parameter type mismatch"
      | _ -> failwith "app not function ")
  | TDBINT _ -> Nat
  | TDBBOP (p1, op, p2) -> (
      match (check_tdb env p1, check_tdb env p2) with
      | Nat, Nat -> Nat
      | _ -> failwith "illegal binary operation, operand not nat")
  | TDBIFZ (p, t1, t2) -> (
      match (check_tdb env p, check_tdb env t1, check_tdb env t2) with
      | Nat, p1, p2 -> if p1 = p2 then p1 else failwith "Ifz condition not nat"
      | _ -> failwith "illegal Ifz construct")
  | TDBFIXFUN (a, t) -> Fixfun (a, check_tdb (a :: env) t)
  | TDBLET (a, t, p) ->
      let b = check_tdb env t in
      let c = check_tdb (a :: env) p in
      if a = b then c else failwith "let binding type mismatch"
  (* -- Pair -- *)
  | TDBPAIR (p1, p2) -> Pair (check_tdb env p1, check_tdb env p2)
  | TDBFST p -> (
      match check_tdb env p with
      | Pair (a, b) -> a
      | _ -> failwith "illegal fst construct")
  | TDBSND p -> (
      match check_tdb env p with
      | Pair (a, b) -> b
      | _ -> failwith "illegal fst construct")
  (* -- List -- *)
  | TDBNIL -> List None (* polymorphic type *)
  | TDBCONS (hd, tl) -> (
      match (check_tdb env hd, check_tdb env tl) with
      | a, List None -> a (* differentiate between empty list and not empty!*)
      | a, List (Some b) ->
          if a = b then a else failwith "list element type not consistent "
      | _ -> failwith "illegal cons")
  | TDBIFNIL (p, t1, t2) -> (
      match (check_tdb env p, check_tdb env t1, check_tdb env t2) with
      | List _, p1, p2 ->
          if p1 = p2 then p1 else failwith "Ifz condition not nat"
      | _ -> failwith "illegal Ifz construct")
  | TDBHD t -> (
      match check_tdb env t with
      | List None -> failwith "empty list not hd"
      | List (Some a) -> a
      | _ -> failwith "hd not list")
  | TDBTL t -> (
      match check_tdb env t with
      | List None -> failwith "empty list not tl"
      | List (Some a) -> a
      | _ -> failwith "tl not list")
  (* -- Tree -- *)
  | TDBLEAF t -> Tree (check_tdb env t)
  | TDBTREE (p1, p2) -> (
      match (check_tdb env p1, check_tdb env p2) with
      | Tree a, Tree b ->
          if a == b then Tree a else failwith "Tree branch not same type"
      | _ -> failwith "Tree branch not tree")
  | TDBITEM t -> (
      match check_tdb env t with Tree a -> a | _ -> failwith "Item not tree")
  | TDBIFLEAF (p, t1, t2) -> (
      match (check_tdb env p, check_tdb env t1, check_tdb env t2) with
      | Tree _, p1, p2 ->
          if p1 = p2 then p1 else failwith "Ifz condition not nat"
      | _ -> failwith "illegal Ifz construct")
  | TDBLTREE t -> (
      match check_tdb env t with
      | Tree a -> Tree a
      | _ -> failwith "ltree not tree")
  | TDBRTREE t -> (
      match check_tdb env t with
      | Tree a -> Tree a
      | _ -> failwith "ltree not tree")
