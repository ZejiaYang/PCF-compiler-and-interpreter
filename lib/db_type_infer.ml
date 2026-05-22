open Db_term

type dbtype =
  | Nat
  | Fun of dbtype * dbtype
  | Fixfun of dbtype * dbtype
  | Pair of dbtype * dbtype
  | List of dbtype (* same type *)
  | Tree of dbtype
  | Unknown of int
  | Any
[@@deriving show, eq]

type tenv = dbtype list
type constrant = (dbtype * dbtype) list

module TypeMap = Map.Make (Int)

type typechecker = tenv -> dbterm -> dbtype

(* produce fresh variable for typing environment*)
let counter = ref 0

let rec next_var () =
  counter := !counter + 1;
  Unknown !counter

(*  type type inference algorithm for untyped db PCF using Hindley Milner Algorithm
    
    1) first constraint generation (type check with constraint generated) 

    2) second unification algorithm: find a substitution that unifies the set
    recursively unifying t1 = t2, substitute and unify on the smaller set until
    empty -> fail or substitution

    3) apply substitution to the type (principal type) 

    to fix:
    1) fixfun and fun: only keep fun
    2) remove any type and change to preserve unknown type
    3) proper polymorphism (HM instantiation/generalisation)

    to optimise
    1) large substitution cost
    2) no normalization before occur check (costly but eventually detect cycles)
**)
let rec gen_type_constraint (env : tenv) (term : dbterm) : dbtype * constrant =
  match term with
  | DBVAR i -> (List.nth env i, [])
  | DBFUN p ->
      let vx = next_var () in
      let t1, c1 = gen_type_constraint (vx :: env) p in
      (Fun (vx, t1), c1)
  | DBAPP (p1, p2) ->
      let t1, c1 = gen_type_constraint env p1 in
      let t2, c2 = gen_type_constraint env p2 in
      let vx = next_var () in
      (vx, c1 @ c2 @ [ (t1, Fun (t2, vx)) ])
      (* t1 might be unknown variables | functions | recursive functions *)
  | DBINT _ -> (Nat, [])
  | DBBOP (p1, op, p2) ->
      let t1, c1 = gen_type_constraint env p1 in
      let t2, c2 = gen_type_constraint env p2 in
      (Nat, c1 @ c2 @ [ (Nat, t1); (Nat, t2) ])
  | DBIFZ (p, p1, p2) ->
      let tp, cp = gen_type_constraint env p in
      let t1, c1 = gen_type_constraint env p1 in
      let t2, c2 = gen_type_constraint env p2 in
      (t1, c1 @ c2 @ cp @ [ (Nat, tp); (t1, t2) ])
  | DBFIXFUN p ->
      let vx1 = next_var () in
      let vx2 = next_var () in
      let t, c = gen_type_constraint (vx1 :: Fixfun (vx1, vx2) :: env) p in
      (Fixfun (vx1, t), c @ [ (vx2, t) ])
  | DBLET (t, p) ->
      let t1, c1 = gen_type_constraint env t in
      let t2, c2 = gen_type_constraint (t1 :: env) p in
      (t2, c1 @ c2)
  (* -- Pair -- *)
  | DBPAIR (p1, p2) ->
      let t1, c1 = gen_type_constraint env p1 in
      let t2, c2 = gen_type_constraint env p2 in
      (Pair (t1, t2), c1 @ c2)
  | DBFST p ->
      let v1 = next_var () in
      let v2 = next_var () in
      let t, c = gen_type_constraint env p in
      (v1, c @ [ (t, Pair (v1, v2)) ])
  | DBSND p ->
      let v1 = next_var () in
      let v2 = next_var () in
      let t, c = gen_type_constraint env p in
      (v2, c @ [ (t, Pair (v1, v2)) ])
  | _ -> failwith "unimplemented construct for now"

(* occur check x : id in y*)
let rec occur_check (x : int) y : bool =
  let recur_check a b = occur_check x a || occur_check x b in
  match y with
  | Unknown y -> x = y
  | Nat -> false
  | Fun (a, b) -> recur_check a b
  | Fixfun (a, b) -> recur_check a b
  | Pair (a, b) -> recur_check a b
  | _ -> failwith "unimplemented type for now"

(* sub_type replace var x with y in a*)
let rec sub_type (x : int) (y : dbtype) (a : dbtype) =
  match a with
  | Unknown id -> if id = x then y else a
  | Nat -> Nat
  | Fun (a, b) -> Fun (sub_type x y a, sub_type x y b)
  | Fixfun (a, b) -> Fixfun (sub_type x y a, sub_type x y b)
  | Pair (a, b) -> Pair (sub_type x y a, sub_type x y b)
  | _ -> failwith "unimplemented type for now"

(* sub_type_list*)
let rec sub_type_list (x : int) (y : dbtype) (c : constrant) =
  match c with
  | [] -> []
  | (a, b) :: c -> (sub_type x y a, sub_type x y b) :: sub_type_list x y c

let rec solve_constraint (c : constrant) : dbtype TypeMap.t =
  match c with
  | [] -> TypeMap.empty
  | _ -> (
      let tl = List.tl c in
      match List.hd c with
      | Nat, Nat -> solve_constraint tl
      | Nat, Unknown x | Unknown x, Nat ->
          TypeMap.add x Nat (solve_constraint (sub_type_list x Nat tl))
      | Nat, _ | _, Nat ->
          failwith "unable to unify Nat with types other than var"
      | Unknown x, Unknown y ->
          if x = y then solve_constraint tl
          else
            TypeMap.add x (Unknown y)
              (solve_constraint (sub_type_list x (Unknown y) tl))
      | Unknown x, y ->
          if occur_check x y then
            failwith "unable to unify due to recursive type"
          else TypeMap.add x y (solve_constraint (sub_type_list x y tl))
      | x, Unknown y ->
          if occur_check y x then
            failwith "unable to unify due to recursive type"
          else TypeMap.add y x (solve_constraint (sub_type_list y x tl))
      | Fun (t1, t2), Fun (t3, t4)
      | Fixfun (t1, t2), Fun (t3, t4)
      | Fun (t1, t2), Fixfun (t3, t4) ->
          solve_constraint ((t1, t3) :: (t2, t4) :: tl)
      | Fun _, _ | _, Fun _ ->
          failwith "unable to unify Fun with types other than fun/fixfun"
      | Fixfun (t1, t2), Fixfun (t3, t4) ->
          solve_constraint ((t1, t3) :: (t2, t4) :: tl)
      | Fixfun _, _ | _, Fixfun _ ->
          failwith "unable to unify Fixfun with types other than fun/fixfun"
      | Pair (t1, t2), Pair (t3, t4) ->
          solve_constraint ((t1, t3) :: (t2, t4) :: tl)
      | Pair _, _ | _, Pair _ ->
          failwith "unable to unify Pair with types other than Pair"
      | _ -> failwith "types not implemented ")

let rec replace_type (t : dbtype) (sol : dbtype TypeMap.t) :
    dbtype * dbtype TypeMap.t =
  match t with
  | Unknown x -> (
      match TypeMap.find_opt x sol with
      | Some v ->
          let t, rsol = replace_type v sol in
          (t, TypeMap.add x t sol)
      | None -> (Any, sol))
  | Nat -> (Nat, sol)
  | Fun (a, b) ->
      let ta, rsol = replace_type a sol in
      let tb, rsol = replace_type b rsol in
      (Fun (ta, tb), rsol)
  | Fixfun (a, b) ->
      let ta, rsol = replace_type a sol in
      let tb, rsol = replace_type b rsol in
      (Fixfun (ta, tb), rsol)
  | Pair (a, b) ->
      let ta, rsol = replace_type a sol in
      let tb, rsol = replace_type b rsol in
      (Pair (ta, tb), rsol)
  | _ -> failwith "unimplemented types for now"

let type_check_infer : typechecker =
 fun env ->
  fun term ->
   let rtype, cons = gen_type_constraint env term in
   let tmap = solve_constraint cons in
   Pair.fst (replace_type rtype tmap)
