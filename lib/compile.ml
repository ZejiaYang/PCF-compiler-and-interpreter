open Db_term

type instruction =
  | Ldi of int
  | Push
  | Search of int
  | Pushenv
  | Popenv
  | Mkclos of code
  | Mkrclos of code
  | Apply
  | Test of code * code
  | Add
  | Sub
  | Mult
  | Div
  (* -- Pair -- *)
  | Mkpair
  | Fst
  | Snd
  (* -- List -- *)
  | Nil
  | Cons
  | CTest of code * code
  | Hd
  | Tl
  (* -- Tree -- *)
  | Leaf
  | Mktree
  | Item
  | TTest of code * code
  | Ltree
  | Rtree

and code = END | ( :: ) of instruction * code
and cvalue = VInt of int | VClosure of code * env | VRClosure of code * env
and env = cvalue list (* map from indices to value,*)

type stack_item = Env of env | Value of cvalue (*static scoping *)

type stack = stack_item list

(* physical design: env is stored as part of stack frame **)
and state = cvalue * stack * env * code

type compiler = dbterm -> code

let rec ( @ ) l1 l2 = match l1 with END -> l2 | t :: c -> t :: (c @ l2)

let rec compile (term : dbterm) : code =
  match term with
  | DBVAR n -> Search n :: END
  | DBFUN p -> Mkclos (compile p) :: END
  | DBAPP (p1, p2) ->
      (Pushenv :: compile p2) @ (Push :: compile p1) @ (Apply :: Popenv :: END)
  | DBINT n -> Ldi n :: END
  | DBBOP (p1, op, p2) -> (
      match op with
      | ADD -> compile p1 @ (Push :: compile p2) @ (Add :: END)
      | MINUS -> compile p1 @ (Push :: compile p2) @ (Sub :: END)
      | MULTI -> compile p1 @ (Push :: compile p2) @ (Mult :: END)
      | DIVIDE -> compile p1 @ (Push :: compile p2) @ (Div :: END))
  | DBIFZ (p1, p2, p3) -> compile p1 @ (Test (compile p2, compile p3) :: END)
  | DBLET (p1, p2) -> compile (DBAPP (DBFUN p2, p1))
  | DBFIXFUN p -> Mkrclos (compile p) :: END
  (* -- Pair -- *)
  | DBPAIR (p1, p2) -> compile p2 @ compile p1 @ (Mkpair :: END)
  | DBFST p -> compile p @ (Fst :: END)
  | DBSND p -> compile p @ (Snd :: END)
  (* -- List -- *)
  | DBNIL -> Nil :: END
  | DBCONS (t, l) -> compile l @ compile t @ (Cons :: END)
  | DBIFNIL (y, p1, p2) -> compile y @ (CTest (compile p1, compile p2) :: END)
  | DBHD p -> compile p @ (Hd :: END)
  | DBTL p -> compile p @ (Tl :: END)
  (* -- Tree -- *)
  | DBLEAF p -> compile p @ (Leaf :: END)
  | DBTREE (l, u) -> compile l @ compile u @ (Mktree :: END)
  | DBITEM p -> compile p @ (Item :: END)
  | DBIFLEAF (p, p1, p2) -> compile p @ (TTest (compile p1, compile p2) :: END)
  | DBLTREE p -> compile p @ (Ltree :: END)
  | DBRTREE p -> compile p @ (Rtree :: END)

let rec search (e : env) (n : int) =
  match (e, n) with
  | hd :: c, 0 -> hd
  | _ :: tl, n -> search tl (n - 1)
  | [], _ -> failwith ("empty list, cannot find" ^ string_of_int n)

let step (state : state) : state =
  match state with
  | acc, stack, env, Mkclos t :: code -> (VClosure (t, env), stack, env, code)
  | acc, stack, env, Mkrclos t :: code -> (VRClosure (t, env), stack, env, code)
  | acc, stack, env, Push :: code -> (acc, Value acc :: stack, env, code)
  | acc, stack, env, Search n :: code ->
      let v = search env n in
      (v, stack, env, code)
  | acc, stack, env, Pushenv :: code -> (acc, Env env :: stack, env, code)
  | acc, Env e :: stack, _, Popenv :: code -> (acc, stack, e, code)
  | VClosure (t, e), Value w :: stack, _, Apply :: code ->
      (VClosure (t, e), stack, w :: e, t @ code) (* add the later code in *)
  | VRClosure (t, e), Value w :: stack, _, Apply :: code ->
      (VRClosure (t, e), stack, w :: VRClosure (t, e) :: e, t @ code)
  | acc, stack, env, Ldi n :: code -> (VInt n, stack, env, code)
  | n, Value m :: stack, env, Add :: code -> (
      match (n, m) with
      | VInt n, VInt m -> (VInt (n + m), stack, env, code)
      | _ -> failwith "binary operands not integer")
  | n, Value m :: stack, env, Sub :: code -> (
      match (n, m) with
      | VInt n, VInt m -> (VInt (m - n), stack, env, code)
      | _ -> failwith "binary operands not integer")
  | n, Value m :: stack, env, Mult :: code -> (
      match (n, m) with
      | VInt n, VInt m -> (VInt (m * n), stack, env, code)
      | _ -> failwith "binary operands not integer")
  | n, Value m :: stack, env, Div :: code -> (
      match (n, m) with
      | VInt 0, _ -> failwith "divisor is zero"
      | VInt n, VInt m -> (VInt (m / n), stack, env, code)
      | _ -> failwith "binary operands not integer")
  | VInt 0, state, env, Test (i, j) :: c -> (VInt 0, state, env, i @ c)
  | VInt n, state, env, Test (i, j) :: c -> (VInt n, state, env, j @ c)
  | _, _, _, END -> state
  | _ ->
      failwith
        "illegal construct for now" (* extends for tree, pair, list later*)

let rec step_star (state : state) : cvalue =
  match step state with v, _, _, END -> v | state -> step_star state

let execute (program : code) =
  let start = (VInt 0, [], [], program) in
  step_star start
