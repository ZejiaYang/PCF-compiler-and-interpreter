open Term

let rec is_value = function INT _ -> true | FUN _ -> true | _ -> false

let rec sub (x : string) (u : term) (t : term) : term =
  let sub' = sub x u in
  match t with
  | INT _ -> t
  | VAR y -> if x = y then u else t
  | FUN (y, p) -> if x = y then t else FUN (y, sub' p)
  | BOP (p1, op, p2) -> BOP (sub' p1, op, sub' p2)
  | IFZ (p1, p2, p3) -> IFZ (sub' p1, sub' p2, sub' p3)
  | APP (p1, p2) -> APP (sub' p1, sub' p2)
  | LET (y, p1, p2) ->
      let p1' = sub' p1 in
      if x = y then LET (y, p1', p2) else LET (y, p1', sub' p2)
  | FIX (y, p) -> if x = y then t else FIX (y, sub' p)
  | PAIR (p1, p2) -> PAIR (sub' p1, sub' p2)
  | FST t -> FST (sub' t)
  | SND t -> SND (sub' t)
  | NIL -> NIL
  | CONS (t, l) -> CONS (sub' t, sub' l)
  | IFNIL (y, p1, p2) -> IFNIL (sub' y, sub' p1, sub' p2)
  | HD t -> HD (sub' t)
  | TL t -> TL (sub' t)
  | LEAF t -> LEAF (sub' t)
  | TREE (l, r) -> TREE (sub' l, sub' r)
  | ITEM l -> ITEM (sub' l)
  | IFLEAF (l, t, u) -> IFLEAF (sub' l, sub' t, sub' u)
  | LTREE t -> LTREE (sub' t)
  | RTREE t -> RTREE (sub' t)

(*
* (fun x -> fun x -> x) 2 3
* (fun x -> fun y -> ((fun x -> x + y) x)) 4 5

* static binding and dynamic binding
let x = 4 in let f = fun y -> y + x in let x = 5 in f 6

* term C b1
C: fun x -> 0
b1: (fix f (fun x -> (f x))) 0

*)

(*
a Call-by-name evaluator for PCF
closed terms -> value
*)
let rec eval_by_name (p : term) : term =
  match p with
  | VAR _ -> failwith "input not closed term"
  | FUN _ | INT _ | LEAF _ | TREE _ | NIL | CONS _ | PAIR _ -> p
  | BOP (p1, op, p2) -> (
      let v1 = eval_by_name p1 in
      let v2 = eval_by_name p2 in
      match (v1, op, v2) with
      | INT n1, ADD, INT n2 -> INT (n1 + n2)
      | INT n1, MINUS, INT n2 -> INT (n1 - n2)
      | INT n1, MULTI, INT n2 -> INT (n1 * n2)
      | INT n1, DIVIDE, INT 0 -> failwith "divide by zero"
      | INT n1, DIVIDE, INT n2 -> INT (n1 / n2)
      | _ -> failwith "binary operands not integer")
  | IFZ (p1, p2, p3) -> (
      match eval_by_name p1 with
      | INT n -> if n = 0 then eval_by_name p2 else eval_by_name p3
      | _ -> failwith "if condition not bool")
  | APP (p1, p2) -> (
      match eval_by_name p1 with
      | FUN (x, t) -> eval_by_name (sub x p2 t)
      | _ -> failwith "not a function in application")
  | LET (x, p1, p2) -> eval_by_name (APP (FUN (x, p2), p1))
  | FIX (x, p1) -> eval_by_name (APP (FUN (x, p1), p))
  (* pair *)
  | FST t -> (
      match eval_by_name t with
      | PAIR (l, _) -> eval_by_name l
      | _ -> failwith "illegal construct")
  | SND t -> (
      match eval_by_name t with
      | PAIR (_, r) -> eval_by_name r
      | _ -> failwith "illegal construct")
  (* list *)
  | IFNIL (y, p1, p2) -> (
      match eval_by_name y with
      | NIL -> eval_by_name p1
      | CONS _ -> eval_by_name p2
      | _ -> failwith "ifnil condition not list")
  | HD t -> (
      match eval_by_name t with
      | NIL -> failwith "empty list"
      | CONS (h, _) -> eval_by_name h
      | _ -> failwith "illegal construct")
  | TL t -> (
      match eval_by_name t with
      | NIL -> failwith "empty list"
      | CONS (_, t) -> eval_by_name t
      | _ -> failwith "illegal construct")
  (* tree *)
  | ITEM t -> (
      match eval_by_name t with
      | LEAF n -> eval_by_name n
      | _ -> failwith "illegal construct")
  | IFLEAF (p, p1, p2) -> (
      match eval_by_name p with
      | LEAF n -> eval_by_name p1
      | TREE _ -> eval_by_name p2
      | _ -> failwith "IFLEAF not tree")
  | LTREE t -> (
      match eval_by_name t with
      | TREE (l, _) -> eval_by_name l
      | LEAF _ -> failwith "LTREE a leaf"
      | _ -> failwith "illegal construct")
  | RTREE t -> (
      match eval_by_name t with
      | TREE (_, r) -> eval_by_name r
      | LEAF _ -> failwith "RTREE a leaf"
      | _ -> failwith "illegal construct")

(*
a Call-by-value evaluator for PCF
closed term -> value
*)
let rec eval_by_value (p : term) : term =
  match p with
  | VAR _ -> failwith "input not closed term"
  | FUN _ | INT _ -> p
  | BOP (p1, op, p2) -> (
      let v1 = eval_by_value p1 in
      let v2 = eval_by_value p2 in
      match (v1, op, v2) with
      | INT n1, ADD, INT n2 -> INT (n1 + n2)
      | INT n1, MINUS, INT n2 -> INT (n1 - n2)
      | INT n1, MULTI, INT n2 -> INT (n1 * n2)
      | INT n1, DIVIDE, INT 0 -> failwith "divide by zero"
      | INT n1, DIVIDE, INT n2 -> INT (n1 / n2)
      | _ -> failwith "binary operands not integer")
  | IFZ (p1, p2, p3) -> (
      match eval_by_value p1 with
      | INT n -> if n = 0 then eval_by_value p2 else eval_by_value p3
      | _ -> failwith "ifz condition not bool")
  | APP (p1, p2) -> (
      match eval_by_value p1 with
      | FUN (x, t) -> eval_by_value (sub x (eval_by_value p2) t)
      | _ -> failwith "not a function in application")
  | LET (x, p1, p2) -> eval_by_value (APP (FUN (x, p2), p1))
  | FIX (x, p1) -> eval_by_value (APP (FUN (x, p1), p))
  (* pair *)
  | PAIR (p1, p2) -> PAIR (eval_by_value p1, eval_by_value p2)
  | FST t -> (
      match eval_by_value t with
      | PAIR (l, _) -> l
      | _ -> failwith "illegal construct")
  | SND t -> (
      match eval_by_value t with
      | PAIR (_, r) -> r
      | _ -> failwith "illegal construct")
  (* list *)
  | NIL -> NIL
  | CONS (t, l) -> CONS (eval_by_value t, eval_by_value l)
  | IFNIL (y, p1, p2) -> (
      match eval_by_value y with
      | NIL -> eval_by_value p1
      | CONS _ -> eval_by_value p2
      | _ -> failwith "ifnil condition not list")
  | HD t -> (
      match eval_by_value t with
      | NIL -> failwith "empty list"
      | CONS (h, _) -> h
      | _ -> failwith "illegal construct")
  | TL t -> (
      match eval_by_value t with
      | NIL -> failwith "empty list"
      | CONS (_, t) -> t
      | _ -> failwith "illegal construct")
  (* tree *)
  | LEAF n -> LEAF (eval_by_value n)
  | TREE (l, r) -> TREE (eval_by_value l, eval_by_value r)
  | ITEM t -> (
      match eval_by_value t with
      | LEAF n -> n
      | _ -> failwith "illegal construct")
  | IFLEAF (p, p1, p2) -> (
      match eval_by_value p with
      | LEAF n -> eval_by_value p1
      | TREE _ -> eval_by_value p2
      | _ -> failwith "IFLEAF not tree")
  | LTREE t -> (
      match eval_by_value t with
      | TREE (l, _) -> l
      | LEAF _ -> failwith "LTREE a leaf"
      | _ -> failwith "illegal construct")
  | RTREE t -> (
      match eval_by_value t with
      | TREE (_, r) -> r
      | LEAF _ -> failwith "RTREE a leaf"
      | _ -> failwith "illegal construct")
