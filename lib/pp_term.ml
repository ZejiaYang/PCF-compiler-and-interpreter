open Term
open Db_term
open Format
open Compile

let pp_op fmt = function
  | ADD -> fprintf fmt "+"
  | MINUS -> fprintf fmt "-"
  | MULTI -> fprintf fmt "*"
  | DIVIDE -> fprintf fmt "/"

let rec pp_term fmt = function
  | INT n -> fprintf fmt "%d" n
  | VAR x -> fprintf fmt "%s" x
  | FUN (x, p) -> fprintf fmt "@[<2>fun %s ->@ %a@]" x pp_term p
  | APP (p1, p2) -> fprintf fmt "@[<2>(%a %a)@]" pp_term p1 pp_term p2
  | BOP (p1, op, p2) ->
      fprintf fmt "@[<2>(%a %a %a)@]" pp_term p1 pp_op op pp_term p2
  | IFZ (p1, p2, p3) ->
      fprintf fmt "@[<2>if %a = 0 then@ %a@ else@ %a@]" pp_term p1 pp_term p2
        pp_term p3
  | FIX (f, p) -> fprintf fmt "@[<2>fix %s.@ %a@]" f pp_term p
  | LET (x, p1, p2) ->
      fprintf fmt "@[<2>let %s = %a in@ %a@]" x pp_term p1 pp_term p2
  (* --- Pair --- *)
  | PAIR (p1, p2) -> fprintf fmt "@[<2>(%a , %a)@]" pp_term p1 pp_term p2
  | FST p -> fprintf fmt "@[<2>fst %a@]" pp_term p
  | SND p -> fprintf fmt "@[<2>snd %a@]" pp_term p
  (* --- List --- *)
  | NIL -> fprintf fmt "nil"
  | CONS (p1, p2) -> fprintf fmt "@[<2>(%a :: %a)@]" pp_term p1 pp_term p2
  | IFNIL (p1, p2, p3) ->
      fprintf fmt "@[<2>ifnil %a then@ %a@ else@ %a@]" pp_term p1 pp_term p2
        pp_term p3
  | HD p -> fprintf fmt "@[<2>hd %a@]" pp_term p
  | TL p -> fprintf fmt "@[<2>tl %a@]" pp_term p
  (* --- Tree --- *)
  | LEAF t -> fprintf fmt "@[<2>leaf(%a)@]" pp_term t
  | TREE (l, r) -> fprintf fmt "@[<2>tree(%a,@ %a)@]" pp_term l pp_term r
  | ITEM t -> fprintf fmt "@[<2>item %a@]" pp_term t
  | IFLEAF (p1, p2, p3) ->
      fprintf fmt "@[<2>ifleaf %a then@ %a@ else@ %a@]" pp_term p1 pp_term p2
        pp_term p3
  | LTREE t -> fprintf fmt "@[<2>ltree %a@]" pp_term t
  | RTREE t -> fprintf fmt "@[<2>rtree %a@]" pp_term t

let rec pp_db_term fmt = function
  | DBINT n -> fprintf fmt "%d" n
  | DBVAR i -> fprintf fmt "#%d" i
  | DBFUN t -> fprintf fmt "@[<2>fun . ->@ %a@]" pp_db_term t
  | DBAPP (p1, p2) -> fprintf fmt "@[<2>(%a %a)@]" pp_db_term p1 pp_db_term p2
  | DBBOP (p1, op, p2) ->
      fprintf fmt "@[<2>(%a %a %a)@]" pp_db_term p1 pp_op op pp_db_term p2
  | DBIFZ (p1, p2, p3) ->
      fprintf fmt "@[<2>if %a = 0 then@ %a@ else@ %a@]" pp_db_term p1 pp_db_term
        p2 pp_db_term p3
  | DBFIXFUN p -> fprintf fmt "@[<2>fixfun . ->@ fun . ->@ %a@]" pp_db_term p
  | DBLET (p1, p2) ->
      fprintf fmt "@[<2>let . = %a in@ %a@]" pp_db_term p1 pp_db_term p2
  (* -- Pair -- *)
  | DBPAIR (p1, p2) ->
      fprintf fmt "@[<2>(%a , %a)@]" pp_db_term p1 pp_db_term p2
  | DBFST p -> fprintf fmt "@[<2>fst %a@]" pp_db_term p
  | DBSND p -> fprintf fmt "@[<2>snd %a@]" pp_db_term p
  (* --- List --- *)
  | DBNIL -> fprintf fmt "nil"
  | DBCONS (p1, p2) ->
      fprintf fmt "@[<2>(%a :: %a)@]" pp_db_term p1 pp_db_term p2
  | DBIFNIL (p1, p2, p3) ->
      fprintf fmt "@[<2>ifnil %a then@ %a@ else@ %a@]" pp_db_term p1 pp_db_term
        p2 pp_db_term p3
  | DBHD p -> fprintf fmt "@[<2>hd %a@]" pp_db_term p
  | DBTL p -> fprintf fmt "@[<2>tl %a@]" pp_db_term p
  (* --- Tree Extension --- *)
  | DBLEAF t -> fprintf fmt "@[<2>leaf(%a)@]" pp_db_term t
  | DBTREE (l, r) ->
      fprintf fmt "@[<2>tree(%a,@ %a)@]" pp_db_term l pp_db_term r
  | DBITEM t -> fprintf fmt "@[<2>item %a@]" pp_db_term t
  | DBIFLEAF (p1, p2, p3) ->
      fprintf fmt "@[<2>ifleaf %a then@ %a@ else@ %a@]" pp_db_term p1 pp_db_term
        p2 pp_db_term p3
  | DBLTREE t -> fprintf fmt "@[<2>ltree %a@]" pp_db_term t
  | DBRTREE t -> fprintf fmt "@[<2>rtree %a@]" pp_db_term t

let rec pp_value fmt = function
  | VINT n -> fprintf fmt "%d" n
  | VFUN (x, p, _) -> fprintf fmt "@[<2>fun %s ->@ %a@]" x pp_term p
  | VFIX (f, p, _) -> fprintf fmt "@[<2>fix %s.@ %a@]" f pp_term p
  | VFIXFUN (f, x, p, _env) ->
      fprintf fmt "@[<2>fixfun %s ->@ fun %s ->@ %a@]" f x pp_term p
  | THUNK (t, _) -> fprintf fmt "@[<2><thunk %a>@]" pp_term t
  (* -- Pair -- *)
  | VPAIR (p1, p2) -> fprintf fmt "@[<2>(%a , %a)@]" pp_value p1 pp_value p2
  (* --- List --- *)
  | VNIL -> fprintf fmt "nil"
  | VCONS (v1, v2) -> fprintf fmt "@[<2>(%a :: %a)@]" pp_value v1 pp_value v2
  (* --- Tree --- *)
  | VLEAF v -> fprintf fmt "@[<2>leaf(%a)@]" pp_value v
  | VTREE (v1, v2) -> fprintf fmt "@[<2>tree(%a,@ %a)@]" pp_value v1 pp_value v2

let rec pp_db_value fmt = function
  | VDBINT n -> fprintf fmt "%d" n
  | VDBFUN (t, e) -> fprintf fmt "@[<2>fun . ->@ %a@]" pp_db_term t
  | VDBFIXFUN (t, env) ->
      fprintf fmt "@[<2>fixfun . ->@ fun . ->@ %a@]" pp_db_term t
  | DBTHUNK (t, _) -> fprintf fmt "@[<2><thunk %a>@]" pp_db_term t
  (* -- Pair -- *)
  | VDBPAIR (t1, t2) ->
      fprintf fmt "@[<2>(%a , %a)@]" pp_db_value t1 pp_db_value t2
  (* --- List  --- *)
  | VDBNIL -> fprintf fmt "nil"
  | VDBCONS (v1, v2) ->
      fprintf fmt "@[<2>(%a :: %a)@]" pp_db_value v1 pp_db_value v2
  (* --- Tree  --- *)
  | VDBLEAF v -> fprintf fmt "@[<2>leaf(%a)@]" pp_db_value v
  | VDBTREE (v1, v2) ->
      fprintf fmt "@[<2>tree(%a,@ %a)@]" pp_db_value v1 pp_db_value v2

let rec pp_instruction fmt = function
  | Ldi n -> fprintf fmt "Ldi %d" n
  | Push -> fprintf fmt "Push"
  | Search i -> fprintf fmt "Search %d" i
  | Pushenv -> fprintf fmt "Pushenv"
  | Popenv -> fprintf fmt "Popenv"
  | Mkclos code -> fprintf fmt "Mkclos (%a)" pp_code code
  | Mkrclos code -> fprintf fmt "Mkrclos (%a)" pp_code code
  | Apply -> fprintf fmt "Apply"
  | Test (c1, c2) -> fprintf fmt "Test (%a, %a)" pp_code c1 pp_code c2
  | Add -> fprintf fmt "Add"
  | Sub -> fprintf fmt "Sub"
  | Mult -> fprintf fmt "Mult"
  | Div -> fprintf fmt "Div"
  | Mkpair -> fprintf fmt "Mkpair"
  | Fst -> fprintf fmt "Fst"
  | Snd -> fprintf fmt "Snd"
  | Nil -> fprintf fmt "Nil"
  | Cons -> fprintf fmt "Cons"
  | CTest (c1, c2) -> fprintf fmt "CTest (%a, %a)" pp_code c1 pp_code c2
  | Hd -> fprintf fmt "Hd"
  | Tl -> fprintf fmt "Tl"
  | Leaf -> fprintf fmt "Leaf"
  | Mktree -> fprintf fmt "Mktree"
  | Item -> fprintf fmt "Item"
  | TTest (c1, c2) -> fprintf fmt "TTest (%a, %a)" pp_code c1 pp_code c2
  | Ltree -> fprintf fmt "Ltree"
  | Rtree -> fprintf fmt "Rtree"

and pp_code fmt (code : code) =
  fprintf fmt "[";
  let rec aux = function
    | END -> ()
    | i :: END -> pp_instruction fmt i
    | i :: rest ->
        fprintf fmt "%a; " pp_instruction i;
        aux rest
  in
  aux code;
  fprintf fmt "]"
