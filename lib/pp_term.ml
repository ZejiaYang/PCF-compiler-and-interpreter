open Term
open Db_term
open Format

let pp_op fmt = function
  | ADD -> fprintf fmt "+"
  | MINUS -> fprintf fmt "-"
  | MULTI -> fprintf fmt "*"
  | DIVIDE -> fprintf fmt "/"

let rec pp_term fmt t =
  match t with
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

let rec pp_db_term fmt t =
  match t with
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
