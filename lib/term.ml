open Format

type term =
  | VAR of string
  | FUN of string * term
  | APP of term * term
  | INT of int
  | BOP of term * op * term
  | IFZ of term * term * term
  | FIX of string * term
  | LET of string * term * term
  | PAIR of term * term
  | FST of term
  | SND of term

and op = ADD | MINUS | MULTI | DIVIDE

type env = END | NEXT of string * value * env

and value =
  | VINT of int
  | VFUN of string * term * env
  | VFIX of string * term * env
  | VFIXFUN of string * string * term * env
  | VPAIR of value * value
  | THUNK of term * env (* legacy for call-by-name *)

let ( ++ ) a b = BOP (a, ADD, b)
let ( -- ) a b = BOP (a, MINUS, b)
let ( ** ) a b = BOP (a, MULTI, b)
let ( // ) a b = BOP (a, DIVIDE, b) (* integer division, truncating *)
let ( @@ ) f x = APP (f, x)

(* use FIX operator *)
(* pulling n out of the lambda, more efficient, less closure *)
let exp =
  FUN
    ( "n",
      FIX
        ( "f",
          FUN
            ( "p",
              IFZ (VAR "p", INT 1, VAR "n" ** (VAR "f" @@ (VAR "p" -- INT 1)))
            ) ) )

(* Primality Check - O(n) *)
(* base case 0, 1 : not prime. inductive case: check the reminder*)
let reminder = FUN ("a", FUN ("b", VAR "a" -- ((VAR "b" ** VAR "a") // VAR "b")))

let is_prime =
  FUN
    ( "n",
      IFZ
        ( VAR "n",
          INT 0,
          IFZ
            ( VAR "n" -- INT 1,
              INT 0,
              APP
                ( FIX
                    ( "f",
                      FUN
                        ( "a",
                          IFZ
                            ( VAR "n" -- VAR "a",
                              INT 1,
                              (* is prime *)
                              IFZ
                                ( reminder @@ VAR "n" @@ VAR "a",
                                  INT 0,
                                  (* not prime *)
                                  VAR "f" @@ (VAR "a" ++ INT 1) ) ) ) ),
                  INT 2 ) ) ) )

(* Polynomials in PCF *)
(* encode a list of integers into a single natural number and evaluate that polynomial*)
(* Cantor Pairing Funnction: bijection between a pair of numbers to a single number*)
let k =
  FUN
    ( "n",
      FUN
        ( "p",
          (((VAR "n" ++ VAR "p") ** (VAR "n" ++ VAR "p" ++ INT 1)) // INT 2)
          ++ VAR "n" ) )

let tri = FUN ("u", (VAR "u" ** (VAR "u" ++ INT 1)) // INT 2)

let u =
  FUN
    ( "q",
      FIX
        ( "f",
          FUN
            ( "u",
              IFZ
                ( VAR "q" ++ INT 1 -- (tri @@ VAR "u"),
                  VAR "u" -- INT 1,
                  VAR "f" @@ (VAR "u" ++ INT 1) ) ) )
      @@ INT 0 )

let k'1 = FUN ("q", LET ("u", u @@ VAR "q", VAR "q" -- (tri @@ VAR "u")))
let k'2 = FUN ("q", LET ("u", u @@ VAR "q", VAR "u" -- (k'1 @@ VAR "q")))
let l = FUN ("n", FUN ("p", k @@ VAR "n" @@ (VAR "p" ++ INT 1)))

let eval_poly =
  FUN
    ( "poly",
      FUN
        ( "x",
          FIX
            ( "f",
              FUN
                ( "i",
                  FUN
                    ( "cur_poly",
                      IFZ
                        ( VAR "cur_poly",
                          INT 0,
                          LET
                            ( "pair",
                              VAR "cur_poly" -- INT 1,
                              LET
                                ( "coef",
                                  k'1 @@ VAR "pair",
                                  LET
                                    ( "rest_poly",
                                      k'2 @@ VAR "pair",
                                      (VAR "coef" ** (exp @@ VAR "x" @@ VAR "i"))
                                      ++ (VAR "f" @@ (VAR "i" ++ INT 1)
                                        @@ VAR "rest_poly") ) ) ) ) ) ) )
          @@ INT 0 @@ VAR "poly" ) )
