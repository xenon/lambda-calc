(* Untyped Lambda Calculus with:
   - Normal Order Reduction -> Lazy evaluation
   - Static Binding instead of naive dynamic binding
   - Reserves variables of type rN where N is an integer.
*)

type expr = Var of string
          | Abs of string * expr
          | App of expr * expr

let rec free var = function
  | Var(s) -> (var = s)
  | Abs(s, e) -> ((not (var = s)) && (free var e))
  | App(n, m) -> (free var n) || (free var m)

let rec bnf = function
  | Var(_) -> true
  | Abs(_, e) -> (bnf e)
  | App(n, m) ->
    (match n with
     | Var(_) -> (bnf m)
     | Abs(_, _) -> false
     | App(_, _) -> ((bnf n) && (bnf m))
    )

let substitute =
  let n = ref 0 in
  fun i j k ->
    let rec subst a x e =
      (match e with
       | Var(s) -> if (s = x) then a else e
       | Abs(s, e2) -> if (s = x) then e else (
         if (free s a) then (
             incr n;
             let fresh = ("r" ^ (string_of_int !n))
             in
             Abs(fresh, (subst a x (subst (Var(fresh)) s e2)))
           ) else (
             Abs(s,(subst a x e2))
           )
         )
       | App(n, m) -> App((subst a x n), (subst a x m))
      )
    in
    (subst i j k)

let rec reduce e =
  let rec nor_step = function
    | Var(s) -> Var(s)
    | Abs(s, e) -> Abs(s, (nor_step e))
    | App(n, m) ->
      (match n with
       | Var(_) -> App(n, (nor_step m))
       | Abs(s,e) -> (nor_step (substitute m s e))
       | App(_,_) -> if (bnf n) then App(n, (nor_step m))
                                else App((nor_step n), m)
      )
  in
  if (bnf e) then e else (reduce (nor_step e))

let print_expr e =
  let rec print_body = function
    | Var(s) -> Printf.printf "%s" s; ()
    | Abs(s, e) -> Printf.printf "(λ%s." s;
      (print_body e);
      Printf.printf ")"; ()
    | App(n, m) -> Printf.printf "(";
      (print_body n);
      Printf.printf " ";
      (print_body m);
      Printf.printf ")"; ()
  in
  (print_body e); Printf.printf "\n"; ()

(* Some functions *)
let ltrue = Abs("t", (Abs("f", (Var "t"))))
let lid = Abs("x", (Var "x"))
