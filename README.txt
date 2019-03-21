lcalc.ml:
A λ-calculus interpreter in OCaml.

Features:
  1) Implements Lazy evaulation and normal order reduction.
  2) Implements static binding to prevent incorrect variable substitution.
  
Would be nice:
  Make a parser so we can convert strings of λ-calculus expressions into their
  internal representation within the code.
  
To use:
  1) Type your expression in OCaml:
    eg: let identityfn = (Abs "x" (Var "x"))
  2) Print it using print_expr:
    eg: (print_expr identityfn)
        -> (λx.x)
  3) Reduce expressions to normal form by using reduce:
    eg: (reduce (App(identityfn, (Var "y")))
    -> (Var "y")
    as in: (λx.x)y --(reduces to)--> y 
