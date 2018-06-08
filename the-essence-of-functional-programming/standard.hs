type I a = a

unitI a = a
a `bindI` k = k a
showI a = showval a

term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
          (Add (Con 10) (Con 11)))

type Name = String

data Term = Var Name
  | Con Int
  | Add Term Term
  | Lam Name Term
  | App Term Term

data Value = Wrong
             | Num Int
             | Fun (Value -> Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> I Value
interp (Var x) e = Main.lookup x e
interp (Con i) e = unitI (Num i)
interp (Add u v) e = add (interp u e) (interp v e)
interp (Lam x v) e = Fun (\a -> interp v ((x,a):e))
interp (App t u) e = apply (interp t e) (interp u e)

lookup :: Name -> Environment -> I Value
lookup x [] = unitI Wrong
lookup x ((y,b):e) = if x==y then unitI b else Main.lookup x e

add :: Value -> Value -> I Value
add (Num i) (Num j) = unitI (Num (i+j))
add a b = unitI Wrong

apply :: Value -> Value -> I Value
apply (Fun k) a = k a
apply f a = unitI Wrong

test :: Term -> String
test t = showI (interp t [])
