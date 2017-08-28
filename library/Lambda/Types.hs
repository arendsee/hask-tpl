module Lambda.Types
(
    Expr(..)
  , step
  , walk
  , substitute
  , shift
) where

-- Simply untyped lambda calculus with de Bruijn indices
data Expr
  = Var Integer
  | App Expr Expr
  | Abs Expr

walk :: Expr -> [Expr]
walk e = case (isval e) of
  True  -> [e]
  False -> [e] ++ walk (step e)

step :: Expr -> Expr
step = undefined

isval :: Expr -> Bool
isval (Var _) = True
isval _ = False

-- Follows definition 6.2.1 from TPL
shift :: Integer -> Expr -> Expr
shift i (Var   k) = undefined 
shift i (App a b) = undefined
shift i (Abs   a) = undefined

substitute :: Integer -> Expr -> Expr -> Expr
substitute i s (App a b) = App (substitute i s a) (substitute i s b)
substitute i s (Abs   a) = Abs (substitute (i+1) s a)
substitute i s (Var k)
  | i == k = shift i s
  | otherwise = Var k

instance Show Expr where
  show (Var x  ) = show(x)
  show (App a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (Abs   b) = "." ++ (show b)
