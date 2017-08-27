module Benjamin.Types
(
    Expr(..)
  , step
  , walk
) where

data Expr
  = T 
  | F 
  | IfThen Expr Expr Expr
  | Zero
  | Succ Expr 
  | Pred Expr
  | IsZero Expr
  | Stuck
  deriving(Show)

is_value :: Expr -> Bool
is_value T        = True
is_value F        = True
is_value Zero     = True
is_value (Succ _) = True
is_value Stuck    = True
is_value _        = False

step :: Expr -> Expr
step (IfThen T a _)     = a
step (IfThen F _ b)     = b
step (IfThen Zero  _ _) = Stuck
step (IfThen Stuck _ _) = Stuck
step (IfThen x a b)     = IfThen (step x) a b
step (Pred Zero)        = Zero
step (Pred (Succ x))    = x
step (IsZero Zero)      = T
step (IsZero (Succ _))  = F
step (IsZero T)         = Stuck
step (IsZero F)         = Stuck
step (IsZero x)         = IsZero (step x)
step x = case (is_value x) of
  True  -> x
  False -> Stuck

walk :: Expr -> [Expr]
walk e = case (is_value e) of
  True  -> [e]
  False -> [e] ++ walk (step e) 
