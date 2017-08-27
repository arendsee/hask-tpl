module Benjamin.Types
(
    A1_Expr(..)
  , L1_Expr(..)
  , step
  , walk
) where

-- Over the course of TPL, several toy programming languages are created.
class Lang expr where
  step :: expr -> expr
  is_value :: expr -> Bool
  walk :: expr -> [expr]
  walk e = case (is_value e) of
    True  -> [e]
    False -> [e] ++ walk (step e) 



data A1_Expr
  = T 
  | F 
  | IfThen A1_Expr A1_Expr A1_Expr
  | Zero
  | Succ A1_Expr 
  | Pred A1_Expr
  | IsZero A1_Expr
  | Stuck
  deriving(Show)

instance Lang A1_Expr where
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

  is_value T        = True
  is_value F        = True
  is_value Zero     = True
  is_value (Succ _) = True
  is_value Stuck    = True
  is_value _        = False



type VarName = String

data L1_Expr
  = Variable VarName
  | Application L1_Expr L1_Expr
  | Abstraction VarName L1_Expr  

instance Show L1_Expr where
  show (Variable x) = x
  show (Application a b) = "(" ++ (show a) ++ ")(" ++ (show b) ++ ")"
  show (Abstraction x b) = "L " ++ x ++ " . " ++ (show b)

instance Lang L1_Expr where
  step (Variable x) = Variable x
  step (Application _ _) = undefined
  step (Abstraction _ _) = undefined

  is_value (Variable _) = True
  is_value _ = False
