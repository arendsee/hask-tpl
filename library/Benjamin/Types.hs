module Benjamin.Types
(
    Expr(..)
  , step
  , walk
  , substitute
  , shift
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
  = A1_T
  | A1_F
  | A1_IfThen A1_Expr A1_Expr A1_Expr
  | A1_Zero
  | A1_Succ A1_Expr
  | A1_Pred A1_Expr
  | A1_IsZero A1_Expr
  | A1_Stuck
  deriving(Show)

instance Lang A1_Expr where
  step (A1_IfThen A1_T a _)     = a
  step (A1_IfThen A1_F _ b)     = b
  step (A1_IfThen A1_Zero  _ _) = A1_Stuck
  step (A1_IfThen A1_Stuck _ _) = A1_Stuck
  step (A1_IfThen x a b)        = A1_IfThen (step x) a b
  step (A1_Pred A1_Zero)        = A1_Zero
  step (A1_Pred (A1_Succ x))    = x
  step (A1_IsZero A1_Zero)      = A1_T
  step (A1_IsZero (A1_Succ _))  = A1_F
  step (A1_IsZero A1_T)         = A1_Stuck
  step (A1_IsZero A1_F)         = A1_Stuck
  step (A1_IsZero x)            = A1_IsZero (step x)
  step x = case (is_value x) of
    True  -> x
    False -> A1_Stuck

  is_value A1_T        = True
  is_value A1_F        = True
  is_value A1_Zero     = True
  is_value (A1_Succ _) = True
  is_value A1_Stuck    = True
  is_value _           = False


type VarName = String

data L1_Expr
  = L1_Var VarName
  | L1_App L1_Expr L1_Expr
  | L1_Abs VarName L1_Expr

-- Simply untyped lambda calculus with de Bruijn indices
-- This format will be used internally
data L1DB_Expr
  = L1DB_Var Integer
  | L1DB_App L1DB_Expr L1DB_Expr
  | L1DB_Abs VarName L1DB_Expr   -- VarName is needed to convert back to L1

to_db_index :: L1_Expr -> L1DB_Expr
to_db_index e = undefined

-- Follows definition 6.2.1 from TPL
shift :: Integer -> L1DB_Expr -> L1DB_Expr
shift i (L1DB_Var   k) = undefined 
shift i (L1DB_App a b) = undefined
shift i (L1DB_Abs n a) = undefined

substitute :: Integer -> L1DB_Expr -> L1DB_Expr -> L1DB_Expr
substitute i s (L1DB_App a b) = L1DB_App (substitute i s a) (substitute i s b)
substitute i s (L1DB_Abs n a) = L1DB_Abs n (substitute (i+1) s a)
substitute i s (L1DB_Var k)
  | i == k = shift i s
  | otherwise = L1DB_Var k

-- substitute :: [(Integer, String)] -> L1DB_Expr -> ThrowsError L1_Expr
-- substitute js (L1DB_Var k) = case (lookup k js) of
--   Nothing -> Left "Aww shucks, couldn't find that number"
--   Just s  -> return $ L1_Var s
-- substitute js (L1DB_App a b) = L1_App <$> (substitute js a) <*> (substitute js b)
-- substitute js (L1DB_Abs s a) = L1_Abs s <$> (substitute new_js a) where
--   new_js =
--     (map (\(i,x) -> (i+1,x)) js) -- Increment outer free variables by 1
--     ++
--     [(0, s)] -- Add the new locally bound variable

instance Show L1_Expr where
  show (L1_Var x  ) = x
  show (L1_App a b) = "(" ++ (show a) ++ ")(" ++ (show b) ++ ")"
  show (L1_Abs x b) = "L " ++ x ++ " . " ++ (show b)

instance Show L1DB_Expr where
  show (L1DB_Var x  ) = show(x)
  show (L1DB_App a b) = "(" ++ (show a) ++ ")(" ++ (show b) ++ ")"
  show (L1DB_Abs _ b) = "L . " ++ (show b)

instance Lang L1_Expr where
  step (L1_Var   x) = L1_Var x
  step (L1_App _ _) = undefined
  step (L1_Abs _ _) = undefined

  is_value (L1_Var _) = True
  is_value _ = False
