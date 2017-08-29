module Moplac.L2
(
    L2(..)
) where

import Moplac.L1
import Moplac.TypedCalculi
import Moplac.Types

data L2
  = L2_Var Integer (Maybe T2)
  | L2_App L2 L2
  | L2_Abs L2 (Maybe T2)

instance Typed L2 where
  erase (L2_Var i _) = L1_Var i
  erase (L2_App a b) = L1_App (erase a) (erase b)
  erase (L2_Abs x _) = L1_Abs (erase x)

instance Show L2 where
  show (L2_Var x   t) = show(x) ++ ":" ++ show(t)
  show (L2_App a b  ) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (L2_Abs   b t) = ".:" ++ (show t) ++ (show b)
