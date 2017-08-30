{-# LANGUAGE MultiParamTypeClasses #-}

module Moplac.L2
(
    L2(..)
) where

-- import Moplac.L1
-- import Moplac.L1Extra
-- import Moplac.TypedCalculi
import Moplac.Types

data L2
  = L2_Var Int 
  | L2_App L2 L2
  | L2_Abs T2 L2 

-- instance Typed L2 T2 where
--   erase (L2_Var   i)  = L1_Var i
--   erase (L2_App a b)  = L1_App (erase a) (erase b)
--   erase (L2_Abs _ x)  = L1_Abs (erase x)

  -- infer cs (L2_Var i) = lookup i cs
  -- infer cs (L2_App (Abs a) b) = L2_App a' b' where
  --   b' = infer cs b
  --   a' =

instance Show L2 where
  show (L2_Var   x  ) = show x
  show (L2_App   a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (L2_Abs t   b) = ".:" ++ (show t) ++ (show b)
