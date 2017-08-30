module Moplac.Types
(
  T2(..)
) where

data T2
  = T2_Unk 
  | T2_Var String
  | T2_Func T2 T2

instance Show T2 where
  show (T2_Var s) = s 
  show (T2_Func a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show T2_Unk = "?"
