module Moplac.Types
(
  T2(..)
) where

data T2
  = T2_Bool
  | T2_Func T2 T2

instance Show T2 where
  show T2_Bool = "Bool"
  show (T2_Func a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
