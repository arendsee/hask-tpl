module Moplac.L1Extra
(
    l1_ID
  , l1_T
  , l1_F
  , l1_IF
) where

import Moplac.L1

l1_ID :: L1
l1_ID = L1_Abs $ L1_Var 0

l1_T :: L1
l1_T = L1_Abs $ L1_Abs $ L1_Var 0 

l1_F :: L1
l1_F = L1_Abs $ L1_Abs $ L1_Var 1 

l1_IF :: L1
l1_IF =
  L1_Abs $
    L1_Abs $
      L1_Abs $ L1_App (L1_App (L1_Var 0) (L1_Var 1)) (L1_Var 2)
