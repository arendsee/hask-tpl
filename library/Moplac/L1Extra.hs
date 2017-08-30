module Moplac.L1Extra
(
    l1_ID
  , l1_T
  , l1_F
  , l1_IF
  , l1_Zero
  , l1_isbool
  , l1_isnum
  , l1_succ
  , l1_pred
  , l1_num
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

l1_Zero :: L1
l1_Zero = L1_Abs $ L1_Abs $ L1_Var 1

-- TODO: Find a representation for negative integers and lose the Maybe. The
-- representation must compatible with pred and succ.
l1_num :: Int -> Maybe L1
l1_num i
  | i < 0 = Nothing
  | otherwise = Just (L1_Abs $ L1_Abs $ inner' i) where
    inner' :: Int -> L1
    inner' 0 = L1_Var 1
    inner' j = L1_App (L1_Var 0) (inner' (j-1))



-- The following are Haskell functions for manipulating lambda calculus
-- objects. TODO: Add the pure lambda calculus equivalents.

l1_isnum :: L1 -> Bool
l1_isnum (L1_Abs (L1_Abs (L1_Var 1))) = True
l1_isnum (L1_Abs (L1_Abs x)) = is_var1 x where
  is_var1 :: L1 -> Bool
  is_var1 (L1_Var 1) = True
  is_var1 (L1_App (L1_Var 0) x') = is_var1 x'
  is_var1 _ = False
l1_isnum _ = False

l1_isbool :: L1 -> Bool
l1_isbool (L1_Abs (L1_Abs (L1_Var 0))) = True
l1_isbool (L1_Abs (L1_Abs (L1_Var 1))) = True
l1_isbool _ = False

l1_succ :: L1 -> Maybe L1
l1_succ (L1_Abs (L1_Abs x))
  | l1_isnum succ' = Just succ'
  | otherwise = Nothing
  where
    succ' = L1_Abs (L1_Abs (L1_App (L1_Var 0) x))
l1_succ _ = Nothing

l1_pred :: L1 -> Maybe L1
l1_pred (L1_Abs (L1_Abs (L1_App (L1_Var 0) x)))
  | l1_isnum pred' = Just pred'
  | otherwise = Nothing
  where
    pred' = L1_Abs (L1_Abs x)
l1_pred _ = Nothing
