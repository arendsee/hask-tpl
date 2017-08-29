module Moplac.Pieces
(
    lmID
  , lmT
  , lmF
  , lmIF
) where

import Moplac.Types

lmID :: Expr
lmID = Abs $ Var 0

lmT :: Expr
lmT = Abs $ Abs $ Var 0 

lmF :: Expr
lmF = Abs $ Abs $ Var 1 

lmIF :: Expr
lmIF = Abs $ Abs $ Abs $ App (App (Var 0) (Var 1)) (Var 2)
