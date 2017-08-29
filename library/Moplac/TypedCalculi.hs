module Moplac.TypedCalculi (
  Typed(..)
) where

import Moplac.L1

class Typed a where
  erase :: a -> L1
  -- check :: a -> Bool
  -- infer :: a -> t -- need type dependencies
