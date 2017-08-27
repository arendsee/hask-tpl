module Benjamin (
    interpret
  , Expr(..)
  , step
  , walk
) where

import Benjamin.Types

interpret :: String -> String
interpret s = "i saw a '" ++ s ++ "'" 
