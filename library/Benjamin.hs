module Benjamin (
    interpret
  , Expr(..)
  , Expr(..)
  , step
  , walk
  , substitute
  , shift
) where

import Benjamin.Types

interpret :: String -> String
interpret s = "i saw a '" ++ s ++ "'" 
