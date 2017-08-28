module Lambda (
    interpret
  , Expr(..)
  , Expr(..)
  , step
  , walk
  , substitute
  , shift
) where

import Lambda.Types

interpret :: String -> String
interpret s = "i saw a '" ++ s ++ "'" 
