module Benjamin (
    interpret
  , A1_Expr(..)
  , L1_Expr(..)
  , L1DB_Expr(..)
  , step
  , walk
) where

import Benjamin.Types

interpret :: String -> String
interpret s = "i saw a '" ++ s ++ "'" 
