module L0
(
  Syntax(..)
) where

type VarName = String

data Syntax =
  Tok_Con VarName T2
  Tok_Abs VarName T2 Syntax
  Tok_App L0 [L0]
  Tok_Var VarName
  Tok_Arg VarName T2
