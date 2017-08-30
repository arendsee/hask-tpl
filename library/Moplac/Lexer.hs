module Moplac.Lexer where

import Control.Monad
import Text.ParserCombinators.Parsec

expr' :: Parser Syntax
expr' = do
      try con'
  <|> try app'
  <|> try abs'
  <|> try var'
  <|> try between (Char '(') (Char ')') >>= expr'

identifier' :: Parser String
identifier' = many (letters <|> char '_')

simple_type' :: Parser T2
simple_type' = do
  i <- identifier'
  return $ T2_Var i

complex_type' :: Parser T2
complex_type' = 
  between (char '(') (char ')') >>=
  sepBy typ' (string "->") >>=
  asT2'
  where
    asT2' :: [String] -> T2
    asT2' []     = T2_Unk
    asT2' [s]    = T2_Var s
    asT2' (s:ss) = T2_Func (T2_Var s) (asT2' ss)

typ' :: Parser T2
typ' = do try simple_type' <|> try complex_type'

app' :: Parser Syntax -- Tok_App
app' = do
  e  <- expr'
  es <- many expr'
  return $ Tok_App e es

abs' :: Parser Syntax -- Tok_Abs
abs' = do
  _ <- char '\\'
  v <- identifier'
  _ <- many space
  t <- char ':' >> optionMaybe typ'
  _ <- many space
  _ <- char '.'
  _ <- many space
  b <- expr
  return $ Tok_Abs v (fromMaybe T2_Unk t) b

var' :: Parser Syntax -- Tok_Var
var' = identifier'

con' :: Parser Syntax -- Tok_Con
con' = do
  v <- identifier'
  _ <- many space
  _ <- char ':'
  _ <- many space
  t <- typ'
  return $ Tok_Con v t
