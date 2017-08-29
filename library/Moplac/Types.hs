module Moplac.Types
(
    Expr(..)
  , eval
  , substitute
  , application
  , shift
  , getFV
  , rootDepth
  , isClosed
  , close
) where

-- Simply untyped lambda calculus with de Bruijn indices
data Expr
  = Var Integer
  | App Expr Expr
  | Abs Expr
  deriving(Ord, Eq)

instance Show Expr where
  show (Var x  ) = show(x)
  show (App a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (Abs   b) = "." ++ (show b)

eval :: Expr -> Expr
eval (Var   x) = Var x
eval (Abs   x) = Abs $ eval x
eval (App a b)
  | expr2 == App a b = expr2
  | otherwise        = eval expr2
  where
    expr2 = application (eval a) (eval b)

application :: Expr -> Expr -> Expr
application (Abs a) b = substitute 0 (shift b) a
application a b       = App a b

rootDepth :: Expr -> Integer
rootDepth = safeMax' . getFV where 
  safeMax' :: [Integer] -> Integer
  safeMax' [] = 0
  safeMax' (x:xs)
    | x > max'  = x
    | otherwise = max'
    where
      max' = safeMax' xs

isClosed :: Expr -> Bool
isClosed e = rootDepth e == 0 

close :: Expr -> Expr
close e = nest' (rootDepth e) e where
  nest' :: Integer -> Expr -> Expr
  nest' 0 e' = e'
  nest' i e' = Abs $ nest' (i-1) e'

substitute
  :: Integer -- nest depth
  -> Expr    -- replacement expression
  -> Expr    -- main expression
  -> Expr
substitute i s (Abs   e) = Abs (substitute (i+1) (shift s) e)
substitute i s (App a b) = App (substitute i s a) (substitute i s b)
substitute i s (Var   k)
  | i == k    = s
  | otherwise = Var k

-- Get free variables as distance beyond root
-- For example,
--   * `0`             - [1]
--   * `.1`            - [1]
--   * `..2`           - [1]
--   * `...2`          - [ ]
--   * `...(0 1 2 3 4) - [1,2]
getFV :: Expr -> [Integer]
getFV e = getFV' 0 e where
  getFV' :: Integer -> Expr -> [Integer]
  getFV' i (Abs   b) = (getFV' (i+1) b)
  getFV' i (App a b) = (getFV' i a) ++ (getFV' i b)
  getFV' i (Var j)
    | j >= i    = [j - i + 1]
    | otherwise = []

-- Follows definition 6.2.1 from TPL
shift :: Expr -> Expr
shift e = shift' 0 e where
  shift' :: Integer -> Expr -> Expr
  shift' i (Abs   a) = Abs (shift' (i+1) a)
  shift' i (App a b) = App (shift' i a) (shift' i b)
  shift' i (Var   k) 
    | k >= i    = Var (k + 1)
    | otherwise = Var k
