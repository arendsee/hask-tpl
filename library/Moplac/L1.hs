module Moplac.L1
(
    L1(..)
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
data L1
  = L1_Var Integer
  | L1_App L1 L1
  | L1_Abs L1
  deriving(Ord, Eq)

instance Show L1 where
  show (L1_Var x  ) = show(x)
  show (L1_App a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (L1_Abs   b) = "." ++ (show b)

eval :: L1 -> L1
eval (L1_Var   x) = L1_Var x
eval (L1_Abs   x) = L1_Abs $ eval x
eval (L1_App a b)
  | expr2 == L1_App a b = expr2
  | otherwise        = eval expr2
  where
    expr2 = application (eval a) (eval b)

application :: L1 -> L1 -> L1
application (L1_Abs a) b = substitute 0 (shift b) a
application a b       = L1_App a b

rootDepth :: L1 -> Integer
rootDepth = safeMax' . getFV where 
  safeMax' :: [Integer] -> Integer
  safeMax' [] = 0
  safeMax' (x:xs)
    | x > max'  = x
    | otherwise = max'
    where
      max' = safeMax' xs

isClosed :: L1 -> Bool
isClosed e = rootDepth e == 0 

close :: L1 -> L1
close e = nest' (rootDepth e) e where
  nest' :: Integer -> L1 -> L1
  nest' 0 e' = e'
  nest' i e' = L1_Abs $ nest' (i-1) e'

substitute
  :: Integer -- nest depth
  -> L1    -- replacement expression
  -> L1    -- main expression
  -> L1
substitute i s (L1_Abs   e) = L1_Abs (substitute (i+1) (shift s) e)
substitute i s (L1_App a b) = L1_App (substitute i s a) (substitute i s b)
substitute i s (L1_Var   k)
  | i == k    = s
  | otherwise = L1_Var k

-- Get free variables as distance beyond root
-- For example,
--   * `0`             - [1]
--   * `.1`            - [1]
--   * `..2`           - [1]
--   * `...2`          - [ ]
--   * `...(0 1 2 3 4) - [1,2]
getFV :: L1 -> [Integer]
getFV e = getFV' 0 e where
  getFV' :: Integer -> L1 -> [Integer]
  getFV' i (L1_Abs   b) = (getFV' (i+1) b)
  getFV' i (L1_App a b) = (getFV' i a) ++ (getFV' i b)
  getFV' i (L1_Var j)
    | j >= i    = [j - i + 1]
    | otherwise = []

-- Follows definition 6.2.1 from TPL
shift :: L1 -> L1
shift e = shift' 0 e where
  shift' :: Integer -> L1 -> L1
  shift' i (L1_Abs   a) = L1_Abs (shift' (i+1) a)
  shift' i (L1_App a b) = L1_App (shift' i a) (shift' i b)
  shift' i (L1_Var   k) 
    | k >= i    = L1_Var (k + 1)
    | otherwise = L1_Var k
