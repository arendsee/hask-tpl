module Moplac.Types
(
    Expr(..)
  , step
  , walk
  , substitute
  , shift
  , isVal
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

instance Show Expr where
  show (Var x  ) = show(x)
  show (App a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (Abs   b) = "." ++ (show b)

type Result = Either String Expr

walk :: Expr -> [Result]
walk e = case (isVal e) of
  True  -> [Right e]
  False -> [Right e] ++ es where
    es = either (\x -> [Left x]) walk (step e)

step :: Expr -> Result
step (Var         k) = Right $ Var k
step (App (Abs a) b) = Right $ substitute 1 (shift b) a
step (App  _      _) = Left  $ "Invalid application"
step (Abs         b) = Abs <$> (step b)

isVal :: Expr -> Bool
isVal (Var _) = True
isVal _ = False

-- Get free variables as distance beyond root
-- For example,
--   * `0`             - [1]
--   * `.1`            - [1]
--   * `..2`           - [1]
--   * `...2`          - [ ]
--   * `...(0 1 2 3 4) - [1,2]
getFV :: Expr -> [Integer]
getFV e = getFV' 0 e where
  getFV' i (Abs   b) = (getFV' (i+1) b)
  getFV' i (App a b) = (getFV' i a) ++ (getFV' i b)
  getFV' i (Var j)
    | j >= i    = [j - i + 1]
    | otherwise = []

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
  nest' 0 e' = e'
  nest' i e' = Abs $ nest' (i-1) e'

-- Follows definition 6.2.1 from TPL
shift :: Expr -> Expr
shift e = shift' 0 e where
  shift' i (Abs   a) = Abs (shift' (i+1) a)
  shift' i (App a b) = App (shift' i a) (shift' i b)
  shift' i (Var   k) 
    | k >= i    = Var (k + 1)
    | otherwise = Var k

substitute :: Integer -> Expr -> Expr -> Expr
substitute i s (Abs   e) = Abs (substitute (i+1) (shift s) e)
substitute i s (App a b) = App (substitute i s a) (substitute i s b)
substitute i s (Var   k)
  | i == k    = s
  | otherwise = Var k
