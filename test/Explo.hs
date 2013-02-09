{-# LANGUAGE RankNTypes #-}

-- | Explicit sharing
module Main where

import Fix
import DAGo

import Exp

-- Alas, the following doesn;t work out because of the type of LetBind...
-- newtype T = T { unT :: forall w. Fix (Let Exp w) }

newtype T w = T { unT :: Fix (Let Exp w) }

instance Show (T w)
instance Eq (T w)

instance Num (T w) where
   (+) a b = T $ Fix $ LetExp $ Add (unT a) (unT b)
   fromInteger x = T $ (Fix . LetExp . Const . fromInteger $ x)

   (*) = undefined
   negate = undefined
   abs = undefined
   signum = undefined


var :: String -> T w
var v = T $ (Fix . LetExp . Var $ v)

x = var "x"
y = var "y"
z = var "z"

let_ :: T w -> (T w -> T w) -> T w
let_ = T $ Fix $ LetBind (unT a) (unT . e . T)

mulT :: Int -> T w -> T w
mulT 0 _ = 0
mulT 1 x = x
mulT n x
   | n `mod` 2 == 0    = let_ (x + x) $ \y -> mulT (n `div` 2) y
   | otherwise         = mulT (n - 1) x + x

-- interpreters

asInt :: T Int -> Int
asInt = letCata expAsInt . unT

asDepth :: T Int -> Int
asDepth = letCata expAsDepth . unT

exec :: T Int -> BiMap (Exp Int)
exec = letCse . unT

-----------------------------------------------
-- expression

-- expr = mulT (10^5) 2
-- expr = mulT 8 2
expr = mulT (10^30) 2

main  = do
   print $ exec expr
   putStrLn ""
   putStr "value: "
   print $ asInt expr
   putStrLn ""
   putStr "depth: "
   print $ asDepth expr
