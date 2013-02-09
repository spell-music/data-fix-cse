-- | Explicit sharing
module Main where

import Data.Fix
import Data.Fix.Cse

import Exp

newtype T = T { unT :: Fix (Let Exp) }
   
instance Num T where
    (+) a b = T $ Fix $ LetExp $ Add (unT a) (unT b)
    fromInteger = T . Fix . LetExp . Const . fromInteger

    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined


var :: String -> T
var = T . Fix . LetExp . Var

x = var "x"
y = var "y"
z = var "z"


let_ :: T -> (T -> T) -> T
let_ = letWrapper T unT

mulT :: Int -> T -> T
mulT 0 _ = 0
mulT 1 x = x
mulT n x
    | n `mod` 2 == 0    = let_ (x + x) $ \y -> mulT (n `div` 2) y
    | otherwise         = mulT (n - 1) x + x 

-- interpreters

asInt :: T -> Int
asInt = letCata expAsInt . unT

asDepth :: T -> Int
asDepth = letCata expAsDepth . unT

exec :: T -> Int
exec = length . fromDag . letCse . unT

-----------------------------------------------
-- expression

expr = mulT (2^30) 2

main = do
    print $ exec expr
    putStrLn "" 
    putStr "value: "
    print $ asInt expr
    putStrLn ""
    putStr "depth: "
    print $ asDepth expr


