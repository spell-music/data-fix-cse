-- | Implicit sharing
module Main where

import Data.Monoid
import Control.Applicative hiding (Const)

import Data.Fix
import Data.Fix.Cse

import Exp

-----------------------------------------------
-- implicit sharing

newtype T = T { unT :: Fix Exp }
    deriving (Show, Eq)

   
instance Num T where
    (+) a b = T $ Fix $ Add (unT a) (unT b)
    fromInteger = T . Fix . Const . fromInteger

    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined

var :: String -> T
var = T . Fix . Var

x = var "x"
y = var "y"
z = var "z"


mulT :: Int -> T -> T
mulT 0 _ = 0
mulT 1 x = x
mulT n x
    | n `mod` 2 == 0    = mulT (n `div` 2) (x + x)
    | otherwise         = mulT (n - 1) x + x 

-- interpreters

asInt :: T -> Int
asInt = cata expAsInt . unT

asDepth :: T -> Int
asDepth = cata expAsDepth . unT

exec :: T -> Dag Exp
exec = cse . unT

-----------------------------------------------
-- expression

expr = mulT (2^20) 2

main = do
    putStrLn $ show $ length $ fromDag $ exec expr


