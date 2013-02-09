module Exp where

import Control.Applicative hiding (Const)
import Data.Monoid
import Data.Traversable
import Data.Foldable

data Exp a 
    = Var String
    | Const Int
    | Add a a
    deriving (Show, Eq, Ord)

instance Functor Exp where
    fmap f x = case x of
        Var str     -> Var str
        Const n     -> Const n
        Add a b     -> Add (f a) (f b)

instance Foldable Exp where
    foldMap f x = case x of
        Add a b     -> f a <> f b
        _           -> mempty

instance Traversable Exp where
    traverse f x = case x of
        Var str     -> pure $ Var str
        Const n     -> pure $ Const n
        Add a b     -> Add <$> f a <*> f b

-----------------------------------------------
-- interpreters

-- ignore variables
expAsInt :: Exp Int -> Int
expAsInt x = case x of
    Var str     -> error "boom" 
    Const n     -> n
    Add a b     -> a + b

expAsDepth :: Exp Int -> Int
expAsDepth x = case x of
    Add a b     -> 1 + max a b
    _           -> 1



