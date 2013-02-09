-- | Implements common subexpression elimination (CSE) with hashconsig algorithm as described in
-- the paper 'Implementing Explicit and Finding Implicit Sharing in EDSLs' by Oleg Kiselyov. 
-- You can define your datatype as a fixpoint type. Then the only thing you need to perform CSE
-- is to define an instance of the class 'Traversable' for your datatype.
module Data.Fix.Cse (
    VarName, Dag, fromDag,
    -- * Implicit sharing
    cse,

    -- * Explicit sharing
    letCse, Let(..),
    letCata, letCataM,
    letWrapper
) where

import Control.Applicative hiding (empty)

import Data.Fix
import Control.Monad.Trans.State.Strict
import qualified Data.IntMap as IM
import Data.Traversable
import Control.Monad.Trans.Class(lift)

import Data.Fix.BiMap

type VarName = Int

-- | Directed acyclic graphs.
type Dag f = IM.IntMap (f VarName)

-- | If plain lists are enough for your case. 
fromDag :: Dag f -> [(VarName, f VarName)]
fromDag = IM.toList

-- | Performs common subexpression elimination with implicit sharing.  
cse :: (Eq (f Int), Ord (f Int), Traversable f) => Fix f -> Dag f
cse x = getDag $ execState (cataM hashcons x) empty


-- | With explicit sharing you provide user with the special function that
-- encodes let-bindings for your EDSL ('LetBind'). You should not use 'LetLift' case.
-- It's reserverd for the CSE algorithm. 
data Let f a
    = LetExp (f a)
    | LetBind a (a -> a)
    | LetLift VarName

-- | Helper function to make explicit let-bindings.
-- For exampe:
--
-- > newtype T = T { unT :: Fix (Let f) }
-- > 
-- > let_ :: T -> (T -> T) -> T
-- > let_ = letWrapper T unT
letWrapper :: (Fix (Let f) -> a) -> (a -> Fix (Let f)) -> a -> (a -> a) -> a
letWrapper to from a e = to $ Fix $ LetBind (from a) (from . e . to)

-- | Performs common subexpression elimination with explicit sharing.  
-- To make sharing explicit you can use the datatype 'Let'.
letCse :: (Eq (f Int), Ord (f Int), Traversable f)
   => Fix (Let f) -> Dag f
letCse x = getDag $ execState (letCataM hashcons x) empty

-- | Monadic catamorphism for fixpoint types wrapped in the type 'Let'.
letCataM :: (Applicative m, Monad m, Traversable f) =>
   (f a -> m a) -> Fix (Let f) -> m a
letCataM m expr = evalStateT (go expr) IM.empty
   where go    = phi . unFix
         phi x = case x of
                   LetLift var -> do
                                  s <- get
                                  return ((IM.!) s var)
                   LetExp a    -> (lift . m) =<< traverse go a
                   LetBind a e -> do
                                  v <- go a
                                  s <- get
                                  let var = IM.size s
                                  let s' = IM.insert var v s
                                  put s'
                                  go . e . Fix . LetLift $ var

-- | Catamorphism for fixpoint types wrapped in the type 'Let'.
letCata :: (Functor f, Traversable f) =>
   (f a -> a) -> Fix (Let f) -> a
letCata f expr = evalState (go expr) IM.empty
   where go    = phi . unFix
         phi x = case x of
                   LetLift var -> do
                                  s <- get
                                  return ((IM.!) s var)
                   LetExp a    -> traverse go a >>= return . f
                   LetBind a e -> do
                                  v <- go a
                                  s <- get
                                  let var = IM.size s
                                  let s' = IM.insert var v s
                                  put s'
                                  go . e . Fix . LetLift $ var

hashcons :: (Ord a) => a -> State (BiMap a) Int
hashcons e = do
  m <- get
  case lookup_key e m of
    Nothing -> let (k,m') = insert e m
               in  put m' >> return k
    Just k  -> return k
