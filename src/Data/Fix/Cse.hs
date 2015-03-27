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
    letWrapper,

    -- * Framed sharing
    -- | If your EDSL contains imperative if-the-else blocks
    -- we need to use special version of the CSE. It allocates
    -- frames per each if- or else block. So that variables
    -- from different if-the-else branches don't get messed up.
    -- We need to allocate a new frame for each branch.
    -- We can do it with special structure @FrameInfo@.    
    FrameInfo(..), cseFramed
) where

import Control.Applicative hiding (empty)

import Data.Fix
import Control.Monad.Trans.State.Strict
import qualified Data.IntMap as IM
import Data.Traversable
import Control.Monad.Trans.Class(lift)

import Data.Fix.BiMap
import qualified Data.Fix.BiMapFramed as F

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


-- | Marker type for creation frames of variables.
-- Start new frame when if-block starts, create next frame
-- when you go into the next branch of the same block (with else ir elif),
-- stop frame when leaving the if-then-else block. Use no frame for all 
-- other expressions.
data FrameInfo = NoFrame | StartFrame | StopFrame | NextFrame
  deriving (Show, Eq, Ord)

-- | Performs common subexpression elimination with implicit sharing using information of frames.  
-- It doesn't share the variables in different branches of imperative if-then-else block.
cseFramed :: (Eq (f Int), Ord (f Int), Traversable f) => (f Int -> FrameInfo) -> Fix f -> Dag f
cseFramed getFrameInfo x = F.getDag $ execState (cataM (hashconsFramed getFrameInfo) x) F.empty

hashconsFramed :: (Ord a) => (a -> FrameInfo) -> a -> State (F.BiMap a) Int
hashconsFramed getFrameInfo e = do
  m' <- get
  let m = case getFrameInfo e of
        NoFrame    -> m'
        StartFrame -> F.startFrame m'
        StopFrame  -> F.stopFrame m'
        NextFrame  -> F.nextFrame m'      

  case F.lookup_key e m of
    Nothing -> let (k,m') = F.insert e m
               in  put m' >> return k
    Just k  -> return k
