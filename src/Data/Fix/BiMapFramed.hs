-- Establishing a bijection between the values of the type a and integers, with
-- the operations to retrieve the value given its key,
-- to find the key for the existing value, and to extend the 
-- bijection with a new association.

-- The type 'a' of values should at least permit equality comparison;
-- In the present implementation, we require 'a' to be a member
-- of Ord.

-- There are many ways to implement bi-maps, for example, using hash tables,
-- or maps.
-- Our implementation uses Data.Map and Data.IntMap to record
-- both parts of the association.

module Data.Fix.BiMapFramed (
	      BiMap, empty, getDag,
	      lookup_key, 
	      lookup_val,
	      startFrame, stopFrame, nextFrame,   
	      insert,
	      size,
	     )
    where

import Data.Maybe
import qualified Data.Map    as M
import qualified Data.IntMap as IM

data BiMap a = BiMap [M.Map a Int] (IM.IntMap a)

startFrame :: BiMap a -> BiMap a
startFrame (BiMap ms im) = BiMap (M.empty : ms) im

stopFrame :: BiMap a -> BiMap a
stopFrame (BiMap ms im) = BiMap (if (null ms) then [M.empty] else tail ms) im

nextFrame :: BiMap a -> BiMap a
nextFrame = startFrame . stopFrame

getDag :: BiMap a -> IM.IntMap a
getDag (BiMap _ a) = a

lookup_key :: Ord a => a -> BiMap a -> Maybe Int
lookup_key e (BiMap ms _) = case dropWhile isNothing $ fmap (M.lookup e) ms of
	Just a : [] -> Just a
	_           -> Nothing

-- Find a value for a key
lookup_val :: Int -> BiMap a -> a
lookup_val k (BiMap _ m) = m IM.! k

-- Insert the value and return the corresponding key
-- and the new map
-- Alas, Map interface does not have an operation to insert and find the index 
-- at the same time (although such an operation is easily possible)
insert :: Ord a => a -> BiMap a -> (Int, BiMap a)
insert v (BiMap (m:ms) im) = (k, BiMap (m':ms) im')
 where m'  = M.insert v k m
       im' = IM.insert k v im
       k   = IM.size im

empty :: BiMap a
empty = BiMap [M.empty] (IM.empty)

instance Show a => Show (BiMap a) where
    show (BiMap _ m) =  "BiMap" ++ show (IM.toList m)

size :: BiMap a -> Int
size (BiMap _ m) = IM.size m

