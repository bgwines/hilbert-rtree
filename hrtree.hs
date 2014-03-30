--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE FlexibleInstances #-}

module HRTree
( HRTree(..)
, HRTree.elem
, HRTree.fromList
, HRTree.insert
, HRTree.empty
, HRTree.count
) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord as Ord

-- TODO: MBR, LHV

type Point = [Integer]
type Rectangle = [Point]

data HRTree a
	= Empty
	| HRTreeLeaf [a]            -- TODO: store MBR, LHV here
	| HRTreeInterior [HRTree a] -- TODO: store MBR, LHV here
		deriving (Show, Eq)

class (Ord a) => (Euclidean a) where
	hvalue :: a -> Integer

{-class HRTreeNode a where
	mbr :: a -> [Point] -- minimum bounding rectangle
	lhv :: a -> Integer -- largest Hilbert value (of children)

instance HRTreeNode ((Euclidean a) => HRTree a) where
	mbr rtree@Empty                     = []
	mbr rtree@(HRTreeLeaf es)           = []
	mbr rtree@(HRTreeInterior children) = []

	lhv rtree@Empty                     = 0
	lhv rtree@(HRTreeLeaf es)           = maximum . map hvalue $ es
	lhv rtree@(HRTreeInterior children) = maximum . map lhv $ children
-}
length' :: [a] -> Integer
length' = toInteger . length

from_just :: Maybe a -> a
from_just (Just x) = x

node_size :: HRTree a -> Integer
node_size node@(Empty) = 0
node_size node@(HRTreeLeaf l) = length' l
node_size node@(HRTreeInterior l) = length' l

node_is_full :: HRTree a -> Bool
node_is_full node = (node_size node) == max_node_size

max_node_size :: Integer -- TODO: take in m in ctor?
max_node_size = 5

empty :: HRTree a
empty = Empty

partition :: Int -> [a] -> [[a]]
partition len l =
	if (length l) <= len
		then [l]
		else (take len l) : (partition len (drop len l))

takeWhileAndRest :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileAndRest f [] = ([], [])
takeWhileAndRest f l@(x:xs) = if not (f x)
	then ([], l)
	else (x:(fst rec), snd rec)
		where rec = takeWhileAndRest f xs

lhv :: (Euclidean a) => HRTree a -> Integer
lhv node@Empty                     = error "Empty nodes don't have LHVs"
lhv node@(HRTreeLeaf es)           = maximum . map hvalue $ es
lhv node@(HRTreeInterior children) = maximum . map lhv $ children

pick_insertion_child :: (Euclidean a) => a -> HRTree a -> HRTree a
pick_insertion_child e node@Empty                     = error "Empty nodes have no children."
pick_insertion_child e node@(HRTreeLeaf es)           = error "Leaf nodes have no children."
pick_insertion_child e node@(HRTreeInterior children) =
	List.minimumBy (Ord.comparing lhv)
		. filter (\child -> (lhv child) < hvalue e)
			$ children

insert :: (Euclidean a) => a -> HRTree a -> HRTree a
insert e hrtree =
	let
		(hrtree', split) = insert_rec e hrtree
		split' = from_just split
	in
		if split == Nothing
			then hrtree'
		else
			HRTreeInterior [hrtree', split']

get_children :: HRTree a -> [HRTree a]
get_children node@(HRTreeInterior children) = children
get_children _ = error "Can only get children from an interior node."

insert_rec :: (Euclidean a) => a -> HRTree a -> (HRTree a, Maybe (HRTree a))
insert_rec e node@Empty = (HRTreeLeaf [e], Nothing)

insert_rec e node@(HRTreeLeaf es) =
	if node_is_full node
		then (node, Just (HRTreeLeaf [e])) -- TODO: split properly
		else (HRTreeLeaf (e : es), Nothing)

insert_rec e node@(HRTreeInterior children) =
	if split == Nothing
		then (node', Nothing)
		else if node_is_full node'
			then (node', Just (HRTreeInterior [split'])) -- TODO: split properly
			else (node'', Nothing)
	where
		insertion_child = pick_insertion_child e node

		children_minus_insertion_child = List.delete insertion_child children

		(insertion_child', split) = HRTree.insert_rec e insertion_child
		split' = from_just split

		node' = HRTreeInterior (insertion_child' : children_minus_insertion_child) -- TODO: order by HV
		node'' = HRTreeInterior (redistribute $ (split' : insertion_child' : children_minus_insertion_child))

		redistribute :: (Euclidean a) => [HRTree a] -> [HRTree a]
		redistribute nodes = map HRTreeInterior children_of_children' -- or HRTreeLeaf?
			where
				children_of_children = concat . map get_children $ nodes
				children_of_children' =
					partition partition_size
						. List.sortBy (Ord.comparing lhv)
							$ children_of_children

				partition_size =
					(length children_of_children) `div` (length nodes)

overlaps :: (Euclidean a) => a -> HRTree a -> Bool
overlaps e node = True -- TODO

elem :: (Euclidean a) => a -> HRTree a -> Bool
elem e hrtree@Empty = False
elem e hrtree@(HRTreeLeaf es) = e `List.elem` es
elem e hrtree@(HRTreeInterior children) =
	let
		possible_children = filter (overlaps e) children
	in
		or . map (HRTree.elem e) $ possible_children

fromList :: (Euclidean a) => [a] -> HRTree a
fromList elems =
	foldl (flip HRTree.insert) HRTree.empty elems

count :: HRTree a -> Integer
count hrtree@Empty = 0
count hrtree@(HRTreeLeaf es) = length' es
count hrtree@(HRTreeInterior children) = sum . map HRTree.count $ children

