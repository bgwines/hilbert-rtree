
module HRtree
( HRtree(..)
, HRtree.elem
, HRtree.fromList
, HRtree.insert
, HRtree.empty
, HRtree.count
) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord as Ord

-- TODO: MBR, LHV

data HRtree a
	= Empty
	| HRtreeLeaf [a]           -- TODO: store MBR, LHV here
	| HRtreeInterior [HRtree a] -- TODO: store MBR, LHV here
		deriving (Show, Eq)

length' :: [a] -> Integer
length' = toInteger . length

from_just :: Maybe a -> a
from_just (Just x) = x

node_size :: HRtree a -> Integer
node_size node@(Empty) = 0
node_size node@(HRtreeLeaf l) = length' l
node_size node@(HRtreeInterior l) = length' l

node_is_full :: HRtree a -> Bool
node_is_full node = (node_size node) == m*2

m :: Integer -- TODO: take in m in ctor?
m = 5

empty :: HRtree a
empty = Empty

hvalue :: a -> Integer
hvalue e = 142857 -- TODO: hvalue typeclass, implement for point, hull

takeWhileAndRest :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileAndRest f [] = ([], [])
takeWhileAndRest f l@(x:xs) = if not (f x)
	then ([], l)
	else (x:(fst rec), snd rec)
		where rec = takeWhileAndRest f xs

insert :: (Ord a) => a -> HRtree a -> HRtree a
insert e hrtree =
	let
		(hrtree', split) = insert_rec e hrtree
		split' = from_just split
	in
		if split == Nothing
			then hrtree'
		else
			HRtreeInterior [hrtree', split']

insert_rec :: (Ord a) => a -> HRtree a -> (HRtree a, Maybe (HRtree a))
insert_rec e node@Empty = (HRtreeLeaf [e], Nothing)

insert_rec e node@(HRtreeLeaf es) =
	if node_is_full node
		then (HRtreeLeaf es      , Just (HRtreeLeaf [e])) -- TODO: split properly
		else (HRtreeLeaf (e : es), Nothing)

insert_rec e node@(HRtreeInterior children) =
	if split == Nothing
		then (node', Nothing)
		else if node_is_full node'
			then (node', Just (HRtreeInterior [split']))
			else (node'', Nothing)
	where
		insertion_child = head children -- TODO: Hilbert heuristic

		children_minus_insertion_child = List.delete insertion_child children

		(insertion_child', split) = HRtree.insert_rec e insertion_child
		split' = from_just split

		node' = HRtreeInterior (insertion_child' : children_minus_insertion_child) -- TODO: order by HV
		node'' = HRtreeInterior (split' : insertion_child' : children_minus_insertion_child)

elem :: (Ord a) => a -> HRtree a -> Bool
elem e hrtree@Empty = False
elem e hrtree@(HRtreeLeaf es) = e `List.elem` es
elem e hrtree@(HRtreeInterior children) =
	let
		possible_children = children -- TODO: overlapping calculation
	in
		or . map (HRtree.elem e) $ possible_children

fromList :: (Ord a) => [a] -> HRtree a
fromList elems =
	foldl (flip HRtree.insert) HRtree.empty elems

count :: HRtree a -> Integer
count hrtree@Empty = 0
count hrtree@(HRtreeLeaf es) = length' es
count hrtree@(HRtreeInterior children) = sum . map HRtree.count $ children

