{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module HRTree
( HRTree(..)
, HRTree.elem
, HRTree.fromList
, HRTree.insert
, HRTree.empty
, HRTree.count
, HRTree.print
) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Bits as Bits

import HLib

import Geometry

data HRTree
	= Empty
	| HRTreeLeaf [Point]
	| HRTreeInterior [HRTree]
		deriving (Show, Eq)

{-instance MaybeOrd [Point] where
	(<<) = comp_point_lists (HRTree.<<)
	(>>) = comp_point_lists (HRTree.>>)
-}

class HasMBR a where
	mbr :: a -> Rectangle

class HasHvalue a where
	hvalue :: a -> Integer

instance HasMBR [Point] where
	mbr :: [Point] -> Rectangle
	mbr points = []

instance HasMBR [Rectangle] where
	mbr :: [Rectangle] -> Rectangle
	mbr rects = foldl1 update_best_so_far rects
		where
			--update_best_so_far :: Rectangle -> Rectangle -> Rectangle
			update_best_so_far a b = zipWith f a b
			f (min0, max0) (min1, max1) = (min min0 min1, max max0 max1)

instance HasMBR HRTree where
	mbr :: HRTree -> Rectangle
	mbr hrtree@Empty                     = []
	mbr hrtree@(HRTreeLeaf es)           = mbr es
	mbr hrtree@(HRTreeInterior children) = mbr . map mbr $ children

instance HasHvalue Point where
	hvalue :: Point -> Integer
	hvalue p@(Point coords) = (xy2d grid_size) . to_pair . (take 2) $ coords
		where
			-- n must be a power of 2, size of grid 
			xy2d :: Integer -> (Integer, Integer) -> Integer
			xy2d n (x, y) = xy2d' n (x, y) (n `div` 2)

			xy2d' :: Integer -> (Integer, Integer) -> Integer -> Integer
			xy2d' n (x, y) s
				| (s == 0) = 0
				| otherwise = d + rec
					where
						rx = if (x Bits..&. s) > 0 then 1 else 0
						ry = if (y Bits..&. s) > 0 then 1 else 0
						(x', y') = rot s (x, y) rx ry
						s' = (s `div` 2)

						d = (s^2) * ((3 * rx)^ry)
						rec = xy2d' n (x', y') s'

			-- Rotate or flip a quadrant appropriately
			rot :: Integer -> (Integer, Integer) -> Integer -> Integer -> (Integer, Integer)
			rot n (x, y) rx ry =
				if ry == 0
					then if rx == 1
						then (n - 1 - y, n - 1 - x)
						else (y, x)
					else (x, y)

			to_pair :: [a] -> (a, a)
			to_pair l = (l !! 0, l !! 1)

instance HasHvalue Rectangle where
	hvalue :: Rectangle -> Integer
	hvalue = hvalue . center

lhv :: HRTree -> Integer
lhv hrtree@Empty                     = error "Empty nodes don't have LHVs"
lhv hrtree@(HRTreeLeaf es)           = maximum . map hvalue $ es
lhv hrtree@(HRTreeInterior children) = maximum . map lhv $ children

get_elements :: HRTree -> [Point]
get_elements hrtree@Empty                   = error "Can only get elements from a leaf node."
get_elements hrtree@(HRTreeLeaf es)         = es
get_elements hrtree@(HRTreeInterior children) = error "Can only get elements from a leaf node."

get_children :: HRTree -> [HRTree]
get_children hrtree@Empty                   = error "Can only get children from an interior node."
get_children hrtree@(HRTreeLeaf es)         = error "Can only get children from an interior node."
get_children hrtree@(HRTreeInterior children) = children

pick_insertion_child :: Point -> HRTree -> HRTree
pick_insertion_child e node@Empty                     = error "Empty nodes have no children."
pick_insertion_child e node@(HRTreeLeaf es)           = error "Leaf nodes have no children."
pick_insertion_child e node@(HRTreeInterior children) =
	List.minimumBy (Ord.comparing lhv)
		. filter (\child -> (lhv child) < hvalue e)
			$ children

fromList :: [Point] -> HRTree
fromList elems =
	foldl (flip HRTree.insert) HRTree.empty elems

count :: HRTree -> Integer
count hrtree@Empty = 0
count hrtree@(HRTreeLeaf es) = length' es
count hrtree@(HRTreeInterior children) = sum . map HRTree.count $ children

empty :: HRTree
empty = Empty

print :: HRTree -> String
print = print_rec ""

print_rec :: String -> HRTree -> String
print_rec tabs hrtree@Empty = tabs  ++ "\n"

print_rec tabs hrtree@(HRTreeLeaf ps) =
	let
		tabs' = tabs ++ "\t"
		points_str = foldl1 (++) . map (f . show . aslist) $ ps
		f p = tabs' ++ p ++ "\n"
	in 
		tabs ++ "Leaf:\n" ++ points_str

print_rec tabs hrtree@(HRTreeInterior children) =
	let
		rec = foldl1 (++) . map (print_rec (tabs ++ "\t")) $ children
	in
		tabs ++ "Interior:\n" ++ rec ++ "\n"

elem :: Point -> HRTree -> Bool
elem e hrtree@Empty = False
elem e hrtree@(HRTreeLeaf es) = e `List.elem` es
elem e hrtree@(HRTreeInterior children) =
	let
		possible_children = filter overlaps children
		overlaps child = point_in_rect e (mbr child)
	in
		or . map (HRTree.elem e) $ possible_children

point_in_rect :: Point -> Rectangle -> Bool
point_in_rect p list = and $ zipWith f (aslist p) list
		where f coord (min_value, max_value) =
			(min_value < coord) && (coord < max_value)

insert :: Point -> HRTree -> HRTree
insert p hrtree =
	if (maximum . aslist $ p) >= grid_size -- < ?
		then error "Nope. Point has coordinate that's too big."
		else
			let
				(hrtree', split) = insert_rec p hrtree
				split' = from_just split
			in
				if split == Nothing
					then hrtree'
				else
					HRTreeInterior [hrtree', split']

insert_rec :: Point -> HRTree -> (HRTree, Maybe (HRTree))
insert_rec e node@Empty = (HRTreeLeaf [e], Nothing)

insert_rec e node@(HRTreeLeaf es) =
	if node_is_full node
		then (node, Just (HRTreeLeaf [e]))
		else (HRTreeLeaf (e : es), Nothing)

insert_rec e node@(HRTreeInterior children) =
	if split == Nothing
		then (node', Nothing)
		else if node_is_full node'
			then (node', Just (HRTreeInterior [split']))
			else (node'', Nothing)
	where
		insertion_child = pick_insertion_child e node

		children_minus_insertion_child = List.delete insertion_child children

		(insertion_child', split) = HRTree.insert_rec e insertion_child
		split' = from_just split

		node'  = HRTreeInterior (                         insertion_child' : children_minus_insertion_child) -- TODO: order by HV
		node'' = HRTreeInterior (redistribute split' $ (split' : insertion_child' : children_minus_insertion_child))

-- super-hacky usage of run-time type-getting, here.
redistribute :: HRTree -> [HRTree] -> [HRTree]
redistribute _type@(HRTreeLeaf _) nodes = map HRTreeLeaf children_of_children_redistributed -- or HRTreeLeaf?
	where
		children_of_children = concat . map get_elements $ nodes
		
		children_of_children_sorted = List.sortBy (Ord.comparing hvalue) children_of_children
		
		children_of_children_redistributed = partition partition_size children_of_children_sorted
			where partition_size = (length children_of_children) `div` (length nodes)

redistribute _type@(HRTreeInterior _) nodes = map HRTreeInterior children_of_children_redistributed -- or HRTreeLeaf?
	where
		children_of_children = concat . map get_children $ nodes
		
		children_of_children_sorted = List.sortBy (Ord.comparing lhv) children_of_children
		
		children_of_children_redistributed = partition partition_size children_of_children_sorted
			where partition_size = (length children_of_children) `div` (length nodes)

-- helpers

node_size :: HRTree -> Integer
node_size node@(Empty) = 0
node_size node@(HRTreeLeaf l) = length' l
node_size node@(HRTreeInterior l) = length' l

node_is_full :: HRTree -> Bool
node_is_full node = (node_size node) == max_node_size

max_node_size :: Integer
max_node_size = 5

center :: Rectangle -> Point
center list = Point (map pair_avg list)
	where pair_avg (min, max) = (min + max) `div` 2

grid_size :: Integer
grid_size = 2^32 -- maximum value of any part of coordinate
