{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE -XInstanceSigs #-}


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

data Point = Point [Integer] deriving Show

type Rectangle = [(Integer, Integer)] -- [(max, min)]

dimension :: Point -> Integer
dimension p@(Point coords) = length' coords

head' :: Point -> Integer
head' p@(Point coords) = head coords

point_index :: Point -> Integer -> Integer
point_index p@(Point coords) i = coords !! (fromInteger i)

aslist :: Point -> [Integer]
aslist p@(Point coords) = coords

instance Eq Point where
	(==) p1@(Point xs) p2@(Point ys) =
		(dimension p1 == dimension p2)
		&& 
		(and $ zipWith (==) xs ys)

list_plus :: (Num a) => [a] -> [a] -> [a]
list_plus = zipWith (+)

point_plus :: Point -> Point -> Point
point_plus p1@(Point xs) p2@(Point ys) = Point (xs `list_plus` ys)

same :: (Eq a) => a -> a -> Maybe a
same a b = if a == b
	then Just a
	else Nothing

comp_points :: (Integer -> Integer -> Bool) -> Point -> Point -> Maybe Bool
comp_points op p1@(Point xs) p2@(Point ys)
	| (dimension p1 == 0) = Nothing
	| (dimension p2 == 0) = Nothing
	| otherwise =
		let
			(x0, y0) = (head xs, head ys)
			c0 = x0 < y0
		in
			foldl (>>=) (return c0)
				. map same
					$ zipWith (op) xs ys

remove_maybe_level :: Maybe (Maybe a) -> Maybe a
remove_maybe_level (Nothing) = Nothing
remove_maybe_level (Just x) = x

comp_point_lists :: (Point -> Point -> (Maybe Bool)) -> [Point] -> [Point] -> Maybe Bool
comp_point_lists op xs ys
	| (length xs == 0) = Nothing
	| (length ys == 0) = Nothing
	| otherwise =
		let
			(x0, y0) = (head xs, head ys)
			c0 = (<<) x0 y0
		in
			remove_maybe_level
				. foldl (>>=) (return c0)
					. map same
						$ zipWith (op) xs ys

class (Eq a) => MaybeOrd a where
	(<<) :: a -> a -> Maybe Bool
	(>>) :: a -> a -> Maybe Bool

instance MaybeOrd Point where
	(<<) = comp_points (<)
	(>>) = comp_points (>)

instance MaybeOrd [Point] where
	(<<) = comp_point_lists (HRTree.<<)
	(>>) = comp_point_lists (HRTree.>>)

data HRTree a
	= Empty
	| HRTreeLeaf [a]
	| HRTreeInterior [HRTree a]
		deriving (Show, Eq)

class (Eq a) => (Euclidean a) where
	hvalue :: a -> Integer
	asrect :: a -> Rectangle
	inside :: a -> Point -> Bool
	center :: a -> Point
	e_mbr  :: [a] -> Rectangle

instance Euclidean Point where
	hvalue p = 142857 -- TODO
	asrect p = map (\(i, p) -> let x = p `point_index` i in (x, x)) . zipWith (\i l -> (i, l)) [0..] . replicate (2 ^ (dimension p)) $ p
	inside p p' = p == p'
	center = id
	e_mbr ps = [] -- TODO

pointDiv :: Point -> Integer -> Point
pointDiv (Point xs) denominator = Point (map (flip div denominator) xs)

instance Euclidean Rectangle where
	--hvalue :: Rectangle -> Integer
	hvalue list = hvalue . center $ list

	--asrect :: Rectangle -> Integer -> Rectangle
	asrect = id

	--inside :: Rectangle -> Point -> Bool
	inside list point = and $ zipWith f (aslist point) list
		where f coord (min_value, max_value) =
			(min_value < coord) && (coord < max_value)

	--center :: Rectangle -> Point
	--TODO: right?
	center list = Point (map pair_avg list)
		where pair_avg (min, max) = (min + max) `div` 2

	e_mbr rects = foldl1 update_best_so_far rects
		where
			--update_best_so_far :: Rectangle -> Rectangle -> Rectangle
			update_best_so_far a b = zipWith f a b
			f (min0, max0) (min1, max1) = (min min0 min1, max max0 max1)


class HRTreeNode a where
	mbr :: a -> Rectangle -- minimum bounding rectangle
	lhv :: a -> Integer -- largest Hilbert value (of children)
	get_children :: a -> [a]

instance (Euclidean a) => HRTreeNode (HRTree a) where
	mbr hrtree@Empty                     = []
	mbr hrtree@(HRTreeLeaf es)           = e_mbr es
	mbr hrtree@(HRTreeInterior children) = e_mbr . map mbr $ children

	lhv hrtree@Empty                     = error "Empty nodes don't have LHVs"
	lhv hrtree@(HRTreeLeaf es)           = maximum . map hvalue $ es
	lhv hrtree@(HRTreeInterior children) = maximum . map lhv $ children

	get_children hrtree@Empty                   = error "Can only get children from an interior node."
	get_children hrtree@(HRTreeLeaf es)         = error "Can only get children from an interior node."
	get_children hrtree@(HRTreeInterior children) = children

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

insert_rec :: (Euclidean a) => a -> HRTree a -> (HRTree a, Maybe (HRTree a))
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

--inside :: a -> Point -> Bool
overlaps :: Rectangle -> Rectangle -> Bool
overlaps a b = True--or $ map (inside a) (asrect b) -- TODO

elem :: (Euclidean a) => a -> HRTree a -> Bool
elem e hrtree@Empty = False
elem e hrtree@(HRTreeLeaf es) = e `List.elem` es
elem e hrtree@(HRTreeInterior children) =
	let
		possible_children = filter overlapping children
		overlapping child = overlaps (asrect e) (mbr child)
	in
		or . map (HRTree.elem e) $ possible_children

fromList :: (Euclidean a) => [a] -> HRTree a
fromList elems =
	foldl (flip HRTree.insert) HRTree.empty elems

count :: HRTree a -> Integer
count hrtree@Empty = 0
count hrtree@(HRTreeLeaf es) = length' es
count hrtree@(HRTreeInterior children) = sum . map HRTree.count $ children

