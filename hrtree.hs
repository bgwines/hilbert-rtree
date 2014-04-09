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
import qualified Data.Bits as Bits

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

point_div :: Point -> Integer -> Point
point_div (Point xs) denominator = Point (map (flip div denominator) xs)

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
rects_overlap :: Rectangle -> Rectangle -> Bool
rects_overlap a b = True--or $ map (inside a) (asrect b) -- TODO

elem :: (Euclidean a) => a -> HRTree a -> Bool
elem e hrtree@Empty = False
elem e hrtree@(HRTreeLeaf es) = e `List.elem` es
elem e hrtree@(HRTreeInterior children) =
	let
		possible_children = filter overlapping children
		overlapping child = rects_overlap (asrect e) (mbr child)
	in
		or . map (HRTree.elem e) $ possible_children

fromList :: (Euclidean a) => [a] -> HRTree a
fromList elems =
	foldl (flip HRTree.insert) HRTree.empty elems

count :: HRTree a -> Integer
count hrtree@Empty = 0
count hrtree@(HRTreeLeaf es) = length' es
count hrtree@(HRTreeInterior children) = sum . map HRTree.count $ children

empty :: HRTree a
empty = Empty

-------------------------------
--           Tests           --
-------------------------------


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

-------------------------------
--           Tests           --
-------------------------------

pair :: a -> b -> (a, b)
pair a b = (a, b)

map_keep :: (a -> b) -> [a] -> [(a, b)]
map_keep f l = zipWith pair l (map f l)

pt_arr :: [Integer]
pt_arr = [0..4]

test_pt :: Point
test_pt = Point pt_arr

pt_empty :: Point
pt_empty = Point []

test_leaf_node1 :: HRTree Point
test_leaf_node1 = HRTreeLeaf [test_pt, test_pt]

test_leaf_node2 :: HRTree Point
test_leaf_node2 = HRTreeLeaf [test_pt, test_pt, test_pt]

test_interior_node :: HRTree Point
test_interior_node = HRTreeInterior [test_leaf_node1, test_leaf_node2]

-- dimension :: Point -> Integer
test_dimension =
	((dimension test_pt) == 5) &&
	((dimension pt_empty) == 0)

-- head' :: Point -> Integer
test_head =
	((head' test_pt) == head pt_arr)

-- point_index :: Point -> Integer -> Integer
test_point_index =
	(or . map (\(e, i) -> (e == i)) . map_keep (point_index test_pt) $ pt_arr)

-- aslist :: Point -> [Integer]
test_aslist =
	(aslist test_pt) == pt_arr

-- list_plus :: (Num a) => [a] -> [a] -> [a]
test_list_plus =
	([1..4] `list_plus` [5..8] == [6,8,10,12])

-- point_plus :: Point -> Point -> Point
test_point_plus =
	((Point [1..4]) `point_plus` (Point [5..8]) == Point [6,8,10,12])

-- comp_points :: (Integer -> Integer -> Bool) -> Point -> Point -> Maybe Bool
test_comp_points =
	((Point [1..4] HRTree.<< Point [5..8]) == Just True) &&
	((Point [1..4] HRTree.>> Point [5..8]) == Nothing) &&
	
	((Point [1..4] HRTree.<< Point [2..5]) == Just True) &&
	((Point [1..4] HRTree.>> Point [2..5]) == Nothing) &&
	
	((Point [1..4] HRTree.<< Point [4..1]) == Nothing) &&
	((Point [1..4] HRTree.>> Point [4..1]) == Nothing) &&
	
	((Point [1..4] HRTree.<< Point [2,3,4,4]) == Nothing)

-- comp_point_lists :: (Point -> Point -> (Maybe Bool)) -> [Point] -> [Point] -> Maybe Bool
test_comp_point_lists =
	(([Point [1..4]] HRTree.<< [Point [5..8]]) == Just True) &&
	(([Point [1..4]] HRTree.>> [Point [5..8]]) == Nothing) &&

	(([Point [1..4]] HRTree.<< [Point [2..5]]) == Just True) &&
	(([Point [1..4]] HRTree.>> [Point [2..5]]) == Nothing) &&

	(([Point [1..4]] HRTree.<< [Point [4..1]]) == Nothing) &&
	(([Point [1..4]] HRTree.>> [Point [4..1]]) == Nothing)

-- point_div :: Point -> Integer -> Point
test_point_div =
	(test_pt `point_div` 1 == test_pt)

-- node_size :: HRTree a -> Integer
test_node_size =
	((node_size test_interior_node) == 2) &&
	((node_size test_leaf_node1) == 2) &&
	((node_size test_leaf_node2) == 3)

-- partition :: Int -> [a] -> [[a]]
test_partition =
	((partition 3 [1..10]) == [[1,2,3], [4,5,6], [7,8,9], [10]])

-- takeWhileAndRest :: (a -> Bool) -> [a] -> ([a], [a])
test_takeWhileAndRest =
	((takeWhileAndRest (<3) [1..10]) == ([1,2],[3,4,5,6,7,8,9,10]))

-- pick_insertion_child :: (Euclidean a) => a -> HRTree a -> HRTree a
test_pick_insertion_child = False

-- insert :: (Euclidean a) => a -> HRTree a -> HRTree a
test_insert = False

-- insert_rec :: (Euclidean a) => a -> HRTree a -> (HRTree a, Maybe (HRTree a))
test_insert_rec = False

-- rects_overlap :: Rectangle -> Rectangle -> Bool
test_rects_overlap = False

-- elem :: (Euclidean a) => a -> HRTree a -> Bool
test_elem = False

-- fromList :: (Euclidean a) => [a] -> HRTree a
test_fromList = False

-- count :: HRTree a -> Integer
test_count = False

-- empty :: HRTree a
test_empty = False

tests :: [Bool]
tests = [test_dimension,
	test_head,
	test_point_index,
	test_aslist,
	test_list_plus,
	test_point_plus,
	test_comp_points,
	test_comp_point_lists,
	test_point_div,
	test_node_size,
	test_partition,
	test_takeWhileAndRest,
	test_pick_insertion_child,
	test_insert,
	test_insert_rec,
	test_rects_overlap,
	test_elem,
	test_fromList,
	test_count,
	test_empty]

test_names :: [String]
test_names = ["test_dimension",
	"test_head",
	"test_point_index",
	"test_aslist",
	"test_list_plus",
	"test_point_plus",
	"test_comp_points",
	"test_comp_point_lists",
	"test_point_div",
	"test_node_size",
	"test_partition",
	"test_takeWhileAndRest",
	"test_pick_insertion_child",
	"test_insert",
	"test_insert_rec",
	"test_rects_overlap",
	"test_elem",
	"test_fromList",
	"test_count",
	"test_empty"]

run_tests :: IO ()
run_tests = putStrLn $ foldl1 (++) $ zipWith print_result tests test_names
	where print_result test name =
		if test
			then "succeeded: " ++ name ++ "\n"
			else "FAILED   : " ++ name ++ "\n"

