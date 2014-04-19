module Geometry
( Point(..)
, aslist
, Rectangle(..)
) where

import HLib
import qualified Data.Bits as Bits

data Point = Point [Integer] deriving (Show, Read)

type Rectangle = [(Integer, Integer)] -- [(max, min)]

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

point_index :: Point -> Integer -> Integer
point_index p@(Point coords) i = coords !! (fromInteger i)

aslist :: Point -> [Integer]
aslist p@(Point coords) = coords

list_plus :: (Num a) => [a] -> [a] -> [a]
list_plus = zipWith (+)

point_plus :: Point -> Point -> Point
point_plus p1@(Point xs) p2@(Point ys) = Point (xs `list_plus` ys)

dimension :: Point -> Integer
dimension p@(Point coords) = length' coords

head' :: Point -> Integer
head' p@(Point coords) = head coords

instance Eq Point where
	(==) p1@(Point xs) p2@(Point ys) =
		(dimension p1 == dimension p2)
		&& 
		(and $ zipWith (==) xs ys)

class (Eq a) => MaybeOrd a where
	(<<) :: a -> a -> Maybe Bool
	(>>) :: a -> a -> Maybe Bool

instance MaybeOrd Point where
	(<<) = comp_points (<)
	(>>) = comp_points (>)

point_div :: Point -> Integer -> Point
point_div (Point xs) denominator = Point (map (flip div denominator) xs)

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