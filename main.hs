{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import System.IO
import System.CPUTime

import Text.Printf

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord as Ord

import HRtree

type Point = [Integer]

type Hull = [Point]

class Euclidean a where
	hvalue :: a -> Integer

instance Euclidean Point where
	hvalue = sum

build_hrtree :: String -> HRtree Point
build_hrtree contents =
	let
		points = (map read . lines $ contents) :: [Point]
	in
		HRtree.fromList points

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

time_query :: HRtree Point -> String -> String
time_query hrtree query =
	let
		query' = (read query) :: Point
	in
		if (HRtree.elem query' hrtree) then "YES\n" else "NO\n"
{-	let
		query' = (read query) :: Point
		result = (show query) ++
			if (HRtree.elem query' Hrtree) then "YES\n" else "NO\n"
	in
		time (result `seq` return ()) `seq` result
-}

main = do
	contents <- readFile "rects.hs"
	let hrtree = build_hrtree contents
	interact (time_query hrtree)
