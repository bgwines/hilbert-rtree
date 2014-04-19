{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import System.IO
import System.CPUTime

import Text.Printf

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Bits as Bits

import HLib
import HRTree
import Geometry

format_input :: String -> [Point]
format_input contents = map Point coords
	where
		coords = map (map read . words) . lines $ contents :: [[Integer]]

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

time_query :: HRTree -> String -> String
time_query hrtree query =
	let
		query' = (read query) :: Point
	in
		if (HRTree.elem query' hrtree) then "YES\n" else "NO\n"

main = do
	contents <- readFile "rects.txt"
	let hrtree = HRTree.fromList $ format_input contents
	interact (time_query hrtree)
