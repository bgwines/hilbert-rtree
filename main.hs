import System.IO
import System.CPUTime

import Text.Printf

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Ord as Ord

import HRtree

type Point = [Integer]

type Hull = [Point]

build_hrtree :: String -> HRtree Hull
build_hrtree contents =
	let
		hulls = (map read . lines $ contents) :: [Hull]
	in
		HRtree.fromList hulls

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

time_query :: HRtree Hull -> String -> String
time_query hrtree query =
	let
		query' = (read query) :: Hull
	in
		if (HRtree.elem query' hrtree) then "YES\n" else "NO\n"
{-	let
		query' = (read query) :: Hull
		result = (show query) ++
			if (HRtree.elem query' Hrtree) then "YES\n" else "NO\n"
	in
		time (result `seq` return ()) `seq` result
-}

main = do
	contents <- readFile "rects.hs"
	let hrtree = build_hrtree contents
	interact (time_query hrtree)
