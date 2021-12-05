{-# LANGUAGE TupleSections #-}

module Days.Day5 (day5_1, day5_2, parse) where

import Linear.V2
import Linear (normalize)
import Data.List.Split (splitOn)
import Control.Lens
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

type Line = (V2 Int, V2 Int)

parse :: String -> Line
parse str =
    let [from, to] = splitOn " -> " str
        [x1, y1] = splitOn "," from
        [x2, y2] = splitOn "," to
     in (V2 (read x1) (read y1), V2 (read x2) (read y2))


lineSpan :: Line -> Map.Map (V2 Int) Int
lineSpan (v1, v2) = Map.fromList $ map (,1) (lineSpan' (v1, v2))
    where
        intNormalize = fmap round . normalize . fmap (fromIntegral :: Int -> Float)
        step = intNormalize $ v1 - v2
        lineSpan' (v1, v2) = if v1 == v2
                                then [v1]
                                else v1:lineSpan' (v1 - step, v2)

isStraight :: Line -> Bool
isStraight (V2 x1 y1, V2 x2 y2) = x1 == x2 || y1 == y2

isDiagonal :: Line -> Bool
isDiagonal (v1, v2) = slope `isCloseto` 1.0
    where
        slope = abs . tan . Linear.V2.unangle $ fmap fromIntegral (v1 - v2)
        isCloseto a b = abs (a - b) <= 0.00001



countOverlaps :: [Line] -> Int
countOverlaps lines = countIntersections $ mergeMaps pointSets
    where
        pointSets = fmap lineSpan lines
        mergeMaps :: [Map.Map (V2 Int) Int] -> Map.Map (V2 Int) Int
        mergeMaps = foldl (Map.unionWith (+)) Map.empty
        countIntersections = length . Map.filter (1<)

day5_1 :: [Line] -> Int
day5_1 lines = countOverlaps $ filter isStraight lines

day5_2 :: [Line] -> Int
day5_2 lines = countOverlaps $ filter (\l -> isStraight l || isDiagonal l) lines
