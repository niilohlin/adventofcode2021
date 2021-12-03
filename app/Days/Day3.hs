
module Days.Day3 (day3_1, day3_2) where

import Data.List (transpose, nub, sort, group, maximumBy, foldl')
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Char (digitToInt)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

groupByElemCount :: Ord a => [a] -> [(Int, a)]
groupByElemCount = map (length &&& head) . group . sort

leastCommon :: String -> Char
leastCommon = snd . minimum . groupByElemCount

mostCommon :: String -> Char
mostCommon = snd . maximum . groupByElemCount

bitFlip ('1':xs) = '0' : bitFlip xs
bitFlip ('0':xs) = '1' : bitFlip xs
bitFlip _ = ""

day3_1 :: [String] -> Int
day3_1 lst = gamma * epsilon
    where gammaStr = map mostCommon $ transpose lst
          gamma = toDec gammaStr
          epsilon = toDec $ bitFlip gammaStr


day3_2 :: [String] -> Int
day3_2 lst = toDec (go mostCommon lst) * toDec (go leastCommon lst)
    where
        go :: (String -> Char) -> [String] -> String
        go _ [] = []
        go _ [""] = []
        go findPivot lst = pivot: go findPivot filteredList
            where
                pivot = head $ map findPivot $ transpose lst
                filteredList = map tail $ filter ((pivot==) . head) lst
