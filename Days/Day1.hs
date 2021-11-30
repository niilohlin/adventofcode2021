

module Days.Day1 (day1_1) where

import Data.Set

day1_1 :: [Int] -> Int
day1_1 numbers =
    let nums = Data.Set.fromList numbers in
    head [x * y | x <- numbers, y <- toList $ nums `Data.Set.intersection` (Data.Set.fromList [2020 - x]), x /= y]

day1_2 :: [Int] -> Int
day1_2 numbers =
    let nums = Data.Set.fromList numbers in
    head [ x * y | x <- numbers
         , y <- toList $ nums `Data.Set.intersection` (Data.Set.fromList [2020 - x])
         , x /= y ]

