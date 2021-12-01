

module Days.Day1 (day1_1, day1_2) where

import Data.Maybe (fromMaybe)
import Shared (countWhere)

day1_1 :: [Int] -> Int
day1_1 lst = countWhere (uncurry (<)) $ zip lst (tail lst)


day1_2 :: [Int] -> Int
day1_2 lst = day1_1 $ slidingSum lst
    where
        slidingSum (x:y:z:lst) = (x + y + z):slidingSum (y:z:lst)
        slidingSum _ = []








