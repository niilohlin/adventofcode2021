
module Shared ((...), countWhere) where

import Data.List (filter)

(...) = (.) . (.)

countWhere :: (a -> Bool) -> [a] -> Int
countWhere = length ... filter
