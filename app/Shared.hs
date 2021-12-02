
module Shared ((...), countWhere, capitalize) where

import Data.List (filter)
import qualified Data.Char as Char

(...) = (.) . (.)

countWhere :: (a -> Bool) -> [a] -> Int
countWhere = length ... filter

capitalize :: String -> String
capitalize (head:tail) = Char.toUpper head : map Char.toLower tail
capitalize [] = []
