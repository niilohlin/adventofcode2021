
module Days.Day2 (day2_1, day2_2, parse) where
import Shared (capitalize)
import Debug.Trace

data Instruction = Forward Int | Up Int | Down Int deriving (Read, Show, Eq)

parse :: String -> Instruction
parse = read . capitalize

day2_1 :: [Instruction] -> Int
day2_1 = uncurry (*) . foldl go (0, 0)
    where go (x, depth) (Forward val) = (x + val, depth)
          go (x, depth) (Up val) = (x, depth - val)
          go (x, depth) (Down val) = (x, depth + val)

day2_2 :: [Instruction] -> Int
day2_2 = uncurry (*) . dropTuple . foldl go (0, 0, 0)
    where go (x, depth, aim) (Forward val) = (x + val, depth + val * aim, aim)
          go (x, depth, aim) (Up val) = (x, depth , aim - val)
          go (x, depth, aim) (Down val) = (x, depth, aim + val)

          dropTuple (a, b, _) = (a, b)
