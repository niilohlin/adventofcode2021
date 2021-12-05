{-# LANGUAGE DeriveFunctor, DeriveTraversable, TupleSections #-}

module Days.Day4 (day4_1, day4_2, parseBoard, hasBingo, callNumber, parseCalls, runBingo, startBingo, createBingo, sumUnmarked) where

import Data.List.Split (splitOn)
import Data.List (transpose, find)
import Data.String (words)

newtype Bingo a = Bingo [[a]] deriving (Show, Eq, Functor, Foldable, Traversable)

createBingo :: [[a]] -> Bingo a
createBingo = Bingo

parseBoard :: String -> Bingo Int
parseBoard str = Bingo $ map (map read . words) $ splitOn "\n" str

hasBingo :: Bingo (Int, Bool) -> Bool
hasBingo (Bingo lst) = hasRowBingo lst
                    || hasRowBingo (transpose lst)
    where
        hasRowBingo lst = any (all snd) lst

callNumber :: Int -> Bingo (Int, Bool) -> Bingo (Int, Bool)
callNumber num = fmap (\(i, called) -> if num == i then (i, True)
                                                  else (i, called))

parseCalls :: String -> [Int]
parseCalls = map read . splitOn ","

runBingo :: [Int] -> [Bingo (Int, Bool)] -> Int
runBingo (call:calls) boards = let newBoards = map (callNumber call) boards
                                in case find hasBingo newBoards of
                                     (Just board) -> call * sumUnmarked board
                                     Nothing -> runBingo calls newBoards
runBingo _ _ = error "no more calls"

startBingo :: Bingo Int -> Bingo (Int, Bool)
startBingo = fmap (, False)

sumUnmarked :: Bingo (Int, Bool) -> Int
sumUnmarked = foldl (\s (num, called) -> if called then s else s + num) 0

day4_1 :: [String] -> Int
day4_1 (calls:boards) = let (calls', boards') = (parseCalls calls, map parseBoard boards)
                         in runBingo calls' (map startBingo boards')
day4_1 _ = error "invalid input"

findLastWinner :: [Int] -> [Bingo (Int, Bool)] -> Int
findLastWinner (call:calls) boards = let newBoards = map (callNumber call) boards
                                in case (find (not . hasBingo) boards, all hasBingo newBoards) of
                                     (_, False) -> findLastWinner calls newBoards
                                     (Just lastBoard, True) -> call * sumUnmarked (callNumber call lastBoard)
                                     (Nothing, True) -> error "should not happen"
findLastWinner _ _ = error "no more calls"

day4_2 :: [String] -> Int
day4_2 (calls:boards) = let (calls', boards') = (parseCalls calls, map parseBoard boards)
                         in findLastWinner calls' (map startBingo boards')
day4_2 _ = error "invalid input"
