module Main where

import Days.Day1
import qualified Days.Day2 as Day2
import qualified Days.Day3 as Day3
import qualified Days.Day4 as Day4
import qualified Days.Day5 as Day5

import System.IO (readFile)
import Data.List.Split (splitOn)

import qualified Data.List as List
import System.Environment (getArgs)

getData :: Show a => String -> (String -> a) -> Int -> IO [a]
getData separator parser day = do
    contents <- readFile ("data/day" ++ show day ++ ".txt")
    let datas = map parser (List.init $ splitOn separator contents)
    return datas

getLineData :: Show a => (String -> a) -> Int -> IO [a]
getLineData = getData "\n"

getLineInt :: Int -> IO [Int]
getLineInt = getLineData (read :: String -> Int)

getLineString :: Int -> IO [String]
getLineString = getLineData id


-- can probably be refactored somehow.
verifyDay :: Int -> Int -> IO Int
verifyDay 1 1 = do
    content <- getLineInt 1
    return $ day1_1 content
verifyDay 1 2 = do
    content <- getLineInt 1
    return $ day1_2 content
verifyDay 2 1 = do
    content <- getLineData Day2.parse 2
    return $ Day2.day2_1 content
verifyDay 2 2 = do
    content <- getLineData Day2.parse 2
    return $ Day2.day2_2 content
verifyDay 3 1 = do
    content <- getLineString 3
    return $ Day3.day3_1 content
verifyDay 3 2 = do
    content <- getLineString 3
    return $ Day3.day3_2 content
verifyDay 4 1 = do
    content <- getData "\n\n" id 4
    return $ Day4.day4_1 content
verifyDay 4 2 = do
    content <- getData "\n\n" id 4
    return $ Day4.day4_2 content
verifyDay 5 1 = do
    content <- getLineData Day5.parse 5
    return $ Day5.day5_1 content
verifyDay 5 2 = do
    content <- getLineData Day5.parse 5
    return $ Day5.day5_2 content
verifyDay _ _ = error "invalid day/part"

main :: IO ()
main = do
    args <- getArgs
    case args of
      [day, part] -> do
        answer <- verifyDay (read day) (read part)
        print answer
      _ -> error "invalid args"
    return ()

