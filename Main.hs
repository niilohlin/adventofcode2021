import Days.Day1

import System.IO (readFile)
import Data.List.Split (splitOn)

import qualified Data.List as List
import Debug.Trace

-- Read the day's data and parse it into lists separated by 'separator'
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
getLineString = getLineData (read :: String -> String)

verifyDay :: Int -> Int -> IO Int
verifyDay 1 1 = do
    content <- getLineInt 1
    let solution = day1_1 content
    print $ day1_1 content
    return solution
verifyDay 1 2 = do
    content <- getLineInt 1
    let solution = day1_2 content
    print $ day1_2 content
    return solution
verifyDay _ _ = undefined

main :: IO ()
main = do
    _ <- verifyDay 1 2
    return ()

