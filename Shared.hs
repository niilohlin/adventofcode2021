

module Shared (getData
            , getLineData
            , getLineInt
            , getLineString
) where

import System.IO (readFile)
import Data.List.Split (splitOn)

import qualified Data.List as List


-- Read the day's data and parse it into lists separated by 'separator'
getData :: String -> (String -> a) -> Int -> IO ([a])
getData separator parser day = do
    contents <- readFile ("data/day" ++ show day ++ ".txt")
    return $ map parser (List.init $ splitOn separator contents)

getLineData :: (String -> a) -> Int -> IO ([a])
getLineData = getData "\n"

getLineInt :: Int -> IO ([Int])
getLineInt = getLineData (read :: String -> Int)

getLineString :: Int -> IO ([String])
getLineString = getLineData (read :: String -> String)




