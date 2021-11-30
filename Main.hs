import Shared
import Days.Day1


verifyDay :: Int -> Int -> IO (Int)
verifyDay 1 1 = do
    content <- getLineInt 1
    let solution = day1_1 content
    putStrLn $ show $ day1_1 content
    return $ [solution]
verifyDay 1 2 = do
    content <- getLineInt 1
    let solution = day1_2 content
    putStrLn $ show $ day1_2 content
    return $ [solution]
verifyDay _ _ = undefined

main :: IO ()
main = do
    _ <- verifyDay 1 1
    return ()

