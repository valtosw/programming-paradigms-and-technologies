-- part 1 (I-a). 19
-- Залишити у списку елементи, що входять у нього по одному разу.
import Data.List (intercalate)

unique :: Eq a => [a] -> [a]
unique arr = filter (\x -> count x arr == 1) arr
  where count x = length . filter (== x)

parseInput :: Read a => String -> [a]
parseInput = map read . words

showArray :: Show a => [a] -> String
showArray arr = "[" ++ intercalate ", " (map show arr) ++ "]"

runTest :: (Eq a, Show a) => Int -> [a] -> [a] -> IO ()
runTest n input expected = do
    let result = unique input
    putStrLn $ "Test " ++ show n ++ ":"
    putStrLn $ "  Input:    " ++ showArray input
    putStrLn $ "  Expected: " ++ showArray expected
    putStrLn $ "  Result:   " ++ showArray result
    putStrLn $ if result == expected then "  PASS\n" else "  FAIL\n"

main :: IO ()
main = do
    putStrLn "Choose input type:"
    putStrLn "1 - Int"
    putStrLn "2 - Double"
    putStrLn "3 - String"
    choice <- getLine

    putStrLn "Enter elements separated by spaces:"

    case choice of
        "1" -> do
            line <- getLine
            let input = parseInput line :: [Int]
            putStrLn $ "Result: " ++ showArray (unique input)

        "2" -> do
            line <- getLine
            let input = parseInput line :: [Double]
            putStrLn $ "Result: " ++ showArray (unique input)

        "3" -> do
            line <- getLine
            let input = words line
            putStrLn $ "Result: " ++ showArray (unique input)

        _ -> putStrLn "Invalid choice"

    putStrLn "\n"

    runTest 1 [1,2,3,2,4,5,1] ([3,4,5] :: [Int])
    runTest 2 ["a","b","a","c"] ["b","c"]
    runTest 3 [1.1,2.2,1.1,3.3] ([2.2,3.3] :: [Double])
    runTest 4 [(1,2),(2,3),(1,2)] [(2,3)]
