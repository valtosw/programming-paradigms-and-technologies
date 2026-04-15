-- part 2 (II). 27
-- Розбити заданий список на кілька списків, записуючи у перший список значення, які менші за 2^0 , у другий
-- – які менші за 2^1 та не потрапили до попереднього списку, у третій – які менші за 2^2 та не потрапили до
-- двох попередніх списків, у четвертий – які менші за 2^3 та не потрапили до попередніх списків і т.д.
import Data.List (intercalate, groupBy, sortOn)
import Data.Function (on)

bucketIndex :: (Ord a, Num a) => a -> Int
bucketIndex x = go 0
  where
    go k
        | x < (2 ^ k) = k
        | otherwise   = go (k + 1)

splitByPowersOfTwo :: (Ord a, Num a) => [a] -> [[a]]
splitByPowersOfTwo xs =
    map (map snd) $
    groupBy ((==) `on` fst) $
    sortOn fst [(bucketIndex x, x) | x <- xs]

showArray :: Show a => [a] -> String
showArray arr = "[" ++ intercalate ", " (map show arr) ++ "]"

show2DArray :: Show a => [[a]] -> String
show2DArray arr = "[" ++ intercalate ", " (map showArray arr) ++ "]"

runTest :: (Ord a, Num a, Show a) => Int -> [a] -> [[a]] -> IO ()
runTest n input expected = do
    let result = splitByPowersOfTwo input
    putStrLn $ "Test " ++ show n ++ ":"
    putStrLn $ "  Input:    " ++ showArray input
    putStrLn $ "  Expected: " ++ show2DArray expected
    putStrLn $ "  Result:   " ++ show2DArray result
    putStrLn $ if result == expected then "  PASS\n" else "  FAIL\n"

main :: IO ()
main = do
    runTest 1 [0, 1, 2, 3, 4]
        [[0], [1], [2,3], [4]]

    runTest 2 [5, 1, 8, 2]
        [[1], [2], [5], [8]]

    runTest 3 [0.5, 1.5, 3.5, 7.5]
        [[0.5], [1.5], [3.5], [7.5]]

    runTest 4 [10, 3, 6, 1]
        [[1], [3], [6], [10]]