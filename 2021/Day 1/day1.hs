import Data.List.Split

countIncreasingDepths :: [Int] -> Int
countIncreasingDepths [] = 0
countIncreasingDepths x = increasingCount where
    depthPairs = zip x (tail x)
    depthDiff = map (\(x, y) -> x - y) depthPairs
    justNeg = filter (<0) depthDiff
    increasingCount = length justNeg

-- Part 1
solve1 :: [Int] -> Int
solve1 input = countIncreasingDepths input

-- Part 2
solve2:: [Int] -> Int
solve2 input = sol where
    depthTriplets = zip3 input (tail input) (tail (tail input))
    depthTripletsSum = map (\(x, y, z) -> x + y + z) depthTriplets
    sol = countIncreasingDepths depthTripletsSum

main = do
    input <- readFile "input.txt"
    let depths = map (read::String->Int) (splitOn "\r\n" input)
    print $ solve1 depths
    print $ solve2 depths