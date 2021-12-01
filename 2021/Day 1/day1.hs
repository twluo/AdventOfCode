import Data.List.Split

countIncreasingDepths :: [Int] -> Int
countIncreasingDepths [] = 0
countIncreasingDepths x = increasingCount where
    depthPairs = zip x (tail x)
    depthDiff = map (\(x, y) -> x - y) depthPairs
    justNeg = filter (<0) depthDiff
    increasingCount = length justNeg

main = do
    input <- readFile "input.txt"
    let depths = map (read::String->Int) (splitOn "\r\n" input)

    -- Part 1
    print (countIncreasingDepths depths)

    -- Part 2
    let depthTriplets = zip3 depths (tail depths) (tail (tail depths))
        depthTripletsSum = map (\(x, y, z) -> x + y + z) depthTriplets
    print (countIncreasingDepths depthTripletsSum)