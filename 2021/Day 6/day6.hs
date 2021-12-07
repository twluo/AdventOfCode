import Data.List.Split
import Data.List

-- Helper Functions

processInput :: String -> [Int]
processInput input =
    let fishes = map (read::String -> Int) $ splitOn "," input
        padFishes = fishes ++ [0, 1, 2, 3, 4, 5, 6, 7 ,8]
    in
        (\x-> x - 1) <$> length <$> (group $ sort $ padFishes)

advanceFish :: [Int] -> [Int]
advanceFish fishes = pre6 ++ ([age6] ++ (drop 1 post6)) ++ [fishSpawns]
    where
        fishSpawns = head fishes
        (pre6, post6) = splitAt 6 (drop 1 fishes)
        age6 = head post6 + fishSpawns

advanceFishes :: [Int] -> Int -> Int -> [Int]
advanceFishes fishes currDay targetDay
    | currDay == targetDay = fishes
    | otherwise = advanceFishes (advanceFish $ fishes) (currDay + 1) targetDay

-- Part 1

solve1 :: [Int] -> Int
solve1 fishes = sum $ advanceFishes fishes 0 80

-- Part 2

solve2 :: [Int] -> Int
solve2 fishes = sum $ advanceFishes fishes 0 256

main = do
    input <- readFile "input.txt"
    let fishes = processInput input
    print $ solve1 fishes
    print $ solve2 fishes