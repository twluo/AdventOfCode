import Data.List.Split
import Data.List

type Crabs = [Int]
type Pos = Int
type Fuel = Int

-- Helper Functions

processInput :: String -> Crabs
processInput input = map (read::String -> Int) $ splitOn "," input

-- Part 1

getFuelCost :: Crabs -> Pos -> Fuel
getFuelCost crabs dest = sum $ (\x -> abs $ dest - x) <$> crabs

solve1 :: Crabs -> Int
solve1 crabs = minimum $ getFuelCost crabs <$> [(minimum crabs)..(maximum crabs)]

-- Part 2

getFuelCost2 :: Crabs -> Pos -> Fuel
getFuelCost2 crabs dest = sum $ getIndividualFuelCost <$> crabs 
    where
        getIndividualFuelCost :: Pos -> Fuel
        getIndividualFuelCost position = spacesMoved * (spacesMoved + 1) `div` 2
            where
                spacesMoved = (abs $ dest - position)

solve2 :: Crabs -> Int
solve2 crabs = minimum $ getFuelCost2 crabs <$> [(minimum crabs)..(maximum crabs)]

main = do
    input <- readFile "input.txt"
    let crabs = processInput input
    print $ solve1 crabs
    print $ solve2 crabs