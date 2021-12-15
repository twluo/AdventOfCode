import Data.Ix
import Data.List.Split
import Data.List
import qualified Data.HashMap.Strict as M

-- Constants
row = 9
col = 9
size = 100
gridCoords = [[(x,y) | x <- [0..row]] | y <- [0..col]]

-- Helper Functions
type Coord = (Int, Int)
type Grid = M.HashMap Coord Int

processInput :: String -> Grid
processInput input = M.fromList $ zip (concat gridCoords) (concat gridValues)
    where
        gridValues = ((read::String -> Int) <$>) <$> ((drop 1) <$> ((splitOn "") <$> (splitOn "\r\n" input)))
        
getCell :: Grid -> Coord -> Int
getCell grid coord =
    case M.lookup coord grid of
        Nothing -> -100
        Just x -> x

gridToList :: Grid -> [[Int]]
gridToList grid = (getCell grid <$>) <$> gridCoords

increaseLevelGrid :: Grid -> Grid
increaseLevelGrid grid = M.fromList $ zip (concat gridCoords) (concat gridValues)
    where
        gridValues = ((+1) <$>) <$> (gridToList grid)

increaseLevelCell :: Grid -> Coord -> Grid
increaseLevelCell grid coord = M.insert coord newValue grid
    where
        newValue = (getCell grid coord) + 1

getNeighborsCoord :: Coord -> [Coord]
getNeighborsCoord (currX, currY) = filter (\(x, y) -> and [inRange (0, row) x, inRange (0, col) y]) potentialNeighbors
    where         
        neighborsOffset = filter (/= (0, 0))  [(x, y) | x <- [-1..1], y <- [-1..1]]
        potentialNeighbors = (\(x, y) -> (currX + x, currY + y)) <$> neighborsOffset

canFlash :: Grid -> Bool
canFlash grid = not $ null flashable
    where
        flashable = filter (> 9) $ concat $ gridToList grid

flashCell :: Grid -> Coord -> Grid
flashCell grid coord 
    | value > 9 = flashCellHelper grid neighbors
    | otherwise = grid
    where
        value = getCell grid coord
        neighbors = getNeighborsCoord coord
        flashCellHelper :: Grid -> [Coord] -> Grid
        flashCellHelper grid [] = M.insert coord (-10) grid
        flashCellHelper grid (coord:rest) = flashCellHelper (increaseLevelCell grid coord) rest

flash :: Grid -> Grid
flash grid 
    | canFlash grid = flash $ flashHelper grid (concat gridCoords)
    | otherwise = grid
    where
        canFlash :: Grid -> Bool
        canFlash grid = not $ null flashable
            where
                flashable = filter (> 9) $ concat $ gridToList grid
        flashHelper :: Grid -> [Coord] -> Grid
        flashHelper grid [] = grid
        flashHelper grid (coord:rest) = flashHelper (flashCell grid coord) rest

reset :: Grid -> (Int, Grid)
reset grid = (flashes, M.fromList $ zip (concat gridCoords) (concat gridValues))
    where
        gridValues = ((\x -> if x <= 0 then 0 else x) <$>) <$> (gridToList grid)
        flashes = length $ filter (==0) (concat gridValues)

stepIn :: Grid -> (Int, Grid)
stepIn grid =
    let step1 = increaseLevelGrid grid
        step2 = flash step1
    in
        reset step2
        
-- Part 1

countFlashes :: Grid -> Int -> Int -> Int
countFlashes grid 0 acc = acc
countFlashes grid stepsRemaining acc = countFlashes newGrid (stepsRemaining - 1) (acc + flashes)
    where
        (flashes, newGrid) = stepIn grid

solve1 :: Grid -> Int
solve1 grid = countFlashes grid 100 0

-- Part 2

firstSimul :: Grid -> Int -> Int
firstSimul grid currGen
    | flashes == size = currGen + 1
    | otherwise = firstSimul newGrid (currGen + 1)
    where
        (flashes, newGrid) = stepIn grid

solve2 :: Grid -> Int
solve2 grid = firstSimul grid 0
        
main = do
    input <- readFile "day11.txt"
    let grid = processInput input
    print $ solve1 grid
    print $ solve2 grid