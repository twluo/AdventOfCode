import Data.Ix
import Data.List.Split
import Data.List

-- Helper Functions
type Coord = (Int, Int)
type HeightMap = [[Int]]
type Basin = [Coord]
type NeighborsCoord = [Coord]
type NeighborsValue = [Int]

processInput :: String -> HeightMap
processInput input = ((read::String -> Int) <$>) <$> ((drop 1) <$> ((splitOn "") <$> (splitOn "\r\n" input)))

getCell :: HeightMap -> Coord -> Int
getCell heightMap (x, y) = heightMap!!y!!x

getNeighborsCoord :: HeightMap -> Coord -> NeighborsCoord
getNeighborsCoord heightMap (currX, currY) = filter (\(x, y) -> and [inRange (0, maxX) x, inRange (0, maxY) y]) potentialNeighbors
    where         
        (maxX, maxY) = ((length $ head heightMap) - 1, (length heightMap) - 1)
        neighborsOffset = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        potentialNeighbors = (\(x, y) -> (currX + x, currY + y)) <$> neighborsOffset

getNeighbors :: HeightMap -> Coord -> NeighborsValue
getNeighbors heightMap (currX, currY) = (getCell heightMap) <$> getNeighborsCoord heightMap (currX, currY)

isLowPoint :: HeightMap -> Coord -> Bool
isLowPoint heightMap currPos = and $ (\x -> x > currValue) <$> neighbors
    where
        currValue = getCell heightMap currPos
        neighbors = getNeighbors heightMap currPos

-- Part 1

solve1 :: HeightMap -> [Coord] -> Int
solve1 heightMap coordList = sum riskLevels
    where
        riskLevels = (\coord -> if isLowPoint heightMap coord then (getCell heightMap coord + 1) else 0) <$> coordList

-- Part 2

getBasin :: HeightMap -> Coord -> Basin
getBasin heightMap currPos = getBasinHelper heightMap [currPos] []
    where
        getBasinHelper :: HeightMap -> NeighborsCoord -> Basin -> Basin
        getBasinHelper _ [] basinSoFar = nub basinSoFar
        getBasinHelper heightMap (candidate:rest) basinSoFar
            | (getCell heightMap candidate) /= 9 = getBasinHelper heightMap (rest ++ notInBasin) (basinSoFar ++ [candidate])
            | otherwise = getBasinHelper heightMap rest basinSoFar
            where            
                neighbors = getNeighborsCoord heightMap candidate    
                notInBasin = filter (\coord -> not $ coord `elem` basinSoFar) neighbors

solve2 :: HeightMap -> [Coord] -> Int
solve2 heightMap coordList = x1 * x2 * x3
    where 
        lowPoints = filter (\(x,y) -> x /= -1) $ (\coord -> if isLowPoint heightMap coord then coord else (-1, -1)) <$> coordList
        (x1:x2:x3:_) = reverse $ sort $ length <$> (getBasin heightMap <$> lowPoints)

main = do
    input <- readFile "day9.txt"
    let heightMap = processInput input
        coordList = [(x, y) | y <- [0..(length heightMap) - 1], x <- [0..(length $ head heightMap) - 1]]
    print $ solve1 heightMap coordList
    print $ solve2 heightMap coordList

