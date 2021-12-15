import Data.Ix
import Data.List.Split
import Data.List
import qualified Data.HashMap.Strict as M

-- Helper Functions

type Coord = (Int, Int)
type Boundaries = (Int, Int)
type Grid = M.HashMap Coord Int
type RiskGrid = (Grid, Boundaries)
type DistMap = M.HashMap Coord Int
type CoordPQ = [(Coord, Int)]

processInput :: String -> RiskGrid
processInput input = (M.fromList $ zip (concat gridCoords) (concat riskValues), (maxX, maxY))
    where
        rawRiskValues = splitOn "\r\n" input
        (maxX, maxY) = (((length $ head rawRiskValues) - 1), (length rawRiskValues) - 1)
        riskValues = ((read::String -> Int) <$>) <$> ((drop 1) <$> ((splitOn "") <$> rawRiskValues))
        gridCoords = [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]

gridToList :: RiskGrid -> [[Int]]
gridToList grid = ((getValue grid <$>) <$> (getGridCoords grid))

getGridCoords :: RiskGrid -> [[Coord]]
getGridCoords (_, (maxX, maxY)) = [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]

getValue :: RiskGrid -> Coord -> Int
getValue (grid, _) coord =
    case M.lookup coord grid of
        Nothing -> 0
        Just x -> x

getCost :: DistMap -> Coord -> Int
getCost grid coord =
    case M.lookup coord grid of
        Nothing -> 99999
        Just x -> x

getNeighborsCoord :: RiskGrid -> Coord -> [Coord]
getNeighborsCoord (_, (maxX, maxY)) (currX, currY) = filter (\(x, y) -> and [inRange (0, maxX) x, inRange (0, maxY) y]) potentialNeighbors
    where         
        neighborsOffset = [(-1, 0), (1, 0), (0, -1), (0, 1)]
        potentialNeighbors = (\(x, y) -> (currX + x, currY + y)) <$> neighborsOffset

sortPQ :: CoordPQ -> CoordPQ
sortPQ pq =  sortBy sortDist pq
    where
        sortDist :: (Coord, Int) -> (Coord, Int) -> Ordering
        sortDist (x1, y1) (x2, y2)
            | y1 < y2 = LT
            | y1 > y2 = GT
            | y1 == y2 = EQ

dijkstra :: RiskGrid -> CoordPQ -> DistMap -> DistMap
dijkstra (grid, boundaries) [] distMap = distMap
dijkstra (grid, boundaries) ((curr, currCost):rest) distMap = dijkstra (grid, boundaries) newPQ newDistMap
    where
        neighbors = (getNeighborsCoord (grid, boundaries) curr)
        costs = (\x -> currCost + (getValue (grid, boundaries) x)) <$>  neighbors
        cheaperNeighbor = filter (\(node, cost) -> (cost < (getCost distMap node))) $ zip neighbors costs
        newDistMap = M.union (M.fromList cheaperNeighbor) distMap
        newPQ = sortPQ (rest ++ cheaperNeighbor)

-- Part 1

solve1 :: RiskGrid -> Int
solve1 grid@(_, boundaries) = getCost (dijkstra grid [((0, 0), 0)] (M.fromList [((0, 0), 0)])) boundaries

-- Part 2
newValue ::  (Int, Int) -> Int -> Int
newValue (xOffset, yOffset) value = ((value + xOffset + yOffset - 1) `mod` 9) + 1

newCoord :: (Int, Int) -> (Int, Int) -> Coord -> Coord
newCoord (xOffset, yOffset) (maxX, maxY) (oldX, oldY) = ((maxX + 1) * xOffset + oldX, (maxY + 1) * yOffset + oldY)

newGrid :: (Int, Int) -> RiskGrid -> Grid
newGrid offset (grid, boundaries) = M.fromList $ (\(coord, value) -> (newCoord offset boundaries coord, newValue offset value)) <$> (M.toList grid)

expandGrid :: RiskGrid -> RiskGrid
expandGrid (grid, (maxX, maxY)) = (foldr M.union M.empty ((\x -> newGrid x (grid, (maxX, maxY))) <$> [(x, y) | x <- [0..5], y <- [0..5]]), ((maxX + 1) * 5 - 1, (maxY + 1) * 5 - 1))

solve2 :: RiskGrid -> Int
solve2 grid = solve1 $ expandGrid grid

main = do
    input <- readFile "day15.txt"
    let grid = processInput input
    print $ solve1 grid
    print $ solve1 $ expandGrid grid

