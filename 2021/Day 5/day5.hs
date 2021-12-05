import Data.List.Split
import Data.List

-- Helper Functions
type Line = (Point, Point)

type Point = (Int, Int)

data Orientation = 
    Horizontal   |
    Vertical     |
    Diagonal Int
    deriving Show

processInput :: String -> [Line]
processInput input = 
    let rawLines = splitOn "\r\n" input
        rawCoords = (splitOn " -> ") <$> rawLines
        parsedCoords = ((splitOn ",") <$>) <$> rawCoords
        processCoord :: [String] -> Point
        processCoord (x:y:rest) = ((read::String -> Int) x, (read::String -> Int) y )
    in
        (\(x:y:rest) -> (x, y)) <$> (processCoord <$>) <$> parsedCoords

getLineOrientation :: Line -> Orientation
getLineOrientation ((x1, y1), (x2, y2))
    | x1 == x2 = Vertical
    | y1 == y2 = Horizontal
    | otherwise = Diagonal $ (x2 - x1) `div` (y2 - y1)

expandLine :: Line -> [Point]
expandLine line@((x1, y1), (x2, y2)) = 
    let maxX = max x1 x2
        maxY = max y1 y2
        minX = min x1 x2
        minY = min y1 y2
    in
        case getLineOrientation line of
            Horizontal -> [(x, maxY) | x <- [minX..maxX]]
            Vertical -> [(maxX, y) | y <- [minY..maxY]]
            Diagonal 1 -> zip [minX..maxX] [minY..maxY]
            Diagonal (-1) -> zip [minX..maxX] [maxY, maxY-1..minY]

getVisitedPoints :: [Line] -> [Point]
getVisitedPoints lines = concat $ map expandLine lines

countIntersection :: [Point] -> Int
countIntersection points = length $ filter (>1) $ map length $ group $ sortBy sortPoints $ points
    where
        sortPoints :: Point -> Point -> Ordering
        sortPoints (x1, y1) (x2, y2)
            | x1 < x2 = LT
            | x1 > x2 = GT
            | x1 == x2 && y1 < y2 = LT
            | x1 == x2 && y1 > y2 = GT
            | x1 == x2 && y1 == y2 = EQ

-- Part 1

solve1 :: [Line] -> Int
solve1 lines = countIntersection $ getVisitedPoints $ filter isOrthogonal lines
    where
        isOrthogonal :: Line -> Bool
        isOrthogonal line = case getLineOrientation line of
            Horizontal -> True
            Vertical -> True
            otherwise -> False

-- Part 2

solve2 :: [Line] -> Int
solve2 lines = countIntersection $ getVisitedPoints lines

main = do
    input <- readFile "input.txt"
    let lines = processInput input
    print $ solve1 lines 
    print $ solve2 lines

    