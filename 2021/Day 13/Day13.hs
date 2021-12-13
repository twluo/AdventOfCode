import Data.List.Split
import Data.List
import qualified Data.HashMap.Strict as M

-- Helper Functions

type Coord = (Int, Int)
type Boundaries = (Int, Int)
type Grid = (M.HashMap Coord Bool, Boundaries)
type Instruction = (Char, Int)

processInput :: String -> (Grid, [Instruction])
processInput input = 
    let (rawCoords:rawInstructions:_) = splitOn "\r\n" <$> splitOn "\r\n\r\n" input
        coords = (\(x:y:_) -> ((read::String -> Int)  x, (read::String -> Int)  y)) <$> (splitOn "," <$> rawCoords)
        instructions = (\(x:y:_) -> (last x, (read::String -> Int) y)) <$> (splitOn "=" <$> rawInstructions)
        (x,y) = unzip coords
        (maxX, maxY) = (maximum x, maximum y)
        blankGrid = getBlankGrid (maxX, maxY)
        newGrid = insertIntoGrid blankGrid coords
    in
        (newGrid, instructions)
    
getGridCoords :: Grid -> [[Coord]]
getGridCoords (_, (maxX, maxY)) = [[(x,y) | x <- [0..maxX]] | y <- [0..maxY]]

getBlankGrid :: Boundaries -> Grid
getBlankGrid (maxX, maxY) = (M.fromList $ zip gridCoords gridValues, (maxX, maxY))
    where
        gridCoords = [(x,y) | x <- [0..maxX], y <- [0..maxY]]
        gridValues = [False | x <- [0..(length gridCoords)]]
        
getCell :: Grid -> Coord -> Bool
getCell (grid, _) coord =
    case M.lookup coord grid of
        Nothing -> False
        Just x -> x

insertIntoGrid :: Grid -> [Coord] -> Grid
insertIntoGrid grid [] = grid
insertIntoGrid (grid, bound) (coord:rest) = insertIntoGrid (M.insert coord True grid, bound) rest

updateCells :: Grid -> [(Coord, Coord)] -> Grid
updateCells grid [] = grid
updateCells (grid, boundaries) (curr:rest) = updateCells (M.insert newCoord newValue grid, boundaries) rest
    where
        (oldCoord, newCoord) = curr
        newValue = (getCell (grid, boundaries) oldCoord) || (getCell (grid, boundaries) newCoord)

flipX :: Int -> Coord -> Coord
flipX middle (x, y) 
    | y > middle = (x, (y - (y - middle) * 2))
    | y <= middle = (x, y)

foldX :: Grid -> Instruction -> Grid
foldX gridT@(grid, (maxX, maxY)) (_, middle) = (newGrid, (maxX, middle - 1))
    where
        coordList = getGridCoords gridT
        transList = (\x -> (x, flipX middle x)) <$> (concat $ coordList)
        (newGrid, _) = updateCells gridT transList

flipY :: Int -> Coord -> Coord
flipY middle (x, y) 
    | x > middle = ((x - (x - middle) * 2), y)
    | x <= middle = (x, y)

foldY :: Grid -> Instruction -> Grid
foldY gridT@(grid, (maxX, maxY)) (_, middle) = (newGrid, (middle -1, maxY))
    where
        coordList = getGridCoords gridT
        transList = (\x -> (x, flipY middle x)) <$> (concat $ coordList)
        (newGrid, _) = updateCells gridT transList

execute :: Grid -> [Instruction] -> Grid
execute grid [] = grid
execute grid ((axis, middle):rest) 
    | axis == 'y' = execute (foldX grid (axis, middle)) rest
    | axis == 'x' = execute (foldY grid (axis, middle)) rest 

-- Part 1

countDots :: Grid -> Int
countDots grid = length $ filter (== '#') $ concat $ gridToList grid

solve1 :: Grid -> [Instruction] -> Int
solve1 grid (curr:rest) = countDots $ execute grid [curr]

-- Part 2

gridToList :: Grid -> [[Char]]
gridToList grid = ((\x -> if x then '#' else '.') <$>) <$>((getCell grid <$>) <$> (getGridCoords grid))

solve2 :: Grid -> [Instruction] -> [[Char]]
solve2 grid instructions = gridToList $ execute grid instructions

main = do
    input <- readFile "input.txt"
    let (grid, instructions) = processInput input
    print $ solve1 grid instructions
    mapM print $ solve2 grid instructions