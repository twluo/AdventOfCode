{-| 

import Data.List.Split
import Data.Ix

countAlive :: [[String]] -> Int
countAlive x = countAliveS $ concat $ concat x

countAliveS :: String -> Int
countAliveS x = length $ filter (== '#') x

expandGrid :: [[String]] -> [[String]]
expandGrid aGrid  = zPaddedInnergrid where
    size = length (head aGrid) + 2
    yPadding = replicate size '.'
    zPadding = replicate size yPadding
    xPaddedInnergrid = map (map (\x -> '.':x ++ ['.'])) aGrid
    yPaddedInnergrid = map (\x -> yPadding:x ++ [yPadding]) xPaddedInnergrid
    zPaddedInnergrid = zPadding:yPaddedInnergrid ++ [zPadding]

getNeighborsCoord :: (Int, Int, Int) -> (Int, Int) -> [(Int, Int, Int)]
getNeighborsCoord (x, y, z) (size, sizeZ) = neighbors where
    directions = [(xd, yd, zd) | xd <- [-1..1], yd <- [-1..1], zd <- [-1..1], (xd, yd, zd) /= (0, 0, 0)]
    possibleNeighbors = map (\(xd, yd, zd) -> (x + xd, y + yd, z + zd)) directions
    range = (0, size - 1)
    rangeZ = (0, sizeZ - 1)
    neighbors = filter (\(xn, yn, zn) -> and [inRange range xn, inRange range yn, inRange rangeZ zn]) possibleNeighbors

getNeighborsCount :: (Int, Int, Int) -> [[String]] -> Int
getNeighborsCount pos grid = count where
    neighborCoords = getNeighborsCoord pos (length $ head grid, length grid)
    neighbors = map (\(x, y, z) -> grid!!z!!y!!x) neighborCoords
    count = countAliveS neighbors

getCell :: (Int, Int, Int) -> [[String]] -> Char
getCell (x, y, z) grid = grid!!z!!y!!x

getNewCell :: Char -> Int -> Char
getNewCell '#' 2 = '#'
getNewCell '#' 3 = '#'
getNewCell '.' 3 = '#'
getNewCell cell neighborCount = '.'

evolveCell :: (Int, Int, Int) -> [[String]] -> Char
evolveCell pos grid = newCell where
    cell = getCell pos grid
    neighborCount = getNeighborsCount pos grid
    newCell = getNewCell cell neighborCount

evolveX :: Int -> Int -> [[String]] -> String
evolveX y z grid = map (\x -> evolveCell (x, y, z) grid) [0..(length $ head $ head grid) - 1]

evolveY :: Int -> [[String]] -> [String]
evolveY z grid = map (\y -> evolveX y z grid) [0..(length $ head grid) - 1]

evolveZ :: [[String]] -> [[String]]
evolveZ grid = map (\z -> evolveY z grid) [0..(length grid) - 1]

evolveGrid :: [[String]] -> [[String]]
evolveGrid grid = evolvedGrid where
    expandedGrid = expandGrid grid
    evolvedGrid = evolveZ expandedGrid

evolve :: Int -> [[[String]]] -> [[String]]
evolve 6 evolutions = last evolutions
evolve n evolutions = evolve (n+1) (evolutions ++ [evolveGrid (last evolutions)])



main = do
    str <- readFile "day17.txt"
    samplestr <- readFile "day17s.txt"
    let grid = [splitOn "\r\n" str]
        sampleGrid = [splitOn "\r\n" samplestr]
    print sampleGrid
    print $ countAlive $ evolve 0 [grid]

-}
import Data.List.Split
import Data.Ix

countAlive :: [[[String]]] -> Int
countAlive x = countAliveS $ concat $ concat $ concat x

countAliveS :: String -> Int
countAliveS x = length $ filter (== '#') x

expandGrid :: [[[String]]] -> [[[String]]]
expandGrid aGrid  = wPaddedInnergrid where
    size = (length $ head $ head aGrid) + 2
    sizeZ = (length aGrid) + 2
    yPadding = replicate size '.'
    zPadding = replicate size yPadding
    wPadding = replicate sizeZ zPadding
    xPaddedInnergrid = map (map (map (\x -> '.':x ++ ['.']))) aGrid
    yPaddedInnergrid = map (map (\x -> yPadding:x ++ [yPadding])) xPaddedInnergrid
    zPaddedInnergrid = map (\x -> zPadding:x ++ [zPadding]) yPaddedInnergrid
    wPaddedInnergrid = wPadding:zPaddedInnergrid ++ [wPadding]

getNeighborsCoord :: (Int, Int, Int, Int) -> (Int, Int) -> [(Int, Int, Int, Int)]
getNeighborsCoord (x, y, z, w) (size, sizeZ) = neighbors where
    directions = [(xd, yd, zd, wd) | xd <- [-1..1], yd <- [-1..1], zd <- [-1..1], wd <- [-1..1], (xd, yd, zd, wd) /= (0, 0, 0, 0)]
    possibleNeighbors = map (\(xd, yd, zd, wd) -> (x + xd, y + yd, z + zd, w + wd)) directions
    range = (0, size - 1)
    rangeZ = (0, sizeZ - 1)
    neighbors = filter (\(xn, yn, zn, wn) -> and [inRange range xn, inRange range yn, inRange rangeZ zn, inRange rangeZ wn]) possibleNeighbors

getNeighborsCount :: (Int, Int, Int, Int) -> [[[String]]] -> Int
getNeighborsCount pos grid = count where
    neighborCoords = getNeighborsCoord pos (length $ head $ head $ grid, length grid)
    neighbors = map (flip getCell grid) neighborCoords
    count = countAliveS neighbors

getCell :: (Int, Int, Int, Int) -> [[[String]]] -> Char
getCell (x, y, z, w) grid = grid!!w!!z!!y!!x

getNewCell :: Char -> Int -> Char
getNewCell '#' 2 = '#'
getNewCell '#' 3 = '#'
getNewCell '.' 3 = '#'
getNewCell cell neighborCount = '.'

evolveCell :: (Int, Int, Int, Int) -> [[[String]]] -> Char
evolveCell pos grid = newCell where
    cell = getCell pos grid
    neighborCount = getNeighborsCount pos grid
    newCell = getNewCell cell neighborCount

evolveX :: Int -> Int -> Int -> [[[String]]] -> String
evolveX y z w grid = map (\x -> evolveCell (x, y, z, w) grid) [0..(length $ head $ head $ head grid) - 1]

evolveY :: Int -> Int -> [[[String]]] -> [String]
evolveY z w grid = map (\y -> evolveX y z w grid) [0..(length $ head $ head grid) - 1]

evolveZ :: Int -> [[[String]]] -> [[String]]
evolveZ w grid = map (\z -> evolveY z w grid) [0..(length $ head $ grid) - 1]

evolveW :: [[[String]]] -> [[[String]]]
evolveW grid = map (\w -> evolveZ w grid) [0..(length grid) - 1]

evolveGrid :: [[[String]]] -> [[[String]]]
evolveGrid grid = evolvedGrid where
    expandedGrid = expandGrid grid
    evolvedGrid = evolveW expandedGrid

evolve :: Int -> [[[[String]]]] -> [[[String]]]
evolve 6 evolutions = last evolutions
evolve n evolutions = evolve (n+1) (evolutions ++ [evolveGrid (last evolutions)])


main = do
    str <- readFile "day17.txt"
    samplestr <- readFile "day17s.txt"
    let grid = [[splitOn "\r\n" str]]
        sampleGrid = [[splitOn "\r\n" samplestr]]
    print $ countAlive $ evolve 0 [grid]