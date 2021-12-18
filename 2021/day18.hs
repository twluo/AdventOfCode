import Data.Char
import Data.List
import Data.List.Split(splitOn)

-- Helper Functions

data SFN = 
    Leaf Int |
    Branch SFN SFN |
    Empty
    deriving (Show, Eq)

type Path = String
type Marker = (Bool, Path, SFN)

-- Parsing
processInput :: String -> [SFN]
processInput input = parseSFN <$> (splitOn "\r\n" input)

parseSFN :: String -> SFN
parseSFN string
    | and $ isDigit <$> string = Leaf ((read::String -> Int) string)
    | otherwise = Branch (parseSFN left) (parseSFN right)
    where
        (left, right) = splitIn2 string "" 0

splitIn2 :: String -> String -> Int -> (String, String)
splitIn2 (',':rest) acc 1 = (drop 1 acc, init rest)
splitIn2 (curr:rest) acc x
    | curr == '[' = splitIn2 rest (acc ++ [curr]) (x + 1)
    | curr == ']' = splitIn2 rest (acc ++ [curr]) (x - 1)
    | otherwise = splitIn2 rest (acc ++ [curr]) x

-- SFN Utilities

removeNode :: SFN -> Path -> SFN
removeNode sfn "" = Leaf 0
removeNode (Branch left right) (c:rest)
    | c == 'L' = Branch (removeNode left rest) right
    | c == 'R' = Branch left (removeNode right rest)

updateNode :: SFN -> Path -> Int -> SFN
updateNode (Leaf x) "" value = Leaf (x + value)
updateNode sfn "" value = sfn
updateNode (Branch left right) (c:rest) value
    | c == 'L' = Branch (updateNode left rest value) right
    | c == 'R' = Branch left (updateNode right rest value)

findNode :: SFN -> Path -> SFN
findNode sfn "" = sfn
findNode (Branch left right) (c:rest)
    | c == 'L' = findNode left rest
    | c == 'R' = findNode right rest

mergeMarkers :: Marker -> Marker -> Marker
mergeMarkers (True, path, sfn) (False, _, _) = (True, path, sfn)
mergeMarkers (False, _, _) (True, path, sfn) = (True, path, sfn)
mergeMarkers (False, _, _) (False, _, _) = (False, "", Empty)
mergeMarkers (True, path1, sfn1) (True, path2, sfn2) = (True, path1, sfn1)

toString :: SFN -> String
toString (Leaf x) = show x
toString (Branch left right) = "[" ++ (toString left) ++ "," ++ (toString right) ++ "]"
-- Explotion

findExplosion :: SFN -> Path -> Marker
findExplosion sfn path =
    case sfn of
        Branch (Leaf left) (Leaf right) -> if (length path) >= 4 then (True, path, sfn) else (False, path, sfn)
        Branch left@(Branch x y) (Leaf right) -> findExplosion left (path ++ "L")
        Branch (Leaf left) right@(Branch x y) -> findExplosion right (path ++ "R")
        Branch left right -> mergeMarkers (findExplosion left (path ++ "L")) (findExplosion right (path ++ "R"))

findLeftNeighborPath :: SFN -> Path -> Bool -> Path
findLeftNeighborPath sfn "" _ = ""
findLeftNeighborPath sfn path True =
    case findNode sfn path of
        Leaf x -> path
        otherwise -> findLeftNeighborPath sfn (path ++ "R") True
findLeftNeighborPath sfn path False =
    case findNode sfn path of
        Leaf x -> path
        otherwise -> do
            case (last path) of
                'R' -> findLeftNeighborPath sfn ((init path) ++ "L") True
                'L' -> findLeftNeighborPath sfn (init path) False

findRightNeighborPath :: SFN -> Path -> Bool -> Path
findRightNeighborPath sfn "" _ = ""
findRightNeighborPath sfn path True =
    case findNode sfn path of
        Leaf x -> path
        otherwise -> findRightNeighborPath sfn (path ++ "L") True
findRightNeighborPath sfn path False =
    case findNode sfn path of
        Leaf x -> path
        otherwise -> do
            case (last path) of
                'L' -> findRightNeighborPath sfn ((init path) ++ "R") True
                'R' -> findRightNeighborPath sfn (init path) False

explode :: SFN -> Marker -> SFN
explode sfn (_, path, (Branch (Leaf left) (Leaf right))) = removeNode rightUpdate path
    where
        leftPath = findLeftNeighborPath sfn path False
        leftUpdate = updateNode sfn leftPath left
        rightPath = findRightNeighborPath sfn path False
        rightUpdate = updateNode leftUpdate rightPath right

-- Split

findSplit :: SFN -> Path -> Marker
findSplit sfn path =
    case sfn of
        (Leaf x) -> if x >= 10 then (True, path, sfn) else (False, path, sfn)
        Branch left right -> mergeMarkers (findSplit left (path ++ "L")) (findSplit right (path ++ "R"))

split :: SFN -> Marker -> SFN
split (Leaf x) (_, "", _) = Branch (Leaf lowerX) (Leaf higherX)
    where
        lowerX = floor (fromIntegral x/2)
        higherX = ceiling (fromIntegral x/2)
split (Branch left right) (_, (c:rest), _)
    | c == 'L' = Branch (split left (True, rest, Empty)) right
    | c == 'R' = Branch left (split right (True, rest, Empty))

-- Reduce

reduce :: SFN -> SFN
reduce sfn
    | shouldExplode = reduce $ explode sfn explosionMarker
    | shouldSplit = reduce $ split sfn splitMarker
    | otherwise = sfn
    where
        explosionMarker@(shouldExplode, _, _) = findExplosion sfn ""
        splitMarker@(shouldSplit, _, _) = findSplit sfn ""

-- Solution Helpers

addSFN :: SFN -> SFN -> SFN
addSFN sfn1 sfn2 = reduce $ Branch sfn1 sfn2

getMagnitude :: SFN -> Int
getMagnitude (Leaf x) = x
getMagnitude (Branch left right) = 3 * (getMagnitude left) + 2 * (getMagnitude right)

-- Part1
solve1 :: [SFN] -> Int
solve1 sfns= getMagnitude $ foldl addSFN (head sfns) (drop 1 sfns)

-- Part2
solve2 :: [SFN] -> Int
solve2 sfns = maximum $ (\(x, y) -> getMagnitude $ addSFN x y) <$> [(x, y) | x <- sfns, y <- sfns]

main = do
    input <- readFile "day18.txt"
    let sfns = processInput input
    print $ solve1 sfns
    print $ solve2 sfns