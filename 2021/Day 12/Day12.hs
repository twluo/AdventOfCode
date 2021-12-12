import Data.Char
import Data.List.Split
import Data.List
import GHC.Exts
import qualified Data.HashMap.Strict as M

-- Helper Functions

type Node = String
type Graph = M.HashMap Node [Node]
type Path = [Node]

processInput :: String -> Graph
processInput input = M.fromList listOfNodes
    where 
        lines = splitOn "\r\n" input
        edges = (\(x:y:_) -> (x, y)) <$> (splitOn "-" <$> lines)
        invertedEdges = (\(x, y) -> (y, x)) <$> edges
        completeEdges = groupWith fst $ edges ++ invertedEdges
        listOfNodes = (\(x, y) -> (head x, y)) <$> (unzip <$> completeEdges)

getNeighbors :: Graph -> Node -> [Node]
getNeighbors graph node = 
    case M.lookup node graph of
        Nothing -> []
        Just x -> x

-- Part 1

findPart1Paths :: Graph -> [Node] -> Path -> Node -> [Path]
findPart1Paths graph visited pathSoFar curr
    | curr == "end" = [pathSoFar ++ [curr]]
    | and $ isUpper <$> curr = concat $ findPart1Paths graph (visited) (pathSoFar ++ [curr]) <$> neighbors
    | and $ isLower <$> curr = concat $ findPart1Paths graph (visited ++ [curr]) (pathSoFar ++ [curr]) <$> neighbors
    where
        neighbors = filter (\x -> x `notElem` visited) $ getNeighbors graph curr

solve1 :: Graph -> Int
solve1 graph = length $ findPart1Paths graph [] [] "start"

-- Part2

findPart2Paths :: Graph -> [Node] -> Path -> Node -> [Path]
findPart2Paths graph visited pathSoFar curr
    | pathSoFar == [] = concat $ findPart2Paths graph (visited ++ [curr]) (pathSoFar ++ [curr]) <$> neighbors
    | curr == "end" = [pathSoFar ++ [curr]]
    | majorCave = concat $ findPart2Paths graph (visited) (pathSoFar ++ [curr]) <$> neighbors
    | canMoveInTwice && minorCave && alreadyInPath = concat $ findPart2Paths graph (visited ++ [curr]) (pathSoFar ++ [curr]) <$> neighbors
    | canMoveInTwice && minorCave = concat $ findPart2Paths graph (visited) (pathSoFar ++ [curr]) <$> neighbors
    | minorCave && alreadyInPath = []
    | minorCave = concat $ findPart2Paths graph (visited ++ [curr]) (pathSoFar ++ [curr]) <$> neighbors
    where
        neighbors = filter (\x -> x `notElem` visited) $ getNeighbors graph curr
        canMoveInTwice = 2 /= (last $ sort $ map length $ group $ sort $ filter (\x -> and $ isLower <$> x ) pathSoFar) 
        alreadyInPath = curr `elem` pathSoFar
        minorCave = and $ isLower <$> curr
        majorCave = and $ isUpper <$> curr

solve2 :: Graph -> Int
solve2 graph = length $ findPart2Paths graph [] [] "start"

main = do
    input <- readFile "input.txt"
    let graph = processInput input
    print $ solve1 graph
    print $ solve2 graph
    