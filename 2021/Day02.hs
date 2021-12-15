import Data.List.Split

processInput :: String -> (String, Int)
processInput line = (direction, ((read::String->Int) value)) where
    direction:value:rest = splitOn " " line

findDepthAndDist :: [(String, Int)] -> (Int, Int) -> (Int, Int)
findDepthAndDist [] acc = acc
findDepthAndDist ((direction, value):rest) (depth, dist)
    | direction == "forward" = findDepthAndDist rest (depth, dist + value)
    | direction == "down" = findDepthAndDist rest (depth + value, dist)
    | direction == "up" = findDepthAndDist rest (depth - value, dist)

findDepthAndDistAndAim :: [(String, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
findDepthAndDistAndAim [] acc = acc
findDepthAndDistAndAim ((direction, value):rest) (depth, dist, aim)
    | direction == "forward" = findDepthAndDistAndAim rest (depth + aim * value, dist + value, aim)
    | direction == "down" = findDepthAndDistAndAim rest (depth, dist, aim + value)
    | direction == "up" = findDepthAndDistAndAim rest (depth, dist, aim - value)

-- Part 1
solve1 :: [(String, Int)] -> Int
solve1 lines = direction * value where (direction, value) = (findDepthAndDist lines (0, 0))

-- Part 2
solve2 :: [(String, Int)] -> Int
solve2 lines = direction * value where (direction, value, _) = (findDepthAndDistAndAim lines (0, 0, 0))

main = do
    input <- readFile "day2.txt"
    let lines = map processInput $ splitOn "\r\n" input
    print $ solve1 lines
    print $ solve2 lines