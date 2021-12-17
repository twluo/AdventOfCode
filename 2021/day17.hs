import Data.Ix
import Data.List.Split

-- Helper Functions

type Target = ((Int, Int), (Int, Int))

processInput :: String -> Target
processInput input = ((minX, maxX), (minY, maxY))
    where 
        rawValues = splitOn (", y=") $ drop 15 input
        ((minX:maxX:_):(minY:maxY:_):_) = ((read::String->Int) <$>) <$> (splitOn ".." <$> rawValues)

getNextPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
getNextPos (pX, pY) (vX, vY) = (pX + vX, pY + vY)

getNextVel :: (Int, Int) -> (Int, Int)
getNextVel (vX, vY) 
    | vX > 0 = (vX - 1, vY - 1)
    | vX < 0 = (vX + 1, vY - 1)
    | vX == 0 = (vX, vY - 1)

-- Part 1

getHighest :: (Int, Int) -> (Int, Int) -> Int
getHighest pos vel@(_, vY) 
    | vY < 0 = snd pos
    | otherwise = getHighest (getNextPos pos vel ) (getNextVel vel)

-- Given gravity, velocity when coming back down and reaching 0 will be equal to the initial velocity going up. So the maximum velocity going down is so that it reaches the max boundary in one step.
solve1 :: Target -> Int
solve1 ((_, _), (minY, _)) = getHighest (0, 0) (0, (abs minY) - 1)

-- Part2

hitTarget :: (Int, Int) -> (Int, Int) -> Target -> Bool
hitTarget pos@(x, y) vel (targetX@(_, maxX), targetY@(minY, _))
    | inRange targetX x && inRange targetY y = True
    | x > maxX = False
    | y < minY = False
    | otherwise = hitTarget (getNextPos pos vel ) (getNextVel vel) (targetX, targetY)

solve2 :: Target -> Int
solve2 target@((_, maxX), (minY, _)) = length $ filter (==True) $ (\x -> hitTarget (0, 0) x target) <$> [(x, y) | x <- [1..maxX], y <- [(abs minY), ((abs minY) - 1)..minY]]

main = do
    input <- readFile "day17.txt"
    let target = processInput input
    print $ solve1 target
    print $ solve2 target
