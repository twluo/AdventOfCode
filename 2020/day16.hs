import Data.List.Split
import Data.List
import Data.Ix
import Text.Regex
import GHC.Exts (sortWith)

parseMinMax :: [Int] -> [(Int, Int)]
parseMinMax x = [(min1, max1), (min2, max2)] where min1:max1:min2:max2:_ = x

checkRanges :: [(Int, Int)] -> Int -> Bool
checkRanges ranges x = all (== False) (map (flip (inRange) x) ranges)

checkRangesList :: [(Int, Int)] -> Int -> [Bool]
checkRangesList ranges x = (map (flip (inRange) x) ranges)

formatIntervals :: [Int] -> ((Int, Int), (Int, Int))
formatIntervals x = ((min1, max1), (min2, max2)) where min1:max1:min2:max2:_ = x

formatConditions :: (String, ((Int, Int), (Int, Int))) -> (String, (Int, Int), (Int, Int))
formatConditions (name, (interval1, interval2)) = (name, interval1, interval2)

checkCondition :: (String, (Int, Int), (Int, Int)) -> Int -> (String, Bool)
checkCondition (name, interval1, interval2) x = (name, or [(inRange interval1 x), (inRange interval2 x)])

checkConditions :: [(String, (Int, Int), (Int, Int))] -> Int -> [String]
checkConditions conditions x = map (\(x, y) -> x) (filter (\(x, y) -> y == True) (map (flip (checkCondition) x) conditions))

checkConditionHelper :: [(String, (Int, Int), (Int, Int))] -> Int -> [(String, Bool)]
checkConditionHelper conditions x = (filter (\(x, y) -> y == True) (map (flip (checkCondition) x) conditions))

findSolution :: [(Int, String)] -> [(Int, [String])] -> [(Int, String)]
findSolution sol [(pos, [val])] = sol ++ [(pos, val)]
findSolution sol ((pos, [val]):ls) = findSolution (sol ++ [(pos, val)]) (map (\(x, y) -> (x, delete val y)) ls)

main = do
    --str <- readFile "day16.txt"
    --let cond:(_:mytix):((_:neartix):_) = (map (splitOn "\r\n") (splitOn "\r\n\r\n" str))
    str <- readFile "day16p.txt"
    let cond:(_:mytix):((_:neartix):_) = (map (splitOn "\n") (splitOn "\n\n" str))
        conds = foldMap parseMinMax (map (map (read::String->Int)) (map (drop 1) (map (splitRegex (mkRegex "[^0-9]+")) cond)))
        tickets = map (map (read::String->Int)) (map (splitOn ",") (mytix ++ neartix))
        rangeCheck = sum (foldMap (filter (checkRanges conds)) tickets)
    print rangeCheck

    let goodTickets = filter (\x -> (length x) == (length cond)) (map (filter (not.checkRanges conds)) tickets)
        conJustList = map (matchRegex (mkRegex "(.*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)")) cond
        Just conStringList = sequence conJustList
        conListNames = map head conStringList
        conListIntervals = map formatIntervals (map (map (read::String->Int)) (map tail conStringList))
        conditions = map formatConditions (zip conListNames conListIntervals)

        goodTicketsT = transpose goodTickets
        possibilitiesT = (map (map (checkConditions conditions)) goodTicketsT)
        possibilitiesIntersect = map (foldr1 intersect) possibilitiesT
        possibilitiesCount = map length possibilitiesIntersect
        possibilitiesWithCol = zip [1..] possibilitiesIntersect
        possibilitiesSort = sortWith (\(x, y) -> length y) possibilitiesWithCol

    let solution = (findSolution [] (sortWith length possibilitiesSort))
        filterSolution = filter (\(x, y) -> isInfixOf "departure" y) solution
        indecesOfSolution = map (\(x, y) -> x - 1) filterSolution
        myTicket = head goodTickets
        departureValues = map (\x -> myTicket!!x) indecesOfSolution
        result = product departureValues
    print (sortWith (\(x, y) -> x) solution)
    print result