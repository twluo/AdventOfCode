import Data.List.Split
import Data.List

-- Helper Functions

processBoards :: [String] -> [[Int]]
processBoards [] = []
processBoards rawBoards = 
    let flattenBoards = intercalate " " <$> splitOn "\r\n" <$> rawBoards
        stringBoards = filter (/= "") <$> splitOn " " <$> flattenBoards
    in
        map (map (read::String -> Int)) stringBoards

processInput :: String -> ([Int], [[Int]])
processInput input =     
    let rawNumbers:rawBoards = splitOn "\r\n\r\n" input
        numbers = (read::String -> Int) <$> splitOn "," rawNumbers
        boards = processBoards rawBoards
    in
        (numbers, boards)

getIndexFromCoord :: (Int, Int) -> Int
getIndexFromCoord (x, y) = y * 5 + x

getCoordFromIndex :: Int -> (Int, Int)
getCoordFromIndex i = (i `mod` 5, i `div` 5)

getRowAndColIndex :: (Int, Int) -> ([Int], [Int])
getRowAndColIndex (x, y) = (xCoords ++ [getIndexFromCoord (x, y)], yCoords ++ [getIndexFromCoord (x, y)]) where
    yCoords = [getIndexFromCoord (x, i) | i <- [0..4], i /= y]
    xCoords = [getIndexFromCoord (i, y) | i <- [0..4], i /= x]
    
checkBingo :: [Int] -> Int -> (Bool, [Int])
checkBingo [] _ = (False, [])
checkBingo board x = (checkBingoHelper newBoard index, newBoard) where
    newBoard = map (\i -> if i == x then -1 else i) board
    index = elemIndex x board

checkBingoHelper :: [Int] -> Maybe Int -> Bool
checkBingoHelper [] _ = False
checkBingoHelper _ Nothing = False
checkBingoHelper board (Just x) = (all (== True) rowChecker) || (all (== True) colChecker) where
    (rows, cols) = getRowAndColIndex $ getCoordFromIndex x
    rowChecker = map (\i -> board!!i == -1) rows
    colChecker = map (\i -> board!!i == -1) cols

scoreBoard :: [Int] -> Int
scoreBoard [] = 0
scoreBoard board = sum $ filter (/= -1) board

-- Part 1

firstBoard :: [[Int]] -> [Int] -> Int -> Int -> ([[Int]], Int, Int)
firstBoard boards (num:rest) _ (-1) = firstBoard newBoards rest num boardIndex where
    (bingoFlag, newBoards) = unzip $ (`checkBingo` num) <$> boards
    boardIndex = case elemIndex True bingoFlag of 
        Nothing -> -1
        Just x -> x
firstBoard boards _ num x = (boards, num, x)

solve1 :: [[Int]] -> [Int] -> Int
solve1 boards numbers = boardScore * winNumber where
    (newBoards, winNumber, boardIndex) = firstBoard boards numbers 0 (-1)
    boardScore = scoreBoard $ newBoards!!boardIndex

-- Part 2

checkBingoAll :: [Int] -> Bool
checkBingoAll boards = rowChecker || colChecker where
    rows = [[getIndexFromCoord (x, y) | x <- [0..4]] | y <- [0..4]]
    cols = [[getIndexFromCoord (x, y) | y <- [0..4]] | x <- [0..4]]
    rowChecker = or $ (and <$> ((map (\i -> boards!!i == -1)) <$> rows))
    colChecker = or $ (and <$> ((map (\i -> boards!!i == -1)) <$> cols))

removeWinningBoards :: [[Int]] -> [[Int]]
removeWinningBoards boards = filter (\x -> not $ checkBingoAll x) boards

lastBoard :: [[Int]] -> [Int] -> Int -> Int -> ([[Int]], Int, Int)
lastBoard boards [] x y = (boards, x, y)
lastBoard boards (num:rest) _ (-1) = lastBoard newBoards rest num boardIndex where
    (bingoFlag, newBoards) = unzip $ (`checkBingo` num) <$> boards
    boardIndex = case elemIndex True bingoFlag of 
        Nothing -> -1
        Just x -> x        
lastBoard [board] _ num x = ([board], num, x)
lastBoard boards numbers num x = lastBoard (removeWinningBoards boards) numbers 0 (-1)

solve2 :: [[Int]] -> [Int] -> Int
solve2 boards numbers = boardScore * winNumber where
    (newBoards, winNumber, boardIndex) = lastBoard boards numbers 0 (-1)
    boardScore = scoreBoard $ head newBoards

main = do
    input <- readFile "day4.txt"
    let (numbers, boards) = processInput input
    print $ solve1 boards numbers
    print $ solve2 boards numbers
    