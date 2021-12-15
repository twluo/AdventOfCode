import Data.List.Split
import Data.List

-- Constants

open = ["(", "{", "<", "["]
close = [")", "}", ">", "]"]
validChunks = zipWith (++) open close
corruptChunks = [x ++ y | x <- open, y <- close, (x++y) `notElem` validChunks]

-- Helper Functions

containsValidChunk :: String -> Bool
containsValidChunk line = or ((\x -> x `isInfixOf` line) <$> validChunks)

stripValidChunks :: String -> String
stripValidChunks "" = ""
stripValidChunks line 
    | containsValidChunk line = stripValidChunks $ stripValidChunksHelper line ""
    | otherwise = line
    where
        stripValidChunksHelper :: String -> String -> String
        stripValidChunksHelper "" acc = acc
        stripValidChunksHelper [c1] acc = acc ++ [c1]
        stripValidChunksHelper (c1:c2:rest) acc 
            | [c1,c2] `elem` validChunks = stripValidChunksHelper rest acc
            | otherwise = stripValidChunksHelper ([c2] ++ rest) (acc ++ [c1])

isCorrupt :: String -> Bool
isCorrupt line = or ((\x -> x `isInfixOf` line) <$> corruptChunks)

processInput :: String -> ([String], [String])
processInput input = partition isCorrupt $ stripValidChunks <$> (splitOn ("\r\n") input)

-- Part 1

getCorruptionScore :: Char -> Int
getCorruptionScore ')' = 3
getCorruptionScore ']' = 57
getCorruptionScore '}' = 1197
getCorruptionScore '>' = 25137

getFirstCorruption :: String -> Char
getFirstCorruption "" = '_'
getFirstCorruption (c1:c2:rest)
    | [c1, c2] `elem` corruptChunks = c2
    | otherwise = getFirstCorruption ([c2] ++ rest)

solve1 :: [String] -> Int
solve1 corrupted = sum $ getCorruptionScore <$> (getFirstCorruption <$> corrupted)

-- Part 2

getAutoCompleteChar :: Char -> Char
getAutoCompleteChar '(' = ')'
getAutoCompleteChar '[' = ']'
getAutoCompleteChar '{' = '}'
getAutoCompleteChar '<' = '>'

getAutoCompleteCharScore :: Char -> Int
getAutoCompleteCharScore ')' = 1
getAutoCompleteCharScore ']' = 2
getAutoCompleteCharScore '}' = 3
getAutoCompleteCharScore '>' = 4

getAutoCompleteString :: String -> String
getAutoCompleteString incomplete = getAutoCompleteChar <$> (reverse incomplete)

getAutoCompleteScore :: String -> Int
getAutoCompleteScore autocomplete = foldr (\x y -> y * 5 + x) 0 (getAutoCompleteCharScore <$> (reverse autocomplete))

solve2 :: [String] -> Int
solve2 incomplete = (sort $ getAutoCompleteScore <$> (getAutoCompleteString <$> incomplete))!!(((length incomplete) - 1) `div` 2)

main = do
    input <- readFile "day10.txt"
    let (corrupted, incomplete) = processInput input
    print $ solve1 corrupted
    print $ solve2 incomplete
