import Data.List.Split
import Data.List

-- Constants

open = ["(", "{", "<", "["]
close = [")", "}", ">", "]"]
validChunks = zipWith (++) open close
corruptChunks = [x ++ y | x <- open, y <- close, (x++y) `notElem` validChunks]

-- Helper Functions

processInput :: String -> [String]
processInput input = splitOn ("\r\n") input

checkLines :: String -> String -> (String, Int)
checkLines autoComplete ""  = ("incomplete", getAutoCompleteScore autoComplete)
checkLines autoComplete (c:rest)  
    | ([c] ++ "") `elem` open = checkLines ([(getAutoCompleteChar c)] ++ autoComplete) rest
    | otherwise = if c == (head autoComplete) then checkLines (drop 1 autoComplete) rest else ("corrupted", getCorruptionScore c)
    
-- Part 1

getCorruptionScore :: Char -> Int
getCorruptionScore ')' = 3
getCorruptionScore ']' = 57
getCorruptionScore '}' = 1197
getCorruptionScore '>' = 25137

solve1 :: [Int] -> Int
solve1 corrupted = sum $ corrupted

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

getAutoCompleteScore :: String -> Int
getAutoCompleteScore autocomplete = foldr (\x y -> y * 5 + x) 0 (getAutoCompleteCharScore <$> (reverse autocomplete))

solve2 :: [Int] -> Int
solve2 incomplete = (sort $ incomplete)!!(((length incomplete) - 1) `div` 2)
    
main = do
    input <- readFile "input.txt"
    let lines = processInput input
        (corrupted, incomplete) = partition (\(x, y) -> x == "corrupted") ((checkLines "") <$> lines)
    print $ solve1 . snd $ unzip corrupted
    print $ solve2 . snd $ unzip incomplete
