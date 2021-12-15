import Data.List.Split
import Data.List

type DecodedDigits = [(String, Int)]
type DigitsOutput = ([String], [String])
type DigitsOutputList = [DigitsOutput]

processInput :: String -> DigitsOutputList
processInput input = 
    let lines = splitOn "\r\n" input
        line = (splitOn " | " ) <$> lines
        splittedLines = (map $ splitOn " " ) <$> line
    in
        (\(digits:output:rest) -> (sort<$> digits, sort <$> output)) <$> splittedLines

-- Part 1

solve1 :: DigitsOutputList -> Int
solve1 digitsOutput = length filteredOutput
    where
        (digits, output) = unzip digitsOutput
        flatOutput = concat output
        filteredOutput = filter (\x -> length x `elem` [2,3,4,7]) flatOutput

-- Part 2

findSubsetMatch :: String -> [String] -> (String, [String])
findSubsetMatch subset candidates = findSubsetMatchHelper subset candidates "" []
    where
        findSubsetMatchHelper :: String -> [String] -> String -> [String] -> (String, [String])
        findSubsetMatchHelper subset [] match others = (match, others)
        findSubsetMatchHelper subset (candidate:rest) match others
            | intersect subset candidate == subset = (candidate, others ++ rest)
            | otherwise = findSubsetMatchHelper subset rest match (others ++ [candidate])

findOneDiff :: String -> [String] -> (String, [String])
findOneDiff subset candidates = findOneDiffHelper subset candidates "" []
    where
        findOneDiffHelper :: String -> [String] -> String -> [String] -> (String, [String])
        findOneDiffHelper subset [] match others = (match, others)
        findOneDiffHelper subset (candidate:rest) match others
            | (length $ intersect subset candidate) == ((length $ subset) - 1) = (candidate, others ++ rest)
            | otherwise = findOneDiffHelper subset rest match (others ++ [candidate])

{--
    (we know 1 4 7 8)
    (find the length 6 that shares all from 4 to get 9)
    (find the length 5 that shares all from 1 to get 3)
    (find the length 6 that shares all from 1 to get 0)
    (last length 6 is 6)
    (find the one that is missing 1 to become 6 to get 5)
    (remaining is 2)
--}
decodeDigits :: [String] -> DecodedDigits
decodeDigits digits = zip [zero, one, two, three, four, five, six, seven, eight, nine] [0..9]
    where        
        twoThreeFive = filter (\x -> (length x == 5)) digits
        nineZeroSix = filter (\x -> (length x == 6)) digits
        one = (head $ filter (\x -> (length x == 2)) digits)
        four = (head $ filter (\x -> (length x == 4)) digits)
        seven = (head $ filter (\x -> (length x == 3)) digits)
        eight = (head $ filter (\x -> (length x == 7)) digits)
        (nine, zeroSix) = findSubsetMatch four nineZeroSix
        (three, twoFive) = findSubsetMatch one twoThreeFive
        (zero, sixList) = findSubsetMatch one zeroSix
        six = head sixList
        (five, twoList) = findOneDiff six twoFive
        two = head twoList

decodeOutput :: [String] -> DecodedDigits -> Int
decodeOutput output digits = decodeOutputHelper output digits 0
    where
        getValue :: String -> DecodedDigits -> Int
        getValue digit digits = sum $ (\(x,y) -> if x == digit then y else 0) <$> digits
        decodeOutputHelper :: [String] -> DecodedDigits -> Int -> Int
        decodeOutputHelper [] digits acc = acc
        decodeOutputHelper (x:xs) digits acc = decodeOutputHelper xs digits (acc * 10 + (getValue x digits))

decodeDigitOutput :: DigitsOutput -> Int
decodeDigitOutput (digits, output) = decodeOutput output decodedDigits
    where
        decodedDigits = decodeDigits digits

solve2 :: DigitsOutputList -> Int
solve2 digitsOutput = sum $ decodeDigitOutput <$> digitsOutput

main = do
    input <- readFile "day8.txt"
    let digitsOutput = processInput input
    print $ solve1 digitsOutput
    print $ solve2 digitsOutput
