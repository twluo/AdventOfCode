import Data.List.Split
import Data.List
import qualified Data.HashMap.Strict as M

-- Helper Functions

type Rules = M.HashMap String Char
type Input = (String, Rules)

processInput :: String -> (String, Rules)
processInput input = (template, M.fromList rules)
    where
        (template:rawRules:_) = splitOn "\r\n\r\n" input
        rules = (\(x:y:_) -> (x, head y)) <$> (splitOn " -> " <$> (splitOn "\r\n" rawRules))

insertChar :: Rules -> String -> String
insertChar rules (x:y:_) = case M.lookup [x, y] rules of
    Nothing -> [x, y]
    Just c -> [x, c, y]

getPairs :: String -> [String]
getPairs string = (\(x, y) -> [x] ++ [y]) <$> (zip string $ drop 1 string)

getPairsCount :: String -> Int -> [(String, Int)]
getPairsCount string scale = (\x -> (head x, scale * (length x))) <$> (group $ sort $ getPairs string)

expand :: Rules -> String -> [(String, Int)] -> Int -> [(Char, Int)]
expand rules template [] gen = expand rules template (getPairsCount template 1) gen
expand rules template pairFreq 0 = (\(x, y) -> if x `elem` [head template, last template] then (x, y - ((y - 1) `div` 2)) else (x, y `div` 2)) <$> consolidatedCharFreq
    where
        charFreq = (\((x1:x2:_), y) -> if (x1 == x2) then [(x1, y * 2)] else [(x1, y), (x2, y)]) <$> pairFreq
        consolidatedCharFreq = M.toList $ foldr (M.unionWith (+)) M.empty $ M.fromList <$> charFreq

expand rules template pairFreq gen = expand rules template consolidatedPairFreq (gen - 1)
    where
        insertedPairFreq =  (\(x, y) -> (insertChar rules x, y)) <$> pairFreq
        expandedPairFreq = (\(x, y) -> (getPairsCount x y)) <$> insertedPairFreq
        consolidatedPairFreq = M.toList $ foldr (M.unionWith (+)) M.empty $ M.fromList <$> expandedPairFreq

findMostLeastCommon :: [(Char, Int)] -> (Int, Int)
findMostLeastCommon charFreq = (last sortedFreq, head sortedFreq)
    where
        sortedFreq = sort $ (\(x, y) -> y) <$> charFreq

-- Part 1

solve1 :: Input -> Int
solve1 (template, rules) = mostCommon - leastCommon
    where
        (mostCommon, leastCommon) = findMostLeastCommon $ expand rules template [] 10

-- Part 2

solve2 :: Input -> Int
solve2 (template, rules) = mostCommon - leastCommon
    where
        (mostCommon, leastCommon) = findMostLeastCommon $ expand rules template [] 40

main = do
    input <- readFile "day14.txt"
    let parsedInput = processInput input
    print $ solve1 parsedInput
    print $ solve2 parsedInput

{--  Old shitty Code

-- Naive Way
stepIn :: Rules -> String -> String -> String
stepIn rules [x] acc = (acc ++ [x])
stepIn rules (x:y:rest) acc = stepIn rules ([y] ++ rest) newAcc
    where
        newAcc = case M.lookup [x, y] rules of
            Nothing -> acc ++ [x]
            Just c -> acc ++ [x] ++ [c]

stepInX :: Rules -> String -> Int -> String
stepInX rules string 0 = string
stepInX rules string counter = stepInX rules (stepIn rules string "") (counter - 1)

findMostLeastCommon :: String -> (Int, Int)
findMostLeastCommon string = (last count, head count)
    where
        count = sort $ (\x -> length x) <$> (group $ sort $ string)

-- Bad Haskell Recursion :(

getFrequencyList :: String -> FrequencyMap
getFrequencyList string = M.fromList $ (\x -> (head x, length x)) <$> (group $ sort $ string)

mergeFrequencyList :: FrequencyMap -> FrequencyMap -> FrequencyMap
mergeFrequencyList map1 map2 = M.unionWith (+) map1 map2

scaleFrequencyList :: FrequencyMap -> Int -> FrequencyMap
scaleFrequencyList frequencyMap scale = M.fromList $ (\(x, y) -> (x, y * scale)) <$> (M.toList frequencyMap)

getFrequencyListGen :: Rules -> Int -> String -> FrequencyMap
getFrequencyListGen rules 0 string = getFrequencyList $ drop 1 string
getFrequencyListGen rules gen string = foldr mergeFrequencyList (M.fromList []) freqList
    where
        pairCount = getPairsCount string
        tripleCount = (\(x, y) -> (insertChar rules x, y)) <$> pairCount
        freqList = (\(x, y) -> scaleFrequencyList (getFrequencyListGen rules (gen - 1) x) y) <$> tripleCount

--}