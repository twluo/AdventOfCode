import Data.List.Split
import Data.List

-- Helper functions
binToDec :: String -> Int
binToDec x = binToDecHelper $ (read::String -> Int) $ x

binToDecHelper :: Int -> Int
binToDecHelper 0 = 0
binToDecHelper i = 2 * binToDecHelper (div i 10) + (mod i 10)

findMajority :: String -> Char
findMajority "" = ' '
findMajority "1" = '1'
findMajority "0" = '0'
findMajority number 
    | count0 > count1 = '0'
    | count1 > count0 = '1'
    | count0 == count1 = '1'
    | otherwise = ' '
    where count0:count1:_ = map length . group . sort $ number

invertDigit :: Char -> Char
invertDigit '0' = '1'
invertDigit '1' = '0'
invertDigit c = c

-- Part 1
getGammaRate :: [String] -> String -> String
getGammaRate numbers acc 
    | length (head numbers) == 0 = acc
    | otherwise = result 
    where
        firstDigits = map head numbers
        majority = findMajority firstDigits
        newAcc = acc ++ [majority]
        newNumbers = map (drop 1) $ numbers
        result = getGammaRate newNumbers newAcc

solve1 :: [String] -> Int
solve1 numbers = powerConsumption where
    gammaRateBin = getGammaRate numbers ""
    epsilonRateBin = map invertDigit gammaRateBin
    gammaRate = binToDec gammaRateBin
    epsilonRate = binToDec epsilonRateBin
    powerConsumption = gammaRate * epsilonRate

-- Part 2
getOxygenRating :: [String] -> String -> String
getOxygenRating [elem] acc = acc ++ elem
getOxygenRating numbers acc = result where
    firstDigits = map head numbers
    majority = findMajority firstDigits
    newAcc = acc ++ [majority]
    newNumbers = map (drop 1) $ filter (\x -> head x == majority) numbers
    result = getOxygenRating newNumbers newAcc

getCO2Rating :: [String] -> String -> String
getCO2Rating [elem] acc = acc ++ elem
getCO2Rating numbers acc = result where
    firstDigits = map head numbers
    minority = invertDigit $ findMajority firstDigits
    newAcc = acc ++ [minority]
    newNumbers = map (drop 1) $ filter (\x -> head x == minority) numbers
    result = getCO2Rating newNumbers newAcc

solve2 :: [String] -> Int
solve2 numbers = lifeSupportRating where
    oxygenRating = binToDec $ getOxygenRating numbers ""
    cO2Rating = binToDec $ getCO2Rating numbers ""
    lifeSupportRating = oxygenRating * cO2Rating

main = do
    input <- readFile "day3.txt"
    let numbers = splitOn "\r\n" input
    print $ solve1 $ numbers
    print $ solve2 $ numbers
    