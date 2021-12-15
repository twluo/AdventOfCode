import qualified Data.HashMap.Lazy as M
import Data.Char
import Data.List
import Data.List.Split

data Rule = 
    Rules [Int] [Int] |
    Literal Char      
    deriving (Show)

getSubRules :: String -> Rule
getSubRules x = let subRules = splitOn " | " x
                    listOfRules = map (splitOn " ") subRules 
                    rule1:rule2 = map (map read) listOfRules in 
                if null rule2 then Rules rule1 [] else Rules rule1 (head rule2)

parseRule :: String -> (Int, Rule)
parseRule rule = ruleTuple where
    (index, _:_:rest) = span isDigit rule
    parsedRule = if or $ map isAlpha rest then Literal (head $ filter isAlpha rest)
                 else getSubRules rest
    ruleTuple = (read index, parsedRule)

getRule :: M.HashMap Int Rule -> Int -> Rule
getRule dict x =     
    case M.lookup x dict of
        Nothing -> Rules [] []
        Just ans -> ans

combineList :: [String] -> [String] -> [String]
combineList x y = [x' ++ y' | x' <- x, y' <- y]

expandRuleHelper :: M.HashMap Int Rule -> [String] -> [Rule] -> [String]
expandRuleHelper dict acc [] = []
expandRuleHelper dict acc [x] = expandRule dict acc x
expandRuleHelper dict acc rules@(x:xs) = 
    let sol = expandRule dict acc x in expandRuleHelper dict sol xs

expandRule :: M.HashMap Int Rule -> [String] -> Rule -> [String]
expandRule dict acc (Literal x) = combineList acc [[x]]
expandRule dict acc (Rules x y) = 
    let xRules = map (getRule dict) x
        yRules = map (getRule dict) y
        xList = expandRuleHelper dict acc xRules
        yList = expandRuleHelper dict acc yRules in
    xList ++ yList

main = do
    str <- readFile "day19.txt"
    let rawRules:rawMessages:_ = splitOn "\r\n\r\n" str
        ruleList = map parseRule (splitOn "\r\n" rawRules)
        dict = M.fromList ruleList
        possibleWords = expandRule dict [""] (Rules [0] [])
        messages = filter (flip (elem) possibleWords) (splitOn ("\r\n") rawMessages)
    print $ length messages