import Data.Char
import Data.List
import Data.List.Split

data Token = 
    Number Int  |
    AddOp       |
    MultOp      |
    Open        |
    Close       |
    Done
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [Done]
tokenize str@(x:xs) 
    | isDigit x  = let (num, rest) = span isDigit str in Number (read num):(tokenize rest) 
    | (x == '+') = AddOp:(tokenize xs)
    | (x == '*') = MultOp:(tokenize xs)
    | (x == ')') = Open:(tokenize xs)
    | (x == '(') = Close:(tokenize xs)
    | otherwise  = tokenize(xs)

data Expr =
    Add Expr Expr   |
    Mult Expr Expr  |
    Num Int         |
    None
    deriving (Show, Eq) 

parseExpression :: [Token] -> (Expr, [Token])
parseExpression tokens@(token:rest) =
    let unit@(expr, rest') = parsePrecendence tokens in
        case head rest' of
            MultOp -> let (nextExpr, rest'')  = parseExpression $ tail rest' in
                        (Mult expr nextExpr, rest'')                        
            _      -> unit

parsePrecendence :: [Token] -> (Expr, [Token])
parsePrecendence tokens@(token:rest) =
    let unit@(expr, rest') = parseUnit tokens in
        case head rest' of
            AddOp  -> let (nextExpr, rest'')  = parsePrecendence $ tail rest' in
                        (Add expr nextExpr, rest'')                     
            _      -> unit

parseUnit :: [Token] -> (Expr, [Token])
parseUnit tokens@(token:rest) = 
    case token of
        Number token -> (Num token, rest)
        Open         -> let (expr, rest') = parseExpression rest in
                            if head rest' /= Close
                            then error ":( Parenthesis no match"
                            else (expr, tail rest')
        _ -> error $ (show tokens)

parse :: [Token] -> Expr
parse tokens = let (expr, _) = parseExpression tokens in expr

eval :: Expr -> Int
eval (Num x) = x
eval (Add x y) = eval x + eval y
eval (Mult x y) = eval x * eval y 

evaluate :: String -> Int
evaluate x = eval $ parse $ tokenize $ reverse x


main = do
    str <- readFile "day18.txt"
    let inputs = splitOn "\r\n" str
        results = map evaluate inputs
    print $ sum $ results
