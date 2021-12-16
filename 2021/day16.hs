import Data.Char
import Data.List.Split

-- Helper Functions
data Packet = 
    Literal Int Int Int |
    Operator Int Int [Packet]
    deriving (Show, Eq)
    
hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

processInput :: String -> Packet
processInput input = fst $ parseOnePacket (concat $ hexToBin <$> input)

parseOnePacket :: String -> (Packet, String)
parseOnePacket (v1:v2:v3:'1':'0':'0':rest) = parseLiteral rest (toDec [v1, v2, v3]) 4
parseOnePacket (v1:v2:v3:t1:t2:t3:rest) = parseOperator rest (toDec [v1, v2, v3]) (toDec [t1, t2, t3])

parsePacket :: String -> [Packet] -> ([Packet], String)
parsePacket x acc
    | (length $ filter (/= '0') x) == 0 =  (acc, x)
parsePacket bits acc = parsePacket newRest (acc ++ [packet])
    where
        (packet, newRest) = parseOnePacket bits

parseLiteral :: String -> Int -> Int -> (Packet, String)
parseLiteral bits version typeID= (Literal version typeID value, rest)
    where
        (value, rest) = parseValue bits 6 ""

parseValue :: String -> Int -> String -> (Int, String)
parseValue ('0':rest) numOfBits acc = (toDec value, newRest)
    where
        value = acc ++ (take 4 rest)
        newRest = drop 4 rest
parseValue ('1':rest) numOfBits acc = parseValue (drop 4 rest) (numOfBits + 5) (acc ++ (take 4 rest))

parseOperator :: String -> Int -> Int -> (Packet, String)
parseOperator ('0':bits) version typeID = ((Operator version typeID packets), (rest ++ (drop (length + 15) bits)))
    where
        length = toDec $ take 15 bits
        (packets, rest) = parsePacket (take length (drop 15 bits)) []
parseOperator ('1':bits) version typeID = ((Operator version typeID packets), rest)
    where
        parsePacketAcc :: String -> Int -> [Packet] -> ([Packet], String)
        parsePacketAcc rest 0 acc = (acc, rest)
        parsePacketAcc bits gen acc = parsePacketAcc rest (gen - 1) (acc ++ [packets])
            where
                (packets, rest) = parseOnePacket bits
        length = toDec $ take 11 bits
        (packets, rest) = parsePacketAcc (drop 11 bits) length []

-- Part 1
sumVersions :: [Packet] -> Int -> Int
sumVersions [] acc = acc
sumVersions (packet:rest) acc =
    case packet of
        Literal version _ _ -> sumVersions rest (acc + version)
        Operator version _ packets -> sumVersions rest (acc + version + sumVersions packets 0)

solve1 :: Packet -> Int
solve1 packet = sumVersions [packet] 0

-- Part 2
eval :: Packet -> Int
eval packet =
    case packet of
        Literal _ _ x -> x
        Operator _ 0 packets -> sum $ eval <$> packets
        Operator _ 1 packets -> product $ eval <$> packets
        Operator _ 2 packets -> minimum $ eval <$> packets
        Operator _ 3 packets -> maximum $ eval <$> packets
        Operator _ 5 (packet1:packet2:_) -> if (eval packet1) > (eval packet2) then 1 else 0
        Operator _ 6 (packet1:packet2:_) -> if (eval packet1) < (eval packet2) then 1 else 0
        Operator _ 7 (packet1:packet2:_) -> if (eval packet1) == (eval packet2) then 1 else 0

solve2 :: Packet -> Int
solve2 packet = eval packet

main = do
    input <- readFile "day16.txt"
    let packet = processInput input
    print $ solve1 packet
    print $ solve2 packet
