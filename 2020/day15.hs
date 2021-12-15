import qualified Data.HashMap.Strict as M


nextTurn :: M.HashMap Int Int -> Int -> Int -> IO(Int)
nextTurn m x 30000000 = do
    print (x)
    return x
nextTurn m x i = do
    case M.lookup x m of
        Nothing -> do 
            let nm = M.insert x i m
            nextTurn nm 0 (i+1)
        Just ans -> do 
            let nm = M.insert x i m
            nextTurn nm (i-ans) (i+1)

main = do
    let a = [0,14,1,3,7,9]::[Int]
        b = [1..(length a)]::[Int]
        c = zip a b
        d = M.fromList c
    nextTurn d 0 ((length a) + 1)