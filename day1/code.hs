import Data.Char

main = do
    input <- readFile "input.txt"
    
    print $ sum $ subtract 48 <$> ord <$> fst <$> [t | t <- zip input (tail input ++ [head input]), fst t == snd t]
    
    let (one, two) = splitAt (length input `div` 2) input
    print $ sum $ subtract 48 <$> ord <$> fst <$> [t | t <- zip input (two ++ one), fst t == snd t]