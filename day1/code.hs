import Data.Char

part1 :: String -> Int
part1 input =   sum
              . map (subtract 48)
              . map ord
              . map fst
              $ [t | t <- zip input (tail input ++ [head input]), fst t == snd t]

part2 :: String -> Int
part2 input = let (one, two) = splitAt (length input `div` 2) input
              in sum
               . map (subtract 48)
               . map ord
               . map fst
               $ [t | t <- zip input (two ++ one), fst t == snd t]

main = do
    input <- readFile "input.txt"
    
    print $ part1 input
    print $ part2 input