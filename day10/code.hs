import Data.List.Split
import Data.Char
import Data.Bits
import Numeric

replaceFrom :: Int -> [a] -> [a] -> [a]
replaceFrom pos with xs = take pos xs ++ with ++ drop (pos+length with) xs

f :: Int -> Int -> [Int] -> [Int]
f l cur xs = let s = length xs
                 c = cycle xs
                 x = reverse . take l . drop cur $ c
                 y = replaceFrom cur     x c
                 z = replaceFrom (cur+s) x y
             in take s . drop s $ z

compute :: Int -> Int -> [Int] -> [Int] -> ([Int], (Int, Int))
compute cur ss xs ls = go cur ss xs ls where
    go cur ss acc []     = (acc, (cur, ss))
    go cur ss acc (l:ls) = go ((cur+l+ss) `mod` length acc) (ss+1) (f l cur acc) ls

runRounds :: Int -> [Int] -> [Int]
runRounds n ls = go n 0 0 [0..255] ls where
    go 0 _   _  xs _  = xs
    go n cur ss xs ls = let (ys, (cur_, ss_)) = compute cur ss xs ls
                        in go (n-1) cur_ ss_ ys ls

pad :: Int -> Char -> String -> String
pad l c str
    | l <= length str = str
    | otherwise       = reverse . take l . (++ repeat c) . reverse $ str

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

part1 :: String -> Int
part1 input =   product
              . take 2
              . fst
              . compute 0 0 [0..255]
              . map read
              . splitOn "," $ input

part2 :: String -> String
part2 input =   concat
              . map (pad 2 '0')
              . map (flip showHex "")
              . map (foldr1 xor)
              . chunksOf 16
              . runRounds 64
              . (++ [17,31,73,47,23])
              . map ord
              . rstrip $ input

main = do
    input <- readFile "input.txt"

    print $ part1 input
    print $ part2 input