import Data.Bits

opA, opB :: Int -> Int
opA = flip mod 2147483647 . (*) 16807
opB = flip mod 2147483647 . (*) 48271

parse :: String -> [Int]
parse =   map read
        . map (drop 24)
        . lines

lowest16bitsMatch :: Int -> Int -> Bool
lowest16bitsMatch x y = (x .&. 65535) == (y .&. 65535)

part1 :: String -> Int
part1 input = let [seedA, seedB] = parse input
                  a = drop 1 $ iterate opA seedA
                  b = drop 1 $ iterate opB seedB
              in length . filter (==True) . take 40000000 $ zipWith lowest16bitsMatch a b

part2 :: String -> Int
part2 input = let [seedA, seedB] = parse input
                  a = filter ((==) 0 . flip mod 4) . drop 1 $ iterate opA seedA
                  b = filter ((==) 0 . flip mod 8) . drop 1 $ iterate opB seedB
              in length . filter (==True) . take 5000000 $ zipWith lowest16bitsMatch a b

main = do
    input <- readFile "input.txt"

    print $ part1 input
    print $ part2 input