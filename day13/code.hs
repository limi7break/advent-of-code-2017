import Data.List.Split

parse :: String -> [(Int, Int)]
parse input =     (\xs -> (read $ head xs, read $ last xs))
              <$> splitOn ": "
              <$> lines input

scanner :: Int -> Int -> Int
scanner d l = d `mod` (2 * l - 2)

part1 :: String -> Int
part1 =   sum
        . map (uncurry (*))
        . filter (\(x,y) -> 0 == scanner x y)
        . parse

part2 :: String -> Int
part2 input = let x = parse input
              in go x 0
                where
                  go x n = let caught = filter (\(x,y) -> 0 == scanner (x+n) y) x
                           in if length caught == 0
                              then n
                              else go x $ n + 1

main = do
    input <- readFile "input.txt"

    print $ part1 input
    print $ part2 input