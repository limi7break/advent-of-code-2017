import Data.List.Split
import Data.List

parse :: String -> [(Int, [Int])]
parse input = let x = lines input
                  lhss = read . takeWhile (/= ' ') <$> x
                  rhss = map read . splitOn ", " . drop 2 . dropWhile (/= '>') <$> x
              in zip lhss rhss

retrieve :: [(Int, [Int])] -> Int -> [Int]
retrieve db n = snd . head . filter ((n==) . fst) $ db

rmDups :: Ord a => [a] -> [a]
rmDups = map head . group . sort

next :: [(Int, [Int])] -> ([Int], [Int]) -> ([Int], [Int])
next db (p,q) = (,)
                (rmDups $ p ++ q)
                (rmDups $ (q \\ p) >>= retrieve db)

compute :: [(Int, [Int])] -> Int -> [Int]
compute db start = let rhs = retrieve db start
                   in go db ([start], rhs)
                     where
                       go db acc = let n@(p, q) = next db acc
                                   in if q == []
                                   then p
                                   else go db n

part1 :: String -> Int
part1 input = let x = parse input
                  y = snd . head $ x
              in length $ compute x 0

part2 :: String -> Int
part2 input = let x = parse input
                  y = fst <$> x
              in length . rmDups . map (compute x) $ y

main = do
    input <- readFile "input.txt"

    print $ part1 input
    print $ part2 input