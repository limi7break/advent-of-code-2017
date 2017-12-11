import Data.List.Split
import Data.Char

move :: String -> (Int, Int) -> (Int, Int)
move dir point = case dir of
    "s"  -> (fst point,     snd point + 1)
    "n"  -> (fst point,     snd point - 1)
    "se" -> (fst point + 1, snd point)
    "nw" -> (fst point - 1, snd point)
    "ne" -> (fst point + 1, snd point - 1)
    "sw" -> (fst point - 1, snd point + 1)

distanceFromOrigin :: (Int, Int) -> Int
distanceFromOrigin (q, r) = maximum [abs q, abs (q+r), abs r]

part1 :: String -> Int
part1 =   distanceFromOrigin
        . (\x -> x (0,0))
        . foldl1 (.)
        . map move
        . splitOn ","
        . head
        . lines

part2 :: String -> Int
part2 =   maximum
        . map distanceFromOrigin
        . scanl (\acc f -> f acc) (0,0)
        . map move
        . splitOn ","
        . head
        . lines

main = do
    input <- readFile "input.txt"

    print $ part1 input
    print $ part2 input