import Data.List

allUnique :: (Ord a) => [a] -> Bool
allUnique x = counts == uniques where
    counts = length x
    uniques = length $ map head . group . sort $ x

part1 :: String -> Int
part1 =   sum
        . map fromEnum
        . map allUnique
        . map words
        . lines

part2 :: String -> Int
part2 =   sum
        . map fromEnum
        . map allUnique
        . (map . map) sort
        . map words
        . lines

main = do
    input <- readFile "input.txt"

    print $ part1 input
    print $ part2 input