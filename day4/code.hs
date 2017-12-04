import Data.List

allUnique :: (Ord a) => [a] -> Bool
allUnique x = counts == uniques where
    counts = length x
    uniques = length $ map head . group . sort $ x

main = do
    input <- readFile "input.txt"

    let parsed = words <$> lines input

    let part1 = sum $ fromEnum <$> allUnique <$> parsed

    let part2 = sum $ fromEnum <$> allUnique <$> (map . map) sort parsed

    print $ part1
    print $ part2