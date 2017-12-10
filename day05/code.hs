import qualified Data.Sequence as S

solveMaze :: (Int -> Int) -> [Int] -> Int
solveMaze f maze = go 0 0 $ S.fromList maze where
    go pos steps mz
        | pos < 0 || pos >= S.length mz = steps
        | otherwise = let el = S.index mz pos
                          newPos = pos + el
                      in go newPos (steps + 1) $ S.update pos (f el) mz

part1 :: String -> Int
part1 =   solveMaze (+1)
        . map read
        . lines

part2 :: String -> Int
part2 =   solveMaze (\x -> if x >= 3 then x - 1 else x + 1)
        . map read
        . lines

main = do
    input <- readFile "input.txt"

    print $ part1 input
    print $ part2 input