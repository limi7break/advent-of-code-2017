import qualified Data.Sequence as S

solveMaze :: [Int] -> (Int -> Int) -> Int
solveMaze maze f = go 0 0 $ S.fromList maze where
    go pos steps mz
        | pos < 0 || pos >= S.length mz = steps
        | otherwise = let el = S.index mz pos
                          newPos = pos + el
                      in go newPos (steps + 1) $ S.update pos (f el) mz

main = do
    input <- readFile "input.txt"

    let parsed = (read :: String -> Int) <$> lines input

    print $ solveMaze parsed (+1)
    print $ solveMaze parsed (\x -> if x >= 3 then x - 1 else x + 1)