import Data.List
import Data.List.Split

clean :: String -> String
clean str = go str [] where
    go (x:y:xs) acc
        | x == '!'  = go xs acc
        | otherwise = go (y:xs) $ acc ++ [x]
    go (x:xs) acc = acc ++ [x]

dropGarbage :: String -> String
dropGarbage str = let x = splitOn "<" str
                      y = concat $ drop 1 . dropWhile (/= '>') <$> x
                  in head x ++ y

score :: String -> Int
score str = go 0 0 str where
    go depth acc ('{':xs) = acc + go (depth+1) 0 xs
    go depth acc ('}':xs) = go (depth-1) (acc+depth) xs
    go depth acc (x:xs)   = go depth acc xs
    go _     acc []       = acc

countGarbage :: String -> Int
countGarbage str = go False 0 str where
    go p acc ('<':xs)
        | p         = go p (acc+1) xs
        | otherwise = go True acc xs
    go _ acc ('>':xs) = go False acc xs
    go p acc (x:xs)   = go p (if p then acc+1 else acc) xs
    go _ acc []       = acc

part1 :: String -> Int
part1 = score . dropGarbage . clean

part2 :: String -> Int
part2 = countGarbage . clean

main = do
    input <- readFile "input.txt"

    print $ part1 input
    print $ part2 input