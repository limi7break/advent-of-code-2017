part1 :: String -> Int
part1 =   sum
        . map (\x -> maximum x - minimum x)
        . (map . map) read
        . map words
        . lines

part2 :: String -> Int
part2 =   sum
        . map (\x -> sum [ a `div` b | a <- x,
                                       b <- x,
                                       a /= b,
                                       a `mod` b == 0 ])
        . (map . map) read
        . map words
        . lines

main = do
    input <- readFile "input.txt"

    print $ part1 input
    print $ part2 input