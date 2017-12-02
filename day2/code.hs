main = do
    input <- readFile "input.txt"

    let parsed = (map . map) (read :: String -> Int) $ words <$> lines input

    print $ sum $ (\x -> maximum x - minimum x) <$> parsed

    print $ sum $ (\x -> sum [ a `div` b | a <- x,
                                           b <- x,
                                           a /= b,
                                           a `mod` b == 0 ]) <$> parsed