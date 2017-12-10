import Control.Arrow

times :: Num a => [a]
times = [1,1] ++ map (+1) times

functions :: Num a => [(a,a) -> (a,a)]
functions = cycle [ first (+1),
                    second (+1),
                    first (subtract 1),
                    second (subtract 1) ]

ops :: Num a => [(a,a) -> (a,a)]
ops = concat $ zipWith replicate times functions

location :: Num a => Int -> (a, a)
location n = foldr1 (.) (take (n-1) ops) (0,0)

isNeighborOf :: (Integral a) => (a,a) -> (a,a) -> Bool
isNeighborOf x y = elem y [(a,b) | a <- [fst x-1 .. fst x+1],
                                   b <- [snd x-1 .. snd x+1],
                                   (a,b) /= x]

part1 :: Num a => Int -> a
part1 n = let (x,y) = location n
          in abs x + abs y

part2 :: (Integral a) => a -> a
part2 = go [((0,0), 1)] ops where
    go xs (op:ops) l = let nextPos = op $ fst $ head xs
                           nextValue = sum [snd x | x <- xs,
                                                    fst x `isNeighborOf` nextPos]
                           nextElem = (nextPos, nextValue)
                       in if nextValue > l
                          then nextValue
                          else go (nextElem:xs) ops l

main = do
    let input = 312051
   
    print $ part1 input
    print $ part2 input