import Data.Maybe
import Data.List

update :: Int -> a -> [a] -> [a]
update n v xs = take n xs ++ v : drop (n+1) xs

nextCycle :: [Int] -> [Int]
nextCycle xs = let max = maximum xs
                   pos = fromJust $ elemIndex max xs
               in go max (pos+1) $ update pos 0 xs where
                  go max pos mem
                      | max == 0  = mem
                      | otherwise = let p = pos `mod` length mem
                                    in go (max-1) (p+1) $ update p (1 + mem !! p) mem

-- Returns the cycles needed to produce a state that
-- has been seen before, and that state.
realloc :: [Int] -> (Int, [Int])
realloc xs = go 1 [xs] where
             go cycles history = let nextState = nextCycle $ head history
                                 in if elem nextState history
                                    then (cycles, nextState)
                                    else go (cycles+1) $ nextState : history

main = do
    input <- readFile "input.txt"

    let parsed = (read :: String -> Int) <$> words input

    let (cycles, state) = realloc parsed

    print $ cycles
    print $ fst $ realloc state