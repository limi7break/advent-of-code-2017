import Data.Maybe
import qualified Data.Map.Strict as Map

type Map k a = Map.Map k a

fromOpcode :: Num a => String -> (a -> a -> a)
fromOpcode opc = case opc of
    "inc" -> (+)
    "dec" -> (-)

fromOperator :: (Ord a, Eq a) => String -> (a -> a -> Bool)
fromOperator cmp = case cmp of
    ">" ->  (>)
    "<" ->  (<)
    ">=" -> (>=)
    "<=" -> (<=)
    "!=" -> (/=)
    "==" -> (==)

run :: Map String Int -> String -> Map String Int
run rs line
    | not $ fromOperator cmp (fromMaybe 0 $ Map.lookup r rs) $ read v = rs
    | otherwise = Map.insert reg (fromOpcode opc (fromMaybe 0 $ Map.lookup reg rs) $ read val) rs
    
    where [reg, opc, val, _, r, cmp, v] = words line

main = do
    input <- readFile "input.txt"

    print $ maximum . Map.elems . foldl run Map.empty $ lines input
    print $ maximum . concatMap Map.elems . scanl run Map.empty $ lines input