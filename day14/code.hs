import qualified Day10 as D
import qualified Data.Sequence as S
import Data.Maybe

type Seq = S.Seq

toBin :: Char -> String
toBin x = case x of
    '0' -> "0000"; '1' -> "0001"; '2' -> "0010"; '3' -> "0011";
    '4' -> "0100"; '5' -> "0101"; '6' -> "0110"; '7' -> "0111";
    '8' -> "1000"; '9' -> "1001"; 'a' -> "1010"; 'b' -> "1011";
    'c' -> "1100"; 'd' -> "1101"; 'e' -> "1110"; 'f' -> "1111";

binHashes :: String -> [String]
binHashes input = (map . concatMap) toBin [D.knotHash $ input ++ "-" ++ show x | x <- [0..127]]

elemAt :: Seq (Seq Char) -> (Int, Int) -> Char
elemAt matrix (x, y)
    | or [x<0,x>127,y<0,y>127] = '0'
    | otherwise                = S.index (S.index matrix x) y

upd :: Seq (Seq a) -> a -> (Int, Int) -> Seq (Seq a)
upd matrix new (x,y) = S.update x (S.update y new $ S.index matrix x) matrix

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

deleteRegion :: Seq (Seq Char) -> (Int, Int) -> Seq (Seq Char)
deleteRegion m (x, y)
    | elemAt m (x, y) /= '1' = m
    | otherwise              = go m $ neighbors (x,y)
                                 where
                                   go m []     = m
                                   go m (z:zs) = go (deleteRegion (upd m '0' (x,y)) z) zs

nextPoint :: (Int, Int) -> (Int, Int)
nextPoint (x, y)
    | y == 127  = (x+1, 0)
    | otherwise = (x, y+1)

scan :: [String] -> Int
scan matrix = go (S.fromList $ map S.fromList matrix) (0,0) 0 where
    go m (x,y) acc
        | and [x==128,y==0] = acc
        | otherwise         = if (==) '1' $ elemAt m (x,y)
                              then go (deleteRegion m (x,y)) (nextPoint (x,y)) (acc+1)
                              else go m (nextPoint (x,y)) acc

part1 :: String -> Int
part1 =   length
        . filter (=='1')
        . concat
        . binHashes

part2 :: String -> Int
part2 =   scan
        . binHashes

main = do
    let input = "wenycdww"

    print $ part1 input
    print $ part2 input