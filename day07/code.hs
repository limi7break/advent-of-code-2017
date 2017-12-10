import Data.Ord
import Data.List
import Data.List.Split

data Disc = Disc {
    name    :: String,
    weight  :: Int,
    onTopOf :: [String]
} deriving Show

parseLine :: String -> Disc
parseLine line = let name    = takeWhile (/= ' ') line
                     weight  = read $ takeWhile (/= ')') $ drop 1 $ dropWhile (/= '(') line
                     onTopOf = splitOn ", " $ drop 2 $ dropWhile (/= '>') line
                 in Disc name weight onTopOf

getBaseName :: [Disc] -> String
getBaseName discs = go discs (discs >>= onTopOf) where
    go [] _ = ""
    go (x:xs) ys
        | name x `elem` ys = go xs ys
        | otherwise        = name x

getDiscByName :: [Disc] -> String -> Disc
getDiscByName discs x = head $ filter (\y -> x == name y) discs

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

rep :: Ord a => [a] -> a
rep = head . head . sortBy (flip $ comparing length) . group . sort

correctWeight :: [Disc] -> Disc -> Int
correctWeight discs x
    | onTopOf x == [""] = 0
    | otherwise         = let ds = getDiscByName discs <$> onTopOf x
                              weights = getWeight <$> ds
                          in if not $ allTheSame weights
                             then let a       = rep weights
                                      (w, dw) = head . filter ((/= a) . fst) $ zip weights (weight <$> ds)
                                  in if w > a
                                     then dw + (a - w)
                                     else dw - (a - w)
                             else 0 where
        getWeight (Disc _ w [""]) = w
        getWeight (Disc _ w o)    = (+) w $ sum $ getWeight <$> getDiscByName discs <$> o

part1 :: String -> String
part1 =   getBaseName
        . map parseLine
        . lines

part2 :: String -> Int
part2 input = let discs = map parseLine . lines $ input
              in head
               . filter (/= 0)
               . map (correctWeight discs)
               $ discs

main = do
    input <- readFile "input.txt"

    print $ part1 input
    print $ part2 input