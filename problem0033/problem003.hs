import Data.Map (mapMaybe)
import Data.Maybe (fromJust)
import Data.Char (digitToInt)

allDigits :: [Char]
allDigits = ['1', '2', '3', '4', '5', '6', '7', '8', '9']

nonTrivialNumbers :: [(Char, Char)]
nonTrivialNumbers = concatMap (filter (uncurry (/=)) . (\d -> map (d,) allDigits)) allDigits

dumbFraction :: (Char, Char) -> (Char, Char) -> Maybe ((Char, Char), (Char, Char), Int, Int, Float)
dumbFraction (a, b) (x, y)
        | fraction >= 1         = Nothing
        | b == y && ia / ix == fraction   = Just ((a, b), (x, y), digitToInt a, digitToInt x, fraction)
        | b == x && ia / iy == fraction   = Just ((a, b), (x, y), digitToInt a, digitToInt y, fraction)
        | a == y && ib / ix == fraction   = Just ((a, b), (x, y), digitToInt b, digitToInt x, fraction)
        | a == x && ib / iy == fraction   = Just ((a, b), (x, y), digitToInt b, digitToInt y, fraction)
        | otherwise             = Nothing
    where ia = read @Float [a]
          ib = read @Float [b]
          ix = read @Float [x]
          iy = read @Float [y]
          fraction = (ia * 10 + ib) / (ix * 10 + iy)

allDumbFractions :: [((Char, Char), (Char, Char), Int, Int, Float)]
allDumbFractions = map fromJust $ concatMap (\a -> filter (/=Nothing) $ map (dumbFraction a) nonTrivialNumbers) nonTrivialNumbers

getDeNumerators :: ([Int], [Int])
getDeNumerators = unzip $ map (\(_, _, a, b, _) -> (a, b)) allDumbFractions

main = do   
    let (numerators, denumerators) = getDeNumerators
    let answer = product denumerators `div` product numerators

    print [answer, answer - 100]