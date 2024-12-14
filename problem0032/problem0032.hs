import Data.Char (digitToInt)
import Data.Maybe (isJust, mapMaybe)
import Data.List (nub)

allDigits = ['1', '2', '3', '4', '5', '6', '7', '8', '9']

combinations :: [Char] -> [[Char]]
combinations [c] = [[c]]
combinations digits
        =  concatMap (\c -> map (c:) (combinations $ subsequence c)) startingDigits
    where n                 = length digits
          subsequence c     = filter (/=c) digits
          startingDigits    = take n $ map (digits !!) [0..]

-- 

operators :: Int -> [(Int, Int)]
operators n = concatMap (\m -> map (m,) (operators' m)) (takeWhile (<n) [1..])
    where operators' i = takeWhile (<n) $ map (+i) [1..]

calculate :: Int -> Int -> [Char] -> Maybe Int
calculate ix_x ix_e digits
        | read @Int first * read @Int seconds == actualResult = Just actualResult
        | otherwise                                           = Nothing
    where (first, rest)     = splitAt ix_x digits
          (seconds, result) = splitAt (ix_e - ix_x) rest
          actualResult = read @Int result





main = do
    let allCombinations = combinations allDigits
    let allOperators = operators (length allDigits)
    let answer = sum $ nub $ concatMap (\(x, y) -> mapMaybe (calculate x y) allCombinations) allOperators
    print [answer, answer - 45228]