import System.Environment (getArgs)
import Utils (indices, slice, splitBy, tuplify2, substringIndices, readMaybeInt, mapTuple, unwrapTuple2)
import Data.List (elemIndex, elemIndices, find)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let str = concat $ lines contents

    print $ sum $ map (uncurry (*)) $ scanMuls True str
    print $ sum $ map (uncurry (*)) $ scanMuls False str
    

-- (o      _      o)

scanMuls :: Bool -> String -> [(Int, Int)]
scanMuls allowAll s = mapMaybe (unwrapTuple2 . mapTuple readMaybeInt) tuples
    where
        tuples = map (tuplify2 . splitBy ',') $ filter (\str -> length (elemIndices ',' str) == 1) splitted
        splitted = map (\(a, Just b) -> slice (a+4) (b-1) s) (filter statementFilter mulsWithClosingBrackets)
        statementFilter = if allowAll then const True else \(i,_) -> isAllowed s i doos donts
        mulsWithClosingBrackets = filter (\(_, b) -> isJust b) $ map (\muli -> (muli, find (> muli) closingBrackets)) muls
        closingBrackets = elemIndices ')' s
        muls = filter (\i -> slice i (i+3) s == "mul(") $ take (length s - 2) (indices s)
        doos = substringIndices s "do()"
        donts = substringIndices s "don't()"

isAllowed :: String -> Int -> [Int] -> [Int] -> Bool
isAllowed str i doos donts
    | isJust closestDo && isJust closestDont = closestDo > closestDont
    | isJust closestDo = True
    | isJust closestDont = False
    | otherwise = True
    where
        closestDo = find  (<i) (reverse doos)
        closestDont = find (<i) (reverse donts)
