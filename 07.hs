import System.Environment (getArgs)
import Utils (splitBy, readInt, isBitSet, variateRep)
import Data.List (any)

data Operation = Add | Mul | Concat deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let parsedLines = map parseLine $ lines contents

    print $ sum $ map fst $ filter (isEquationValid [Add, Mul]) parsedLines
    print $ sum $ map fst $ filter (isEquationValid [Add, Mul, Concat]) parsedLines

isEquationValid :: [Operation] -> (Int, [Int]) -> Bool
isEquationValid ops (val, nums) = val `elem` calculatePossibleExpressions nums ops

calculatePossibleExpressions :: [Int] -> [Operation] -> [Int]
calculatePossibleExpressions nums ops = map calculate $ getPossibleExpressions nums
    where   calculate = foldl (\acc (op, x) -> doOp op acc x) (head nums)
            getPossibleExpressions nums = map (\combo -> zip combo (tail nums)) (variateRep (length nums-1) ops)

doOp :: Operation -> Int -> Int -> Int
doOp Add a b = a + b
doOp Mul a b = a * b
doOp Concat a b = readInt $ show a ++ show b

parseLine :: String -> (Int, [Int])
parseLine ln = (readInt a, map readInt (words b))
    where [a, b] = splitBy ':' ln