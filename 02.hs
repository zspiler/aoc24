import System.Environment (getArgs)
import Data.List (sort, any)
import Utils (readInt, indices, removeElementAt, isSorted)

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let sequences = map (map readInt . words) $ lines contents

    print $ length $ filter isSafe sequences
    print $ length $ filter (any isSafe . generateSequences) sequences

isSafe :: [Int] -> Bool
isSafe sequence = isSorted sequence && areDiffsValid (diffs sequence)
    where   diffs sequence = zip sequence $ drop 1 sequence
            areDiffsValid = all (\(a, b) -> abs (a-b) `elem` [1,2,3])

generateSequences :: [Int] -> [[Int]]
generateSequences sequence = sequence : map (`removeElementAt` sequence) (indices sequence)