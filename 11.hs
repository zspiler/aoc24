import System.Environment (getArgs)
import Utils (readInt)

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let stones = map readInt $ words contents

    print $ length $ iterate (concatMap changeStone) stones !! 25

changeStone :: Int -> [Int]
changeStone stone
    | stone == 0 = [1]
    | hasEvenNumOfDigits stone = splitNum stone
    | otherwise = [stone * 2024]

hasEvenNumOfDigits :: Int -> Bool
hasEvenNumOfDigits num = even $ length (show num)

splitNum :: Int -> [Int]
splitNum num = map readInt [take middleIndex str, drop middleIndex str]
    where   
            middleIndex = len `div` 2
            len = length str
            str = show num