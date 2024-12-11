import System.Environment (getArgs)
import Data.List(any, findIndex)
import Utils (indices, readInt, updateAtIndex)
import Data.Maybe (fromJust)

type Block = [Int]

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let blocks = expand contents

    print $ checksum $ compact blocks

expand :: String -> [Block]
expand compressed = map (mapFn compressed) (indices compressed)
    where
        mapFn str index
            | even index = replicate n (index `div` 2)
            | otherwise = replicate n (-1)
            where n = readInt [str !! index]

checksum :: [Block] -> Int
checksum blocks = sum $ map (\i -> i * (flattened !! i)) $ indices flattened
    where flattened = concat blocks

compact :: [Block] -> [Block]
compact = until (not . hasEmptyElements) compactingStep
    where hasEmptyElements = any (\block -> (-1) `elem` block)

compactingStep :: [Block] -> [Block]
compactingStep blocks = filter (not. null) $ 
                        updateAtIndex lastBlockIndex updatedLastBlock $
                        updateAtIndex firstEmptyBlockIndex updatedFirstNonFullBlock blocks
  where
    updatedFirstNonFullBlock = fillFirstEmptySpace (blocks !! firstEmptyBlockIndex) $ last (blocks !! lastBlockIndex)
    updatedLastBlock = init (blocks !! lastBlockIndex)
    firstEmptyBlockIndex = fromJust $ findIndex (\block -> (-1) `elem` block) blocks
    lastBlockIndex = length blocks - 1

fillFirstEmptySpace :: Block -> Int -> Block
fillFirstEmptySpace block num = case break (== -1) block of
    (before, _:after) -> before ++ [num] ++ after -- break splits into (before -1, after -1)
    _ -> block