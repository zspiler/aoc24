import System.Environment (getArgs)
import Data.Char (ord)
import Data.List (sort)
import Utils (readInt)
import qualified Data.Map as Map

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let split = map words $ lines contents
    let sortedA = sort $ map (readInt . head) split
    let sortedB = sort $ map (readInt . last) split

    -- part1
    print $ sum $ zipWith (\a b -> abs $ a - b) sortedA sortedB

    -- part2
    let counts = getCounts sortedB
    print $ sum $ map (\x -> x * Map.findWithDefault 0 x counts) sortedA

getCounts numbers = Map.fromListWith (+) (map (\x -> (x, 1)) numbers)