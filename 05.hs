import System.Environment (getArgs)
import Data.List (all, sortBy, (\\))
import Utils (splitByEmptyLines, splitBy, readInt, tuplify2, reverseTuple)
import qualified Data.Map as Map

type RuleMap = Map.Map Int ([Int], [Int])

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let sections = splitByEmptyLines $ lines contents
    let rules = map (tuplify2 . map readInt . splitBy '|') (head sections)
    let updates = map (map readInt . splitBy ',') (sections !! 1)

    let ruleMap = makeRuleMap rules

    let sortedUpdates = map (sortByRules ruleMap) updates
    let validUpdates = map fst $ filter (uncurry (==)) $ zip updates sortedUpdates
    let sortedInvalidUpdates = map (sortByRules ruleMap) (updates \\ validUpdates)

    print $ sum $ map summarize validUpdates
    print $ sum $ map summarize sortedInvalidUpdates

summarize :: [Int] -> Int
summarize update = update !! div (length update) 2

sortByRules :: RuleMap -> [Int] -> [Int]
sortByRules ruleMap = sortBy (\a b -> comparator a b ruleMap)

comparator :: Int -> Int -> RuleMap -> Ordering
comparator a b ruleMap
    | a `elem` lessThanB = LT
    | a `elem` greaterThanB = GT
    | b `elem` lessThanA = GT
    | b `elem` greaterThanA = LT
    | otherwise = EQ
    where   (lessThanB, greaterThanB) = Map.findWithDefault ([], []) b ruleMap
            (lessThanA, greaterThanA) = Map.findWithDefault ([], []) a ruleMap

makeRuleMap :: [(Int, Int)] -> RuleMap
makeRuleMap rules = Map.unionWith merge lessThan greaterThan
  where
    merge (lt1, gt1) (lt2, gt2) = (lt1 ++ lt2, gt1 ++ gt2)
    greaterThan = Map.map (\v -> ([], v)) $ buildGreaterThanMap rules
    lessThan = Map.map (\v -> (v, [])) $ buildLessThanMap rules
    buildLessThanMap rules = buildGreaterThanMap (map reverseTuple rules)
    buildGreaterThanMap rules = Map.fromListWith (++) (map (\(a,b) -> (a,[b])) rules)