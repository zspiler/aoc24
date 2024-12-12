import System.Environment (getArgs)
import Utils (isValidCoordinate, atCoordinate, readInt, filterCoordinatesBy)
import Data.List ( (\\), nub, all)

type Grid = [[Int]]
type Path = [(Int, Int)]

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let grid = map (map (\c -> readInt [c])) $ lines contents
    let allStarts = filterCoordinatesBy grid (==0)
    let allEnds = filterCoordinatesBy grid (==9)
    print $ sum $ map (\start -> trailHeadScore grid start allEnds) allStarts

trailHeadScore :: Grid -> (Int, Int) -> [(Int, Int)] -> Int
trailHeadScore grid (x, y) ends = length $ nub $ filter (`elem` ends) lastNodesInPaths
    where
        lastNodesInPaths = map last allPaths
        allPaths = nub $ dfs grid (x,y) []

dfs :: Grid -> (Int, Int) -> [Path] -> [Path]
dfs grid (x, y) paths
    | atCoordinate grid (x, y) == 9 = updatedPaths
    | null nonVisitedValidNeighbors = updatedPaths
    | otherwise = concatMap (\neigh -> dfs grid neigh updatedPaths) nonVisitedValidNeighbors
    where
        updatedPaths = safeInit paths ++ [safeLast paths ++ [(x, y)]]
        nonVisitedValidNeighbors = validNeighbors \\ safeLast paths
        validNeighbors = filter (isIncline grid (x, y)) $ filter (isValidCoordinate grid) $  allNeighbors
        allNeighbors = [(x+1,y), (x-1,y), (x,y-1), (x,y+1)]

isIncline :: Grid -> (Int, Int) -> (Int, Int) -> Bool
isIncline grid (x1, y1) (x2, y2) = atCoordinate grid (x2, y2) == atCoordinate grid (x1, y1) + 1

safeLast :: [Path] -> Path
safeLast [] = []
safeLast paths = last paths

safeInit :: [Path] -> [Path]
safeInit [] = []
safeInit paths = init paths