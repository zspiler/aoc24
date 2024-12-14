import System.Environment (getArgs)
import Data.List ((\\) , nub)
import Utils (adjacentCoordinates, isValidCoordinate, atCoordinate, getCoordinates)

type Grid = [String]
type Region = [(Int, Int)]

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let grid = lines contents

    print $ sum $ map (fencePrice grid) $ findRegions grid

fencePrice :: Grid -> Region -> Int
fencePrice grid region = perimeter grid region * length region

perimeter :: Grid -> Region -> Int
perimeter grid region = sum $ map (perimeterOfOne grid) region
  where perimeterOfOne grid (x, y) = length $ filter (`notElem` region) candidates
          where candidates = [(x+1,y), (x-1,y), (x,y-1), (x,y+1)]

findRegions :: Grid -> [Region]
findRegions grid = loop (getCoordinates grid) []
  where
    loop [] regions = regions
    loop (firstCoord:restCoords) regions
      | any (firstCoord `elem`) regions = loop restCoords regions
      | otherwise = loop (restCoords \\ foundRegion) (foundRegion : regions)
                    where foundRegion = floodFill grid firstCoord

floodFill :: Grid -> (Int, Int) -> Region
floodFill grid coord = fst $ until (\(visited, queue) -> null queue) (processQueue grid char) ([], [coord])
  where char = atCoordinate grid coord

processQueue :: Grid -> Char -> ([(Int, Int)], [(Int, Int)]) -> ([(Int, Int)], [(Int, Int)])
processQueue grid char (visited, queue) = (newVisited, (tail queue ++ newQueue) \\ newVisited)
  where (newVisited, newQueue) = visit grid char (head queue) visited

visit :: Grid -> Char -> (Int, Int) -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
visit grid char (x,y) visited = (nub updatedVisited, nub neighborsToVisit)
  where
        updatedVisited = visited ++ [(x,y)]
        neighborsToVisit = validNeighbors \\ updatedVisited
        validNeighbors = filter (\neigh -> isValidCoordinate grid neigh && atCoordinate grid neigh == char) [(x+1,y), (x-1,y), (x,y-1), (x,y+1)]