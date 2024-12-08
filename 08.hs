import System.Environment (getArgs)
import Utils (filterCoordinatesBy, atCoordinate, isValidCoordinate)
import Data.List (any, (\\), tails, nub)
import qualified Data.Map as Map

type FindAntidotes = (Int, Int) -> (Int, Int) -> [String] -> [(Int, Int)]
type CoordsByAntenna = Map.Map Char [(Int, Int)]

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let grid = lines contents
    
    let coordsByAntenna = findCoordsByAntenna grid

    let antinodesByAntenna1 = Map.map (findAllAntinodes findAntinodes grid) coordsByAntenna
    let antinodesByAntenna2 = Map.map (findAllAntinodes findAntinodes2 grid) coordsByAntenna

    print $ length $ nub $ concat (Map.elems antinodesByAntenna1)
    print $ length $ nub $ concat (Map.elems antinodesByAntenna2)

findCoordsByAntenna :: [String] -> CoordsByAntenna
findCoordsByAntenna grid = Map.fromListWith (++) (map (\coord -> (atCoordinate grid coord, [coord])) antennaCoords)
    where antennaCoords = filterCoordinatesBy grid (/='.')

findAllAntinodes :: FindAntidotes -> [String] -> [(Int, Int)] -> [(Int, Int)]
findAllAntinodes findAntidotesFn grid coords = concatMap (filter (isValidCoordinate grid) . (\(coord1, coord2) -> findAntidotesFn coord1 coord2 grid)) (makeCombinations coords)

makeCombinations :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
makeCombinations coords = [(coord1, coord2) | coord1 <- coords, coord2 <- coords, coord1 /= coord2, coord1 < coord2]

findAntinodes :: FindAntidotes
findAntinodes (x1, y1) (x2, y2) grid = filter (isValidCoordinate grid) [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]
  where
    dx = x2 - x1
    dy = y2 - y1

findAntinodes2 :: FindAntidotes
findAntinodes2 (x1, y1) (x2, y2) grid = nub $ filter (isValidCoordinate grid) (forwards ++ backwards ++ [(x1, y1), (x2, y2)])
  where
    forwards = takeWhile (isValidCoordinate grid) $ map (\i -> (x1 + i*dx, y1 + i*dy)) [1..]
    backwards = takeWhile (isValidCoordinate grid) $ map (\i -> (x1 - i*dx, y1 - i*dy)) [1..]
    dx = x2 - x1
    dy = y2 - y1