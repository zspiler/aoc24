import System.Environment (getArgs)
import Data.List (groupBy)
import qualified Data.Map as Map
import Utils (splitBy, readInt, tuplify2)

type Position = (Int, Int)
type Velocity = (Int, Int)
type Robot = (Position, Velocity)

data Quadrant = TopLeft | TopRight | BottomLeft | BottomRight | None deriving (Show, Eq, Ord)

width = 101
height = 103

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let lns = lines contents

    let robots = map parseRobot lns
    let movedRobots = iterate (map (moveRobot width height)) robots !! 100
    let robotsPerQuadrant = countRobotsPerQuadrant movedRobots

    print $ product $ Map.elems robotsPerQuadrant

countRobotsPerQuadrant :: [Robot] -> Map.Map Quadrant Int
countRobotsPerQuadrant robots = Map.filterWithKey (\k _ -> k /= None) $ Map.fromListWith (+) $ map (\robot -> (determineQuadrant width height robot, 1 :: Int)) robots 

determineQuadrant :: Int -> Int -> Robot -> Quadrant
determineQuadrant width height ((x, y), _)
    | x < width `div` 2 && y < height `div` 2 = TopLeft
    | x > width `div` 2 && y < height `div` 2 = TopRight
    | x < width `div` 2 && y > height `div` 2 = BottomLeft
    | x > width `div` 2 && y > height `div` 2 = BottomRight
    | otherwise = None

moveRobot :: Int -> Int -> Robot -> Robot
moveRobot width height robot@((posX, posY), (velX, velY)) = (((posX + velX) `mod` width, (posY + velY) `mod` height), snd robot)

parseRobot :: String -> Robot
parseRobot ln = tuplify2 $ map parseTuple $ words ln
    where parseTuple t = tuplify2 $ map readInt $ splitBy ',' $ drop 2 t
