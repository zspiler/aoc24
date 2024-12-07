import System.Environment (getArgs)
import Data.List (find)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Utils (atCoordinateBS, getCoordinatesBS, maybeAtCoordinate, isValidCoordinateBS, untilStable, filterCoordinatesBS)

type Grid = [B.ByteString]

guardChars = ['^', '>', '<', 'v']

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- B.readFile inputFile

    let grid = BC.lines contents

    let walkedGrid = untilStable walk grid
    mapM_ BC.putStrLn walkedGrid
    
    let visitedCoords = filter (\coord -> atCoordinateBS walkedGrid coord `elem` ('x' : guardChars)) (getCoordinatesBS walkedGrid)
    print $ length visitedCoords

walk :: Grid -> Grid
walk grid = [BC.pack [putEl (x, y) | x <- [0..width-1]] | y <- [0..height-1]]
    where
        putEl (x, y)
            | guardCoord == nextGuardCoord = atCoordinateBS grid (x, y)
            | (x, y) == nextGuardCoord = newGuardDirection guardCoord nextGuardCoord
            | (x, y) == guardCoord = 'x'
            | otherwise = atCoordinateBS grid (x, y)
        guardDirection = atCoordinateBS grid guardCoord
        nextGuardCoord = findNextGuardCoord grid guardCoord
        guardCoord = head $ filterCoordinatesBS grid guardChars
        height = length grid
        width = BC.length (head grid)

newGuardDirection :: (Int, Int) -> (Int, Int) -> Char
newGuardDirection (oldX, oldY) (newX, newY)
    | newX > oldX = '>'
    | newX < oldX = '<'
    | newY < oldY = '^'
    | newY > oldY = 'v'
    | otherwise = error "🐵"

findNextGuardCoord :: Grid -> (Int, Int) -> (Int, Int)
findNextGuardCoord grid guardCoord
    | not (isValidCoordinateBS grid frontCoord) = guardCoord
    | frontEl /= '#' = frontCoord
    | frontEl == '#' && rightEl /= '#' = rightCoord
    | otherwise = guardCoord
    where
        frontEl = atCoordinateBS grid frontCoord
        rightEl = atCoordinateBS grid rightCoord
        frontCoord = moveForward grid guardCoord
        rightCoord = moveRight grid guardCoord

moveForward :: Grid -> (Int, Int) -> (Int, Int)
moveForward grid (guardX, guardY)
    | dir == '^' = (guardX, guardY-1)
    | dir == 'v' = (guardX, guardY+1)
    | dir == '<' = (guardX-1, guardY)
    | dir == '>' = (guardX+1, guardY)
    where dir = atCoordinateBS grid (guardX, guardY)

moveRight :: Grid -> (Int, Int) -> (Int, Int)
moveRight grid (guardX, guardY)
    | dir == '^' = (guardX+1, guardY)
    | dir == 'v' = (guardX-1, guardY)
    | dir == '<' = (guardX, guardY-1)
    | dir == '>' = (guardX, guardY+1)
    where dir = atCoordinateBS grid (guardX, guardY)