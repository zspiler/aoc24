import System.Environment (getArgs)
import Data.List (any)
import Utils (maybeAtCoordinate, atCoordinate, getCoordinates, isValidCoordinate)

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let grid = lines contents
    let coordinates = getCoordinates grid

    print $ sum $ map (xmasStarts grid) coordinates
    print $ length $ filter (isXmas2 grid) coordinates

xmasStarts :: [String] -> (Int, Int) -> Int
xmasStarts grid coord = length $ filter (isXmas coord grid) $ filter (/=(0,0)) [(x,y) | x <- [-1,0,1], y <- [-1,0,1]]

isXmas :: (Int,Int) -> [String] -> (Int, Int) -> Bool
isXmas (x,y) grid (xStep, yStep) =
    maybeAtCoordinate grid (x,y) == Just 'X'
    && maybeAtCoordinate grid (x+xStep,y+yStep) == Just 'M'
    && maybeAtCoordinate grid (x+2*xStep,y+2*yStep) == Just 'A'
    && maybeAtCoordinate grid (x+3*xStep,y+3*yStep) == Just 'S'

isXmas2 :: [String] -> (Int,Int) -> Bool
isXmas2 grid (x,y) = maybeAtCoordinate grid (x,y) == Just 'A' && masCount == 2
    where
        masCount = length $ filter (\(dx, dy) -> isMas (x+(-1)*dx, y+(-1)*dy) grid (dx, dy)) steps
        steps = [(-1,-1), (1,-1), (1,1), (-1,1)]

isMas :: (Int,Int) -> [String] -> (Int, Int) -> Bool
isMas (x,y) grid (xStep, yStep) =
    maybeAtCoordinate grid (x,y) == Just 'M'
    && maybeAtCoordinate grid (x+xStep,y+yStep) == Just 'A'
    && maybeAtCoordinate grid (x+2*xStep,y+2*yStep) == Just 'S'