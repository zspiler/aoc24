import System.Environment (getArgs)
import Utils (splitByEmptyLines, filterCoordinates, atCoordinate, updateAtCoordinate)
import Debug.Trace (trace)

type Grid = [String]

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let split = splitByEmptyLines $ lines contents
    let grid = head split
    let moves = concat $ split !! 1

    let updatedGrid = foldl update grid moves
    mapM_ putStrLn updatedGrid
    print $ sum $ map (\(x, y) -> x + y * 100) $ filterCoordinates updatedGrid ['O']

update :: Grid -> Char -> Grid
update grid move = foldl (\grd (coord, newVal) -> updateAtCoordinate coord newVal grd) grid updates
    where
            -- NOTE: order matters 🙈
            updates = newRobotUpdates ++ movableBoxesUpdates ++ movedBoxesUpdates  ++ newRobotPosUpdates
            newRobotUpdates = [(newRobotPos, '@')]
            movedBoxesUpdates = map (, 'O') movedBoxes
            movableBoxesUpdates = filter (\(coord, char) -> newRobotPos /= coord) $ map (, '.') movableBoxes
            newRobotPosUpdates = [(robotPos, '.') | newRobotPos /= robotPos]

            movedBoxes = map (applyMove grid move) movableBoxes
            movableBoxes = movableBoxesInDirection grid move robotPos
            newRobotPos = if (atCoordinate grid posInDirection == '.') || not (null movableBoxes) then posInDirection else robotPos
            robotPos = head $ filterCoordinates grid ['@']
            posInDirection = applyMove grid move robotPos

            height = length grid
            width = length (head grid)

movableBoxesInDirection :: Grid -> Char -> (Int, Int) -> [(Int, Int)]
movableBoxesInDirection grid move robot@(robotX, robotY)
    | move == '^' = if not (null foundBoxes) && atCoordinate grid (robotX, snd (last foundBoxes) - 1) == '.' then foundBoxes else []
    | move == 'v' = if not (null foundBoxes) && atCoordinate grid (robotX, snd (last foundBoxes) + 1) == '.' then foundBoxes else []
    | move == '<' = if not (null foundBoxes) && atCoordinate grid (fst (last foundBoxes) - 1, robotY) == '.' then foundBoxes else []
    | move == '>' = if not (null foundBoxes) && atCoordinate grid (fst (last foundBoxes) + 1, robotY) == '.' then foundBoxes else []
    where
            foundBoxes = boxesInDirection grid move robot
            height = length grid
            width = length $ head grid

boxesInDirection :: Grid -> Char -> (Int, Int) -> [(Int, Int)]
boxesInDirection grid move (robotX, robotY)
    | move == '^' = map (robotX,) $ takeWhile (\y -> atCoordinate grid (robotX, y) == 'O') $ reverse [0..robotY-1]
    | move == 'v' = map (robotX,) $ takeWhile (\y -> atCoordinate grid (robotX, y) == 'O') [robotY+1..height]
    | move == '>' = map (,robotY) $ takeWhile (\x -> atCoordinate grid (x, robotY) == 'O') [robotX+1..width]
    | move == '<' = map (,robotY) $ takeWhile (\x -> atCoordinate grid (x, robotY) == 'O') $ reverse [0..robotX-1]
    where   height = length grid
            width = length $ head grid

applyMove :: Grid -> Char -> (Int, Int) -> (Int, Int)
applyMove grid move (x, y)
    | move == '^' = (x, y-1)
    | move == 'v' = (x, y+1)
    | move == '<' = (x-1, y)
    | move == '>' = (x+1, y)
    | otherwise = error "Invalid move"