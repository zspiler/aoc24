import System.Environment (getArgs)
import Data.List (intersect, sortBy)
import Utils (splitByEmptyLines, readInt)
import Data.Ord (comparing)

type Machine = ((Int, Int), (Int, Int), (Int, Int))

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile

    let sections = splitByEmptyLines $ lines contents

    print $ sum $ map (minMachinePrice . parseMachine) sections

minMachinePrice :: Machine -> Int
minMachinePrice ((aX, aY), (bX, bY), (prizeX, prizeY)) = if null sortedValidCombos then 0 else comboPrice (head sortedValidCombos)
    where   sortedValidCombos = sortBy (comparing comboPrice) validCombos
            validCombos = filter (\(aPresses, bPresses) -> aPresses * aX + bPresses * bX == prizeX && aPresses * aY + bPresses * bY == prizeY ) combos
            combos = [(aPresses, bPresses) | aPresses <- aXRange `intersect` aYRange, bPresses <- bXRange `intersect` bYRange]
            aXRange = [0..prizeX `div` aX]
            aYRange = [0..prizeY `div` aY]
            bXRange = [0..prizeX `div` bX]
            bYRange = [0..prizeY `div` bY]
            
comboPrice :: (Int, Int) -> Int
comboPrice (aPresses, bPresses) = aPresses * 3 + bPresses
    
parseMachine :: [String] -> Machine
parseMachine [l1, l2, l3] = (parseButton l1, parseButton l2, parsedPrize l3)
    where
        parsedPrize l = (readInt $ drop 2 (split !! 1), readInt $ drop 2 (split !! 2))
            where split = words $ filter (/= ',') l

        parseButton l = (parseMove (split !! 2), parseMove (split !! 3))
            where   parseMove s = readInteger $ drop 1 s
                    split = words $ filter (/= ',') l

readInteger :: String -> Int
readInteger s = if head s == '+' then readInt (tail s) else readInt s
