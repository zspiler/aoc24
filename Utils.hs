module Utils where

import Data.Bits (shift, (.&.))

untilStable :: (Eq a) => (a -> a) -> a -> a
untilStable fn = until (\x -> fn x == x) fn


-- Parsing

readInt :: String -> Int
readInt s = read s :: Int

firstAndLast :: [a] -> [a]
firstAndLast [] = []
firstAndLast [x] = [x, x]
firstAndLast x = [head x, last x]

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

splitBy :: Char -> String -> [String]
splitBy delim = filter (not . null) . foldl (cb delim) []
    where cb delim acc c
            | null acc && c /= delim = [[c]]
            | c == delim = acc++[""]
            | otherwise = init acc ++ [last acc ++ [c]]

splitByEmptyLines :: [String] -> [[String]]
splitByEmptyLines = foldl cb []
        where cb acc l
                | null l = acc ++ [[]]
                | null acc = [[l]]
                | otherwise = init acc ++ [last acc ++ [l]]

-- Tuples

reverseTuple :: (a, b) -> (b, a)
reverseTuple (a, b) = (b, a)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- Lists

slice :: Int -> Int -> [a] -> [a]
slice start end s = take (end - start + 1) (drop start s)

enumerated :: [a] -> [(Int, a)]
enumerated = zip [0..]

indices s = take (length s) [0..]

-- coordinates

areAdjacent :: (Int, Int) -> (Int, Int) -> Bool
areAdjacent (x0, y0) (x1, y1) = abs (x0 - x1) <= 1 && abs (y0 - y1) <= 1

adjacentCoordinates :: (Int, Int) -> [(Int, Int)]
adjacentCoordinates (x, y) = [(x + dx, y + dy) | dx <- [-1,0,1], dy <- [-1,0,1]]

getAtCoordinate :: [[a]] -> (Int, Int) -> a
getAtCoordinate grid (x, y) = grid !! y !! x

gridCoordinates :: [[a]] -> [(Int, Int)]
gridCoordinates grid = [(x, y) | y <- [0..length grid - 1], x <- [0..length (head grid)-1]]


-- Bits

isBitSet :: Int -> Int -> Bool
isBitSet number position = (number .&. (1 `shift` position)) /= 0