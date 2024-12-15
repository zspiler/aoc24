module Utils where

import Data.List (sort)
import Data.Bits (shift, (.&.))
import Text.Read (readMaybe)
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

untilStable :: (Eq a) => (a -> a) -> a -> a
untilStable fn = until (\x -> fn x == x) fn

-- Parsing

readInt :: String -> Int
readInt s = read s :: Int

readMaybeInt :: String -> Maybe Int
readMaybeInt s = readMaybe s :: Maybe Int

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

substringIndices :: String -> String -> [Int]
substringIndices str substr = filter (\i -> slice i (i+(length substr - 1)) str == substr) $ take (length str - length substr - 2) (indices str)

-- Tuples

reverseTuple :: (a, b) -> (b, a)
reverseTuple (a, b) = (b, a)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)

unwrapTuple2 :: (Maybe a, Maybe b) -> Maybe (a, b)
unwrapTuple2 (Just a, Just b) = Just (a, b)
unwrapTuple2 _ = Nothing

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- Lists

slice :: Int -> Int -> [a] -> [a]
slice start end s = take (end - start + 1) (drop start s)

enumerated :: [a] -> [(Int, a)]
enumerated = zip [0..]

indices :: [a] -> [Int]
indices s = take (length s) [0..]

removeElementAt :: Int -> [a] -> [a]
removeElementAt removeIndex s = map (s !!) $ filter (/= removeIndex) (indices s)

isSorted :: (Ord a) => [a] -> Bool
isSorted s = s `elem` [sort s, reverse $ sort s]

updateAtIndex :: Int -> a -> [a] -> [a]
updateAtIndex idx newVal xs = take idx xs ++ [newVal] ++ drop (idx + 1) xs

updateAtCoordinate :: (Int, Int) -> a -> [[a]] -> [[a]]
updateAtCoordinate (x, y) newVal xss = updateAtIndex y newRow xss
  where
    newRow = updateAtIndex x newVal (xss !! y)
    
-- 2D lists

areAdjacent :: (Int, Int) -> (Int, Int) -> Bool
areAdjacent (x0, y0) (x1, y1) = abs (x0 - x1) <= 1 && abs (y0 - y1) <= 1

adjacentCoordinates :: (Int, Int) -> [(Int, Int)]
adjacentCoordinates (x, y) = [(x + dx, y + dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]

atCoordinate :: [[a]] -> (Int, Int) -> a
atCoordinate grid (x, y) = grid !! y !! x

maybeAtCoordinate :: [[a]] -> (Int, Int) -> Maybe a
maybeAtCoordinate grid (x,y)
        | isValidCoordinate grid (x,y) = Just (atCoordinate grid (x,y))
        | otherwise = Nothing

isValidCoordinate :: [[a]] -> (Int, Int) -> Bool
isValidCoordinate grid (x,y) = x >= 0 && x < cols && y >= 0 && y < rows
        where rows = length grid
              cols = if rows > 0 then length (head grid) else 0

getCoordinates :: [[a]] -> [(Int, Int)]
getCoordinates grid = [(x, y) | y <- [0..length grid - 1], x <- [0..length (head grid)-1]]

filterCoordinates :: [String] -> [Char] -> [(Int, Int)]
filterCoordinates grid elems = filter (\(x, y) -> atCoordinate grid (x, y) `elem` elems) $ getCoordinates grid

filterCoordinatesBy :: [[x]] -> (x -> Bool) -> [(Int, Int)]
filterCoordinatesBy grid predicate = filter (\(x, y) -> predicate $ atCoordinate grid (x, y)) $ getCoordinates grid

-- 2D lists (bytestrings)

atCoordinateBS :: [B.ByteString] -> (Int, Int) -> Char
atCoordinateBS grid (x, y) = BC.index (grid !! y) x

maybeAtCoordinateBS :: [B.ByteString] -> (Int, Int) -> Maybe Char
maybeAtCoordinateBS grid (x, y)
    | isValidCoordinateBS grid (x, y) = Just (atCoordinateBS grid (x, y))
    | otherwise = Nothing

isValidCoordinateBS :: [B.ByteString] -> (Int, Int) -> Bool
isValidCoordinateBS grid (x, y) = x >= 0 && x < cols && y >= 0 && y < rows
  where
    rows = length grid
    cols = if rows > 0 then BC.length (head grid) else 0

getCoordinatesBS :: [B.ByteString] -> [(Int, Int)]
getCoordinatesBS grid = [(x, y) | y <- [0..length grid - 1], x <- [0..BC.length (head grid) - 1]]

filterCoordinatesBS :: [B.ByteString] -> [Char] -> [(Int, Int)]
filterCoordinatesBS grid elems = filter (\(x, y) -> atCoordinateBS grid (x, y) `elem` elems) $ getCoordinatesBS grid

-- Bits

isBitSet :: Int -> Int -> Bool
isBitSet number position = (number .&. (1 `shift` position)) /= 0

-- Combinatorics

-- https://hackage.haskell.org/package/combinatorial-0.1.1/docs/Combinatorics.html
variateRep :: Int -> [a] -> [[a]]
variateRep n x =
   if n<0 then [] else nest n (\y -> concatMap (\z -> map (z:) y) x) [[]]

-- applies function N times
nest :: Int -> (a -> a) -> a -> a
nest 0 _ x = x
nest n f x = f (nest (n-1) f x)