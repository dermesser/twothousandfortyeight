module Twothousandfourtyeight where

import Data.Matrix
import Data.List (foldl')

import System.Random

sidelength :: Int
sidelength = 4

-- Empty tile is 0.
type Tile = Int

-- Usually 4x4
type Field = Matrix Tile

data Direction = ToLeft | ToRight | ToUp | ToDown

makeField :: Field
makeField = matrix sidelength sidelength (const 0)

-- We need foldl and foldr because it's essential in which order the list is traversed
shiftField :: Direction -> Field -> Field
shiftField ToUp f = foldl' (flip upshift) f [(r,c) | r <- [2..sidelength], c <- [1..sidelength]]
shiftField ToDown f = foldl' (flip downshift) f [(r,c) | r <- [1..sidelength-1], c <- [1..sidelength]]
shiftField ToLeft f = foldl' (flip leftshift) f [(r,c) | r <- [1..sidelength], c <- [2..sidelength]]
shiftField ToRight f = foldr rightshift f [(r,c) | r <- [1..sidelength], c <- [1..sidelength-1]]

upshift :: (Int,Int) -> Field -> Field
upshift p@(r,c) f | r < 2 || c < 1 = f
                  | (f ! (r-1,c)) == 0 = upshift (r-1,c) $ multipleSet [((r-1,c),f ! p), (p,0)] f
                  | (f ! (r-1,c)) == (f ! p) = multipleSet [((r-1,c),2 * (f ! p)), (p,0)] f
                  | otherwise = f

downshift :: (Int,Int) -> Field -> Field
downshift p@(r,c) f | r > 4 || c < 1 = f
                    | (f ! (r+1,c)) == 0 = downshift (r+1,c) $ multipleSet [((r+1,c),f ! p), (p,0)] f
                    | (f ! (r+1,c)) == (f ! p) = multipleSet [((r+1,c), 2 * (f ! p)), (p,0)] f
                    | otherwise = f

leftshift :: (Int,Int) -> Field -> Field
leftshift p@(r,c) f | r < 1 || c < 2 = f
                    | (f ! (r,c-1)) == 0 = leftshift (r,c-1) $ multipleSet [((r,c-1),f ! p), (p,0)] f
                    | (f ! (r,c-1)) == (f ! p) = multipleSet [((r,c-1),2 * (f ! p)), (p,0)] f
                    | otherwise = f

rightshift :: (Int,Int) -> Field -> Field
rightshift p@(r,c) f | r < 1 || c > 4 = f
                     | (f ! (r,c+1)) == 0 = rightshift (r,c+1) $ multipleSet [((r,c+1),f ! p), (p,0)] f
                     | (f ! (r,c+1)) == (f ! p) = multipleSet [((r,c+1),2 * (f ! p)), (p,0)] f
                     | otherwise = f

-- Random generation of tiles.

powers :: [Tile]
powers = [2,2,2,4,4] -- 3:2 is the two to four tiles ration

getTile :: IO Tile
getTile = do
    i <- randomIO
    return (powers !! abs (i `mod` 5))

getFreePosition :: Field -> IO (Int,Int)
getFreePosition f = do
    i <- randomIO
    if length freepos > 0
    then return (freepos !! abs (i `mod` length freepos))
    else return (0,0)
    where freepos = getZeroes f

placeRandomTile :: Field -> IO Field
placeRandomTile f = do
    pos <- getFreePosition f
    if pos == (0,0)
    then return f
    else do
        tile <- getTile
        return $ setElem tile pos f

-- Utils

-- basically a fold using setElem
multipleSet :: [((Int,Int),Tile)] -> Field -> Field
multipleSet l f = foldr (\(p,v) oldfield -> setElem v p oldfield) f l

getZeroes :: Field -> [(Int,Int)]
getZeroes f = foldr (\p c -> if (f ! p) /= 0 then c else p:c) [] [(r,c) | r <- [1..sidelength], c <- [1..sidelength]]

