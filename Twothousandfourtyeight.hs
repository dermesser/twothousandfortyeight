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
shiftField ToUp f = absoluteMap $ foldl' (moveTile ToUp) f [(r,c) | r <- [2..sidelength], c <- [1..sidelength]]
shiftField ToDown f = absoluteMap $ foldr (flip $ moveTile ToDown) f [(r,c) | r <- [1..sidelength-1], c <- [1..sidelength]]
shiftField ToLeft f = absoluteMap $ foldl' (moveTile ToLeft) f [(r,c) | r <- [1..sidelength], c <- [2..sidelength]]
shiftField ToRight f = absoluteMap $ foldr (flip $ moveTile ToRight) f [(r,c) | r <- [1..sidelength], c <- [1..sidelength-1]]

moveTile :: Direction -> Field -> (Int,Int) -> Field
moveTile d f p@(r,c) = case f ! p of
                            0 -> f
                            v | not (isVoidAddress nextfield) -> case f ! nextfield of
                                    0 -> moveTile d (multipleSet [(nextfield,f ! p),(p,0)] f) nextfield
                                    nextv | nextv == v && not (isVoidAddress nextfield) -> multipleSet [(nextfield,-2 * (f ! p)),(p,0)] f
                                          | otherwise -> f
                              | otherwise -> f
    where nextfield = advanceField d p

advanceField :: Direction -> (Int,Int) -> (Int,Int)
advanceField ToUp (r,c) = (r-1,c)
advanceField ToDown (r,c) = (r+1,c)
advanceField ToLeft (r,c) = (r,c-1)
advanceField ToRight (r,c) = (r,c+1)


isVoidAddress :: (Int,Int) -> Bool
isVoidAddress (r,c) = r < 1 || c < 1 || r > sidelength || c > sidelength

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

matrixMap :: (a -> a) -> Matrix a -> Matrix a
matrixMap f m = foldr (\p mat -> setElem (f (mat ! p)) p mat) m [(r,c) | r <- [1..nrows m], c <- [1..ncols m]]

absoluteMap :: Field -> Field
absoluteMap = matrixMap abs

