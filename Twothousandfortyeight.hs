module Twothousandfortyeight where

import Data.Matrix
import Data.List (foldl',maximumBy)

sidelength :: Int
sidelength = 4

-- Empty tile is 0.
type Tile = Int

-- Usually 4x4
type Field = Matrix Tile

data Direction = ToLeft | ToRight | ToUp | ToDown deriving Show

makeField :: Field
makeField = matrix sidelength sidelength (const 0)

-- We need foldl and foldr because it's essential in which order the list is traversed
shiftField :: Direction -> Field -> Field
shiftField ToUp f = absoluteMap $ foldl' (moveTile ToUp) f [(r,c) | r <- [2..sidelength], c <- [1..sidelength]]
shiftField ToDown f = absoluteMap $ foldr (flip $ moveTile ToDown) f [(r,c) | r <- [1..sidelength-1], c <- [1..sidelength]]
shiftField ToLeft f = absoluteMap $ foldl' (moveTile ToLeft) f [(r,c) | r <- [1..sidelength], c <- [2..sidelength]]
shiftField ToRight f = absoluteMap $ foldr (flip $ moveTile ToRight) f [(r,c) | r <- [1..sidelength], c <- [1..sidelength-1]]


moveTile :: Direction -> Field -> (Int,Int) -> Field
moveTile d f p = case f ! p of
                            0 -> f
                            v | not (isVoidAddress nextfield) -> case f ! nextfield of
                                    0 -> moveTile d (multipleSet [(nextfield,f ! p),(p,0)] f) nextfield
                                    nextv | nextv == v -> multipleSet [(nextfield,-2 * (f ! p)),(p,0)] f -- a merged tile is marked by setting the value (*-1) so it won't be automatically merged again
                                          | otherwise -> f
                              | otherwise -> f
    where nextfield = advanceField d p

advanceField :: Direction -> (Int,Int) -> (Int,Int)
advanceField ToUp (r,c) = (r-1,c)
advanceField ToDown (r,c) = (r+1,c)
advanceField ToLeft (r,c) = (r,c-1)
advanceField ToRight (r,c) = (r,c+1)


-- Solving algorithm

fieldQuality :: Field -> Int
fieldQuality f = (1000 * fieldsum) `div` filledtiles
    where nfields = sidelength * sidelength
          nzerotiles = length $ getTilesWith 0 f
          filledtiles = nfields - nzerotiles
          fieldsum = matrixFold (+) 0 f

bestAction :: Field -> Direction
bestAction f = fst maxaction
    where maxaction = maximumBy compfunc scores
          scores = map (\d -> (d,fieldQuality (shiftField d f))) [ToUp,ToDown,ToRight,ToLeft]
          compfunc (_,q1) (_,q2) = q1 `compare` q2

-- Utils, either specific or generic for matrices

-- basically a fold using setElem
multipleSet :: [((Int,Int),Tile)] -> Field -> Field
multipleSet l f = foldr (\(p,v) oldfield -> setElem v p oldfield) f l

getTilesWith :: Tile -> Field -> [(Int,Int)]
getTilesWith v f = foldr (\p c -> if (f ! p) /= v then c else p:c) [] [(r,c) | r <- [1..nrows f], c <- [1..ncols f]]

isVoidAddress :: (Int,Int) -> Bool
isVoidAddress (r,c) = r < 1 || c < 1 || r > sidelength || c > sidelength

matrixMap :: (a -> a) -> Matrix a -> Matrix a
matrixMap f m = foldr (\p mat -> setElem (f (mat ! p)) p mat) m [(r,c) | r <- [1..nrows m], c <- [1..ncols m]]

matrixFold :: (a -> b -> a) -> a -> Matrix b -> a
matrixFold f i m = foldr (\p acc -> f acc (m ! p)) i [(r,c) | r <- [1..nrows m], c <- [1..ncols m]]

absoluteMap :: Field -> Field
absoluteMap = matrixMap abs

