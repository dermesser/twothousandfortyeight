module Twothousandfourtyeight where

import Data.Matrix
import Data.List (foldl')

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
shiftField ToUp f = foldr upshift f [(r,c) | r <- [2..sidelength], c <- [1..sidelength]]
shiftField ToDown f = foldl' (flip downshift) f [(r,c) | r <- [1..sidelength-1], c <- [1..sidelength]]
shiftField ToLeft f = foldr leftshift f [(r,c) | r <- [1..sidelength], c <- [2..sidelength]]
shiftField ToRight f = foldl' (flip rightshift) f [(r,c) | r <- [1..sidelength], c <- [1..sidelength-1]]

upshift :: (Int,Int) -> Field -> Field
upshift p@(r,c) f | (f ! (r-1,c)) == 0 = multipleSet [((r-1,c),f ! p), (p,0)] f
                  | (f ! (r-1,c)) == (f ! p) = multipleSet [((r-1,c),2 * (f ! p)), (p,0)] f
                  | otherwise = f

downshift :: (Int,Int) -> Field -> Field
downshift p@(r,c) f | (f ! (r+1,c)) == 0 = multipleSet [((r+1,c),f ! p), (p,0)] f
                    | (f ! (r+1,c)) == (f ! p) = multipleSet [((r+1,c), 2 * (f ! p)), (p,0)] f
                    | otherwise = f

leftshift :: (Int,Int) -> Field -> Field
leftshift p@(r,c) f | (f ! (r,c-1)) == 0 = multipleSet [((r,c-1),f ! p), (p,0)] f
                    | (f ! (r,c-1)) == (f ! p) = multipleSet [((r,c-1),2 * (f ! p)), (p,0)] f
                    | otherwise = f

rightshift :: (Int,Int) -> Field -> Field
rightshift p@(r,c) f | (f ! (r,c+1)) == 0 = multipleSet [((r,c+1),f ! p), (p,0)] f
                     | (f ! (r,c+1)) == (f ! p) = multipleSet [((r,c+1),2 * (f ! p)), (p,0)] f
                     | otherwise = f

-- Utils

-- basically a fold using setElem
multipleSet :: [((Int,Int),Tile)] -> Field -> Field
multipleSet l f = foldr (\(p,v) oldfield -> setElem v p oldfield) f l

