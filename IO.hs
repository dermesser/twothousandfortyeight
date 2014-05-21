module IO where

import qualified Data.Vector as V

import Data.Maybe (fromMaybe)

import Data.Matrix
import Twothousandfortyeight

import System.Console.ANSI
import System.Random
-- Gameplay-related code

droppedTiles :: [Tile]
droppedTiles = [2,2,2,4,4] -- Which tiles are dropped. Elements are randomly chosen, i.e. the ratio is 3:2 [2]:[4] tiles.

getTile :: IO Tile
getTile = do
    i <- randomIO
    return (droppedTiles !! abs (i `mod` 5))

getFreePosition :: Field -> IO (Int,Int)
getFreePosition f = do
    i <- randomIO
    return (if not $ null freepos
            then freepos !! abs (i `mod` length freepos)
            else (0,0))
    where freepos = getTilesWith 0 f

placeRandomTile :: Field -> IO (Maybe Field)
placeRandomTile f = do
    pos <- getFreePosition f
    if pos == (0,0)
    then return Nothing
    else do
        tile <- getTile
        return . Just $ setElem tile pos f

-- Displaying a field

prettyField :: Field -> (Tile -> String) -> String
prettyField f pf = concatMap prettifyrow rows
    where rows = map (V.toList . (flip getRow $ f)) [1..sidelength]
          prettifyrow r = concatMap pf r ++ "\n"

coloredTile :: Tile -> String
coloredTile t = setSGRCode colorcode ++ printfTile 5 t ++ setSGRCode normalcolor
    where color = fromMaybe (Vivid,White) (lookup t tileColors)
          colorcode = [colorToSGR color]
          normalcolor = [colorToSGR (Vivid,White)]

printfTile :: Int -> Tile -> String
printfTile width t = (replicate (width - length strn) ' ') ++ strn
    where strn = show t

tileColors :: [(Tile,(ColorIntensity,Color))]
tileColors = [(2,(Vivid,Black)),
           (4,(Dull,Red)),
           (8,(Dull,Green)),
           (16,(Dull,Yellow)),
           (32,(Dull,Blue)),
           (64,(Dull,Magenta)),
           (128,(Dull,Cyan)),
           (256,(Vivid,Red)),
           (512,(Vivid,Green)),
           (1024,(Vivid,Blue)),
           (2048,(Vivid,Magenta))
    ]

colorToSGR :: (ColorIntensity,Color) -> SGR
colorToSGR = uncurry $ SetColor Background
