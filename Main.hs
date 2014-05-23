module Main where

import Data.Maybe (fromMaybe)

import Twothousandfortyeight
import IO

import System.Exit

main = do
    let f = makeField
    play f

play :: Field -> IO ()
play oldfld =
    case oldfld of
        _fld | gameIsOver oldfld -> printField oldfld >> gameOver
             | hasWon oldfld -> printField oldfld >> gameWon
             | otherwise -> do
                newfld <- placeRandomTile oldfld
                nextAction (fromMaybe oldfld newfld)

nextAction :: Field -> IO ()
nextAction fld = do
            printField fld
            c <- getChar
            case c of
                'a' -> play (shiftField ToLeft fld)
                'w' -> play (shiftField ToUp fld)
                's' -> play (shiftField ToDown fld)
                'd' -> play (shiftField ToRight fld)
                'h' -> play (shiftField (bestAction fld) fld)
                'q' -> exitSuccess
                _ -> play fld

printField :: Field -> IO ()
printField fld = putStrLn "" >> putStr (prettyField fld coloredTile) >> putStrLn ""

gameWon :: IO ()
gameWon = do
    putStrLn ""
    putStrLn "######################"
    putStrLn "-- Congratulations! --"
    putStrLn "--  You have won.   --"
    putStrLn "######################"

gameOver :: IO ()
gameOver = do
    putStrLn ""
    putStrLn "###############"
    putStrLn "-- GAME OVER --"
    putStrLn "###############"

-- Testing functions

hasWon :: Field -> Bool
hasWon = not . null . getTilesWith 2048

gameIsOver :: Field -> Bool
gameIsOver f = (null $ getTilesWith 0 f)
            && f == shiftField ToUp f
            && f == shiftField ToDown f
            && f == shiftField ToRight f
            && f == shiftField ToLeft f

