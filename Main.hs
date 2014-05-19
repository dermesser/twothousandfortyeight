module Main where

import Data.Maybe (fromMaybe)

import Twothousandfortyeight
import System.Console.ANSI

import System.Exit

main = do
    let f = makeField
    play f

play :: Field -> IO ()
play oldfld = do
    case oldfld of
        _fld | gameIsOver oldfld -> gameOver
             | hasWon oldfld -> gameWon
             | otherwise -> do
                newfld <- placeRandomTile oldfld
                nextAction (fromMaybe oldfld newfld)

nextAction :: Field -> IO ()
nextAction fld = do
            clearScreen
            putStrLn ""
            print fld
            putStrLn ""
            c <- getChar
            case c of
                'a' -> play (shiftField ToLeft fld)
                'w' -> play (shiftField ToUp fld)
                's' -> play (shiftField ToDown fld)
                'd' -> play (shiftField ToRight fld)
                'q' -> exitSuccess
                _ -> play fld


gameWon :: IO ()
gameWon = do
    clearScreen
    putStrLn ""
    putStrLn "######################"
    putStrLn "-- Congratulations! --"
    putStrLn "--  You have won.   --"
    putStrLn "######################"

gameOver :: IO ()
gameOver = do
    clearScreen
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

