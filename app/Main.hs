module Main where

import qualified Morse (wordToMorse, alphaToMorse)
import qualified Words (testWords)

import Data.Function ((&))
-- import Control.Arrow ((>>>))

import qualified System.Random as Random (randomRIO)
-- import Data.Vector (Vector)
import qualified Data.Vector as Vector (unsafeIndex, length)
import qualified Data.List as List (intersperse)
import qualified Data.Char as Char (isAlpha)

getTest :: IO (String, String)
getTest = do
    r <- Random.randomRIO (0, Vector.length Words.testWords - 1)
    let w = Vector.unsafeIndex Words.testWords r
    return (w, Morse.wordToMorse w)

writePractice :: IO ()
writePractice = do
    (plain, morse) <- getTest
    putStrLn plain
    guess <- getLine
    if fixSpaces guess == morse
    then putStrLn "Correct!"
    else do
        putStrLn "Incorrect! Correct was:"
        putStrLn morse
    putStrLn ""
    writePractice
  where 
    -- corrects the spacing between letters to the morse code standard of 3
    fixSpaces = concat . List.intersperse "   " . words

readPractice :: IO ()
readPractice = do
    (plain, morse) <- getTest
    putStrLn morse 
    guess <- getLine
    if guess == plain
    then putStrLn "Correct!"
    else do
        putStrLn "Incorrect!"
        putStrLn $ "Correct was " ++ plain
    putStrLn ""
    readPractice

translateMorse :: IO ()
translateMorse = do
    plain <- getLine
    putStrLn $ plain
        & filter Char.isAlpha
        & map Morse.alphaToMorse
        & List.intersperse "   "
        & concat
    translateMorse

main :: IO ()
main = do
    putStrLn 
        "Practice (r)eading morse, (w)riting morse, or (t)ranslate to morse (r/w/t): "
    readOrWrite <- getLine
    case readOrWrite of
        "r" -> readPractice
        "w" -> writePractice
        "t" -> translateMorse
        ___ -> main
