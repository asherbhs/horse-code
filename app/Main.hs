module Main where

import Morse (wordToMorse)
import Words (testWords)

import qualified System.Random as Random (randomRIO)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (unsafeIndex, length)
import qualified Data.List as List (intersperse)

getTest :: IO (String, String)
getTest = do
    r <- Random.randomRIO (0, Vector.length testWords - 1)
    let w = Vector.unsafeIndex testWords r
    return (w, wordToMorse w)

writePractice :: IO ()
writePractice = do
    (plain, morse) <- getTest
    putStrLn plain
    guess <- getLine
    if fixSpaces guess == morse
    then putStrLn "Correct!"
    else do
        putStrLn "Incorrect!"
        putStrLn $ "Correct was " ++ morse
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

main :: IO ()
main = do
    putStrLn "Practise reading or writing morse? (r/w): "
    readOrWrite <- getLine
    case readOrWrite of
        "r" -> readPractice
        "w" -> writePractice
        _   -> main
