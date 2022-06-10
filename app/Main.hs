module Main where

import Morse (alphaToMorse)

import System.Random (randomRIO) 

getTest :: IO (Char, String)
getTest = do
    r <- randomRIO (0, 25)
    let alpha = ['a'..'z'] !! r
    return (alpha, alphaToMorse alpha)

writePractice :: IO ()
writePractice = do
    (alpha, morse) <- getTest
    putStrLn $ [alpha] ++ ": "
    guess <- getLine
    putStrLn $ 
        if guess == morse 
        then "Correct!" 
        else "Wrong! The correct answer was " ++ morse
    putStrLn ""
    writePractice

readPractice :: IO ()
readPractice = do
    (alpha, morse) <- getTest
    putStrLn $ morse ++ ": "
    guess <- getLine
    case guess of
        [c] -> putStrLn $ 
            if c == alpha 
            then "Correct!" 
            else "Wrong! The correct answer was " ++ ['\'', alpha, '\'']
        _   -> return ()
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
