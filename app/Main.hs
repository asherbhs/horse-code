module Main where

import qualified Morse
import qualified Words

-- import Data.Function ((&))
-- import Control.Arrow ((>>>))

import qualified System.Random as Random
-- import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Char as Char

getTest :: IO (String, String)
getTest = do
    testType <- Random.randomRIO (0 :: Int, 16 :: Int)
    index1 <- Random.randomRIO (0, Vector.length Words.testWords - 1)
    let word1 = Vector.unsafeIndex Words.testWords index1
    string <- case testType of
        -- no punctuation case
        0 -> return word1
        
        -- single word with punctuation
        1 -> return $ word1 ++ "."
        2 -> return $ word1 ++ "?"
        3 -> return $ word1 ++ "!"
        4 -> return $ "$" ++ word1
        5 -> return $ "@" ++ word1

        -- enclosing punctuation
        6 -> return $ "("  ++ word1 ++ ")"
        7 -> return $ "\"" ++ word1 ++ "\""
        8 -> return $ "'"  ++ word1 ++ "'"

        -- two words
        _ -> do
            index2 <- Random.randomRIO (0, Vector.length Words.testWords - 1)
            let word2 = Vector.unsafeIndex Words.testWords index2
            let infixStr = case testType of
                    9  -> "/"
                    10 -> " & "
                    11 -> ": "
                    12 -> "; "
                    13 -> " = "
                    14 -> " + "
                    15 -> " - "
                    16 -> "_"
                    __ -> ""
            return $ word1 ++ infixStr ++ word2
    return (string, Morse.wordToMorse string)

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
    if map Char.toLower guess == plain
    then putStrLn "Correct!"
    else do
        putStrLn "Incorrect!"
        putStrLn $ "Correct was:\t" ++ plain
    putStrLn ""
    readPractice

translateMorse :: IO ()
translateMorse = do
    plain <- getLine
    putStrLn $ Morse.wordToMorse plain
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
