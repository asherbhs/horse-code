module Morse (charToMorse, morseToChar, wordToMorse) where

-- import Data.Function ((&))
import Control.Arrow ((>>>))

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Tuple as Tuple

charToMorseList :: [(Char, String)]
charToMorseList =
    [ ('a', ".-"  ), ('b', "-...")
    , ('c', "-.-."), ('d', "-.." )
    , ('e', "."   ), ('f', "..-.")
    , ('g', "--." ), ('h', "....")
    , ('i', ".."  ), ('j', ".---")
    , ('k', "-.-" ), ('l', ".-..")
    , ('m', "--"  ), ('n', "-."  )
    , ('o', "---" ), ('p', ".--.")
    , ('q', "--.-"), ('r', ".-." )
    , ('s', "..." ), ('t', "-"   )
    , ('u', "..-" ), ('v', "...-")
    , ('w', ".--" ), ('x', "-..-")
    , ('y', "-.--"), ('z', "--..")

    , ('0', "-----"), ('1', ".----")
    , ('2', "..---"), ('3', "...--")
    , ('4', "....-"), ('5', ".....")
    , ('6', "-...."), ('7', "--...")
    , ('8', "---.."), ('9', "----.")
    ]

charToMorseMap :: Map Char String
charToMorseMap = Map.fromList charToMorseList

morseToCharMap :: Map String Char
morseToCharMap = Map.fromList $ map Tuple.swap charToMorseList

charToMorse :: Char -> String
charToMorse = (Map.!) charToMorseMap . Char.toLower

morseToChar :: String -> Char
morseToChar = (Map.!) morseToCharMap

wordToMorse :: String -> String
wordToMorse
    =   filter (\it 
            -> Char.isAlpha it 
            || Char.isDigit it 
            || it == ' '
        )
    >>> map (\it -> 
            if it == ' ' 
            then " " 
            else charToMorse it
        )
    >>> List.intersperse "   "
    >>> concat