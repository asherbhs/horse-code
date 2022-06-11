module Morse (alphaToMorse, morseToAlpha, wordToMorse) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List (intersperse)

alphaToMorseList :: [(Char, String)]
alphaToMorseList =
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

alphaToMorseMap :: Map Char String
alphaToMorseMap = Map.fromList alphaToMorseList

morseToAlphaMap :: Map String Char
morseToAlphaMap = Map.fromList $ map (\(x, y) -> (y, x)) alphaToMorseList

alphaToMorse :: Char -> String
alphaToMorse = (Map.!) alphaToMorseMap

morseToAlpha :: String -> Char
morseToAlpha = (Map.!) morseToAlphaMap

wordToMorse :: String -> String
wordToMorse = concat . List.intersperse "   " . map alphaToMorse