module Morse (alphaToMorse, morseToAlpha) where

import Data.Map (Map)
import qualified Data.Map as Map

alphaMorseList :: [(Char, String)]
alphaMorseList =
    [ ('a', ".-"  ), ('b', "-...")
    , ('c', "-.-."), ('d', "-.." )
    , ('e', "."   ), ('f', "..-.")
    , ('g', "--." ), ('h', "....")
    , ('i', ".."  ), ('j', "---.")
    , ('k', "-.-" ), ('l', ".-..")
    , ('m', "--"  ), ('n', "-."  )
    , ('o', "---" ), ('p', ".--.")
    , ('q', "--.-"), ('r', "-.-" )
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

alphaToMorse :: Map Char String
alphaToMorse = Map.fromList alphaMorseList

morseToAlpha :: Map String Char
morseToAlpha = Map.fromList $ map (\(x, y) -> (y, x)) alphaMorseList