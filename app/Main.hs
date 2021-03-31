module Main where

import XML.Parser
import Pug.Parser


import Data.ByteString (readFile)
import Data.ByteString.UTF8 (toString)
import Prelude hiding (readFile)

-- main = readFile "attr.html" >>= xmlParserTest
main = readFile "./test-files/test.pug" >>= pugPurserTest . toString









