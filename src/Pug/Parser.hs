{-# LANGUAGE FlexibleContexts #-}

module Pug.Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Prelude hiding (appendFile)

import Control.Monad
import Data.Either
import Data.ByteString.UTF8 (fromString)
import Data.ByteString (appendFile)

import Types
import XML.Parser (name, value)
import XML.Extractor (extract)



pugPurserTest :: String -> IO ()
pugPurserTest st = do
    st <- return $ (cutEnd st) ++ "\n"
    print st
    -- case (parse test "" $ cutEnd st ) of
    --     Left mess -> print mess
    --     Right ast -> do            
    --         appendFile "./test-files/test.pug.extract.html" (fromString $ extract ast)    
    parseTest tags st
    parseTest test st

combine :: [(Int, XML)] -> [XML]
combine [(i, t)] = [t]
combine ((i,t):ts) 
                    | null next = [inject t (combine sub)]
                    | null sub = [t] ++ combine ts
                    | otherwise = [inject t (combine sub)] ++ (combine next)
    where
        spanT = span (\(j,_) -> j > i ) ts
        sub = fst spanT
        next = snd spanT

inject :: XML -> [XML] -> XML
inject (Tag nameT attrL within) subs = Tag nameT attrL (within ++ subs)


test :: Parser [XML]
test = tags >>= return . combine

lenTabs :: String -> Parser Int
lenTabs tb = return $ length $ fromRight "" $ parse (many tab) "" (reverse tb)

cutEnd :: String -> String
cutEnd doc = reverse $ fromRight "" $  parse ((many $ endOfLine <|> tab) *> (many anyChar)) "" (reverse doc)

tags :: Parser [(Int, XML)]
tags = many tag


attr :: Parser Attr
attr = do
    spaces
    nameA <- name
    spaces
    char '='
    spaces
    char '"'
    valueA <- value
    char '"'
    many space_
    return $ Attr (nameA, valueA)



space_ :: Parser Char
space_  = tab <|> char ' '


content :: Parser [XML]
content = do
    c <- (many space_) *> val <* (many1 endOfLine)
    case (length $ c) of
        0 -> return []
        _-> return $ [Content c]
    where
        val = many $ digit <|> letter <|> oneOf ":;,! \t"


tag :: Parser (Int, XML)
tag = do    
    splitter <- many $ endOfLine <|> tab <|> char ' '  
    level <- lenTabs splitter
    nameT <- name
    qCI <- qClassId
    attrL <- attrs
    contentV <- content    
    return $ (level, Tag nameT (qCI ++ attrL) contentV)



attrs :: Parser [Attr]
attrs = do
    many space_
    lBracket <- optionMaybe $ char '('
    case lBracket of
        Just _ -> do            
            sep <- lookAhead attrSep
            a <- (attr `sepBy` sep) <* char ')'
            return a
        Nothing -> return []


attrSep :: Parser (Parser Char)
attrSep = do
    attr    
    many space_
    sep <- optionMaybe $ char ','    
    case sep of
        Nothing -> return $ space_
        Just _ -> return $ (many space_) >> char ','


idAttrs :: Parser Attr
idAttrs = (char '#' *> name >>= \name_ -> return $ Attr ("id", name_)) <?> ""

classAttrs :: Parser Attr
classAttrs = (char '.' *> name >>= \name_ -> return $ Attr ("class", name_)) <?> ""

qClassId :: Parser [Attr]
qClassId = do
    qcl <- many $ try  classAttrs <|> idAttrs
    checkId qcl
    return qcl

checkId :: [Attr] -> Parser ()
checkId attrL | findIds attrL > 1 = do
    unexpected "second id" <?> "must be once id"
checkId _ = return ()

findIds :: [Attr] -> Int
findIds attrL = length $ filter isId attrL

isId :: Attr -> Bool
isId (Attr ("id", _)) = True
isId _ = False


