{-# LANGUAGE FlexibleContexts #-}

module XML.Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Control.Monad

import Types

xmlParserTest :: String -> IO ()
xmlParserTest = parseTest tag

name :: Parser String
name = many1 $ letter <|> digit

content :: Parser XML
content = Content <$> (many1 $ digit <|> letter <|> space)

openT :: Parser (String, [Attr])
openT = do
    char '<'    
    nameT <- name
    spaces
    attrsL <- attrs
    char '>'
    return (nameT, attrsL)


closeT :: String -> Parser ()
closeT nameT = do
    string "</"
    string nameT
    spaces
    char '>'
    spaces
    return ()

tag :: Parser XML
tag = do
    (nameT, attrsL) <- try openT
    within <- many $ tag <|> content
    closeT nameT
    return $ Tag nameT attrsL within





value :: Parser String
value = many $ letter <|> digit <|> oneOf "/.:;" <|> space

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
    return $ Attr (nameA, valueA)


attrs :: Parser [Attr]
attrs = attr `sepBy` spaces