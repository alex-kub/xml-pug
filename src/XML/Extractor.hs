module XML.Extractor where

import Types


extract :: [XML] -> String
extract frags = concat $ map extractOne frags

extractOne :: XML -> String
extractOne (Content st) = st
extractOne (Tag nameT attrL within) = openT nameT attrL ++ (concat $ map extractOne within)  ++ closeT nameT


openT :: String -> [Attr] -> String
openT name attrL = "<" ++ name ++ (attrs attrL) ++ ">"


closeT :: String -> String
closeT name = "</" ++ name ++ ">"


attr :: Attr -> String
attr (Attr (name, value)) | null name = error $ "Empty attribute"
                          | findedQ && findedQQ = error $ "Error quotation mark: " ++ value
                          | findedQ = " " ++ name ++ "=\"" ++ value ++ "\""
                          | otherwise = " " ++ name ++ "='" ++ value ++ "'"
    where
        findedQ = elem '\'' value
        findedQQ = elem '"' value


attrs :: [Attr] -> String
attrs attrL = concat $ map attr attrL

