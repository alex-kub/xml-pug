module Types where


newtype Attr = Attr (String, String) deriving Show

data XML = Void | Content String | Tag String [Attr] [XML] deriving Show