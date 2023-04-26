{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Token
    ( Token
    , cursor
    , tokens
    , HasTokens
    , textContent
    , create
    , show
    ) where

-- import Text.XML
import Text.XML
import Text.XML.Cursor
import XmlUtils
import EpiDoc.EpiDoc
import EpiDoc.TypeClasses
import qualified Data.Text as T

data Token = Token {tokenCursor :: Cursor}


class HasTokens a where
    tokens :: a -> [Token]


instance HasTextContent Token where
    textContent :: Token -> String
    textContent = T.unpack . descContent . tokenCursor


instance Show Token where
    show :: Token -> String
    show t = "Token('" ++ textContent t ++ "')"


create :: Cursor -> Token
create = Token


cursor :: Token -> Cursor
cursor = tokenCursor 
