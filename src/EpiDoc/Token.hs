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

-- data Token = Token {tokenCursor :: Cursor}

data Token = 
      Word Cursor
    | Name Cursor


-- instance HasCursor Token where
--     cursor


instance HasCursor Token where
    cursor :: Token -> Cursor
    cursor (Word a) = a
    cursor (EpiDoc.Token.Name a) = a



class HasTokens a where
    tokens :: a -> [Token]


instance HasTextContent Token where
    textContent :: Token -> String
    textContent = T.unpack . descContent . tokenCursor


instance Show Token where
    show :: Token -> String
    show t = "Token('" ++ textContent t ++ "')"


create :: Cursor -> Token
create a = case localName a of
    Just "w" -> Word a
    Just "name" -> EpiDoc.Token.Name a
    _ -> Word a


tokenCursor :: Token -> Cursor
tokenCursor = cursor

-- cursor :: Token -> Cursor
-- cursor = tokenCursor 


