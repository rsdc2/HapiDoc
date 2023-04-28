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
import Text.XML ()
import Text.XML.Cursor ( Cursor )
import XmlUtils ( descContent, localName )
import EpiDoc.EpiDoc ()
import EpiDoc.TypeClasses ( HasCursor(..), HasTextContent(..) )
import qualified Data.Text as T

-- data Token = Token {tokenCursor :: Cursor}

data Token = 
      Word Cursor
    | Name Cursor
    | Num Cursor


instance HasCursor Token where
    cursor :: Token -> Cursor
    cursor (Word c) = c
    cursor (Name c) = c
    cursor (Num c) = c

    create :: Cursor -> Maybe Token
    create c = case localName c of
        Just "w" -> Just (Word c)
        Just "num" -> Just (Num c)
        Just "name" -> Just (EpiDoc.Token.Name c)
        _ -> Nothing

class HasTokens a where
    tokens :: a -> [Token]


instance HasTextContent Token where
    textContent :: Token -> String
    textContent = T.unpack . descContent . cursor


instance Show Token where
    show :: Token -> String
    show (Word c) = "Word('" ++ textContent (Word c) ++ "')"
    show (Num c) = "Num('" ++ textContent (Num c) ++ "')"
    show (EpiDoc.Token.Name c) = "Name('" ++ textContent (EpiDoc.Token.Name c) ++ "')"



tokenCursor :: Token -> Cursor
tokenCursor = cursor

-- cursor :: Token -> Cursor
-- cursor = tokenCursor 


