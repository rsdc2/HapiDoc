{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Element
    ( EpiDoc.Element.create
    ) where

-- import Text.XML
import Text.XML.Cursor (Cursor)
import XmlUtils (localName)
import EpiDoc.EpiDoc ()
import EpiDoc.TypeClasses (HasCursor, cursor)
import EpiDoc.Token (Token, create, HasTokens, tokens)
import EpiDoc.Lb (create, Lb)


data BaseElement = BaseElement Cursor


instance HasCursor BaseElement where
    cursor :: BaseElement -> Cursor
    cursor (BaseElement c) = c

    create :: Cursor -> Maybe BaseElement
    create c = Just (BaseElement c)
 

create :: (HasCursor e) => Cursor -> Maybe e
create c = case localName c of
    Just "lb" -> EpiDoc.Lb.create c
    Just "w" -> EpiDoc.Token.create c
    Just "num" -> EpiDoc.Token.create c
    Just "name" -> EpiDoc.Token.create c
    _ -> EpiDoc.Element.create c

getTokens :: (HasTokens a) => a -> [Token]
getTokens = tokens

