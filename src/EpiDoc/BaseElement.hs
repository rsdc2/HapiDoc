{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.BaseElement
    ( create, cursor, BaseElement
    ) where

-- import Text.XML
import Text.XML.Cursor (Cursor)
import XmlUtils (localName)
import EpiDoc.TypeClasses (HasCursor, cursor)
import EpiDoc.Token (Token, create, HasTokens, tokens)
import EpiDoc.Lb (create, Lb)


data BaseElement = BaseElement Cursor


instance HasCursor BaseElement where
    cursor :: BaseElement -> Cursor
    cursor (BaseElement c) = c

    create :: Cursor -> Maybe BaseElement
    create c = Just (BaseElement c)
 



getTokens :: (HasTokens a) => a -> [Token]
getTokens = tokens

