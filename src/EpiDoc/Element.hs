{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Element (
    EpiDoc.Element.create,
    followingSibElems
) where

import EpiDoc.TypeClasses(HasCursor)
import Text.XML.Cursor(Cursor, followingSibling)
import EpiDoc.Lb(Lb, create, cursor)
import EpiDoc.Token(create)
import EpiDoc.TypeClasses ( create )
import EpiDoc.BaseElement(BaseElement, create)
import XmlUtils(localName)
import Data.Maybe(mapMaybe)

create :: (HasCursor e) => Cursor -> Maybe e
create c = case localName c of
    Just "lb" -> EpiDoc.Lb.create c
    Just "w" -> EpiDoc.Token.create c
    Just "num" -> EpiDoc.Token.create c
    Just "name" -> EpiDoc.Token.create c
    _ -> EpiDoc.BaseElement.create c


followingSibElems :: (HasCursor e) => e -> [e]
followingSibElems lb = do
    let sibs = followingSibling . cursor $ lb
    mapMaybe EpiDoc.Element.create sibs