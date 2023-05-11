{-# LANGUAGE OverloadedStrings #-}

module EpiDoc.TypeClasses
    ( HasTextContent
    , textContent
    , descTextContent
    , HasNumber
    , numberStr
    , numberInt
    , HasCursor
    , cursor
    , create
    , EditionElem(..)
    -- , XMLable(..)
    ) where

import Text.XML.Cursor
import Text.XML(Element, Node(..))
import qualified Data.Text as T


class HasTextContent a where
    descTextContent :: a -> T.Text
    textContent :: a -> T.Text


class HasNumber a where
    numberStr :: a -> Maybe String
    numberInt :: a -> Maybe Int


class HasCursor a where
    cursor :: a -> Cursor
    create :: Cursor -> Maybe a


-- class XMLable a where
--     toElems :: a -> [Element]
--     toNodes :: a -> [Node]


class EditionElem a where
    editionText :: a -> T.Text