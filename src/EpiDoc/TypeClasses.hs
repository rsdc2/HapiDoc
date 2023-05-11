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
    , XMLable(..)
    , HasXMLName(..)
    ) where

import Text.XML.Cursor
import Text.XML
    ( Element
    , Node(..)
    , Name(..))
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


class XMLable a where
    toNodes :: a -> [Node]


class HasXMLName a where
    tagName :: a -> Name


class EditionElem a where
    editionText :: a -> T.Text