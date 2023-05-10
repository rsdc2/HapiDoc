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
    ) where

import Text.XML.Cursor
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


class EditionElem a where
    editionText :: a -> T.Text