{-# LANGUAGE OverloadedStrings #-}

module EpiDoc.TypeClasses
    ( HasTextContent
    , textContent
    , HasNumber
    , numberStr
    , numberInt
    , HasCursor
    , cursor
    , create
    ) where

import Text.XML.Cursor
import qualified Data.Text as T


class HasTextContent a where
    textContent :: a -> T.Text


class HasNumber a where
    numberStr :: a -> Maybe String
    numberInt :: a -> Maybe Int


class HasCursor a where
    cursor :: a -> Cursor
    create :: Cursor -> Maybe a
