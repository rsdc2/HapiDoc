{-# LANGUAGE OverloadedStrings #-}

module EpiDoc.TypeClasses
    ( HasTextContent
    , textContent
    , HasNumber
    , numberStr
    , numberInt
    , HasCursor
    , cursor
    ) where

import Text.XML.Cursor


class HasTextContent a where
    textContent :: a -> String


class HasNumber a where
    numberStr :: a -> Maybe String
    numberInt :: a -> Maybe Int


class HasCursor a where
    cursor :: a -> Cursor
