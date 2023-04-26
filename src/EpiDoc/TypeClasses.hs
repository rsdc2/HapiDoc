{-# LANGUAGE OverloadedStrings #-}

module EpiDoc.TypeClasses
    ( HasTextContent
    , textContent
    ) where

-- import Text.XML
import Text.XML
import Text.XML.Cursor
import XmlUtils
import EpiDoc.EpiDoc



import qualified Data.Text as T


class HasTextContent a where
    textContent :: a -> String


