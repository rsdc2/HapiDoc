{-# LANGUAGE OverloadedStrings #-}

module EpiDoc.TypeClasses
    ( WContentable
    , wContent
    ) where

-- import Text.XML
import Text.XML
import Text.XML.Cursor
import XmlUtils
import EpiDoc.EpiDoc



import qualified Data.Text as T


class WContentable a where
    wContent :: a -> T.Text


