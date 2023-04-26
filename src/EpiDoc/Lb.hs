{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Lb
    (Lb
    , createLb
    , tokens
    , show
    , lbCursor
    ) where

-- import Text.XML
import Text.XML
import Text.XML.Cursor
import XmlUtils
import EpiDoc.EpiDoc
import EpiDoc.TypeClasses
import EpiDoc.Token
import qualified Data.Text as T

data Lb = Lb {lbCursor :: Cursor}


instance EpiDoc.Token.HasTokens Lb where
    tokens :: Lb -> [Token]
    tokens lb = do 
        let wordFilter e = localName e == Just "w"
        let sibs = followingSibling . lbCursor $ lb
        let filtered = filter wordFilter sibs
        [EpiDoc.Token.create e | e <- filtered]
        
instance Show Lb where
    show :: Lb -> String
    show lb = "lb"

-- instance WContentable Lb where
--     wContent :: Lb -> T.Text
--     wContent lb = descContent . map ws . followingSibling . lbCursor $ lb


cursor :: Lb -> Cursor
cursor = lbCursor 

createLb :: Cursor -> Lb
createLb = Lb