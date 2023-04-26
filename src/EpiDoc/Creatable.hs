{-# LANGUAGE OverloadedStrings #-}

module EpiDoc.Creatable
    ( Creatable
    , create'''
    ) where

import EpiDoc.Edition
import EpiDoc.Token
import Text.XML.Cursor

data EpiDocNode = Edition | Token

class Creatable c where
    create''' :: c -> Maybe EpiDocNode
